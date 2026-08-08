/*
 * Copyright (c) 2026, Huawei Technologies Co., Ltd. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifdef AARCH64
#include "classfile/bytecodeEnhancement.hpp"
#include "jvm_io.h"
#include "utilities/exceptions.hpp"
#include "utilities/formatBuffer.hpp"
#include "classfile/classFileStream.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/classLoaderData.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/vmClassMacros.hpp"
#include "classfile/vmClasses.hpp"
#include "classfile/vmSymbols.hpp"
#include "logging/log.hpp"
#include "memory/resourceArea.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/symbol.hpp"
#include "runtime/atomic.hpp"
#include "runtime/globals.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "utilities/ostream.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/resourceHash.hpp"

#include <ctype.h>
#include <stdlib.h>
#include <sys/stat.h>

// ------------------------ [inner classes & variables] ------------------------

enum class BuiltinLoader : int {
  BootLoader,
  PlatformLoader,
  AppLoader,
  NotBuiltinLoader,
  Unknown
};

enum class EnhancementKind : int {
  ClassReplacement,
  ClassAdd,
  Unknown
};

static constexpr const char* bytecode_enhancement_list_name = "bytecode-enhancement.list";
static constexpr const char* internal_classdata_prefix = "jdk/internal/enhancement/classdata/";
static constexpr const char* internal_classdata_suffix = ".classdata";

static const char* internal_classdata_resource_name(Symbol* name) {
  const char* class_name = name->as_C_string();
  size_t length = strlen(internal_classdata_prefix) + strlen(class_name) +
                  strlen(internal_classdata_suffix) + 1;
  char* resource_name = NEW_RESOURCE_ARRAY(char, length);
  jio_snprintf(resource_name, length, "%s%s%s",
               internal_classdata_prefix, class_name, internal_classdata_suffix);
  return resource_name;
}

// ClassEnhancementAction transforms the stream of an existing class.
class ClassEnhancementAction: public CHeapObj<mtClass> {
 public:
  virtual ClassFileStream* enhance_class_stream(ClassFileStream* stream, ClassLoaderData* loader_data, TRAPS) = 0;
  virtual void print_debug() const = 0;
};

// Replaces an existing class after the defining loader has found the original
// bytes. The original stream CRC32 must match before replacement is applied.
class ClassReplacementAction: public ClassEnhancementAction {
 private:
  Symbol* _name;
  uint32_t _crc32;
  ClassPathEntry* _path_entry;

 public:
  ClassReplacementAction(Symbol* name, uint32_t crc32, ClassPathEntry* path_entry):
      _name(name), _crc32(crc32), _path_entry(path_entry) {}

  bool can_apply(ClassFileStream* stream) const;
  ClassFileStream* enhance_class_stream(ClassFileStream* stream, ClassLoaderData* loader_data, TRAPS) override;
  void print_debug() const override;

 protected:
  Symbol* name() const { return _name; }
  uint32_t crc32() const { return _crc32; }
  virtual ClassFileStream* load_class_stream(TRAPS);
};

// Replaces a JDK class with bytecode stored as a private runtime-image
// resource. The resource already has the target class's internal name.
class ClassReplacementActionInternal: public ClassReplacementAction {
 public:
  ClassReplacementActionInternal(Symbol* name, uint32_t crc32):
      ClassReplacementAction(name, crc32, nullptr) {}

  void print_debug() const override;

 protected:
  ClassFileStream* load_class_stream(TRAPS) override;
};

// ClassAddAction loads a class that was not found by the normal lookup path.
class ClassAddAction: public CHeapObj<mtClass> {
 private:
  Symbol* _name;
  Symbol* _owner_class;
  BuiltinLoader _owner_loader;
  ClassPathEntry* _path_entry;

 public:
  ClassAddAction(Symbol* name, Symbol* owner_class, BuiltinLoader owner_loader, ClassPathEntry* path_entry):
      _name(name), _owner_class(owner_class), _owner_loader(owner_loader), _path_entry(path_entry) {}

  bool can_apply(BuiltinLoader loader) const;
  ClassFileStream* open_class_stream(TRAPS);
  virtual void print_debug() const;

 protected:
  Symbol* name() const { return _name; }
  BuiltinLoader owner_loader() const { return _owner_loader; }
  virtual ClassFileStream* load_class_stream(TRAPS);
};

// Provides one new built-in-loader class from the private runtime-image classdata.
class ClassAddActionInternal: public ClassAddAction {
 public:
  ClassAddActionInternal(Symbol* name, BuiltinLoader loader): ClassAddAction(name, nullptr, loader, nullptr) {}

  void print_debug() const override;

 protected:
  ClassFileStream* load_class_stream(TRAPS) override;
};

class OwnerClassLoaderBinding {
 private:
  volatile int _loader;

 public:
  OwnerClassLoaderBinding(): _loader((int) BuiltinLoader::Unknown) {}

  BuiltinLoader loader() const {
    return (BuiltinLoader) Atomic::load(&_loader);
  }

  void set_loader(BuiltinLoader loader, Symbol* owner);
};

static unsigned symbol_hash(Symbol* const& s) {
  return s->identity_hash();
}

static bool symbol_equals(Symbol* const& s0, Symbol* const& s1) {
  return s0 == s1;
}

using BytecodeEnhancementTable = ResourceHashtable<Symbol*, ClassEnhancementAction*,
                                               107, AnyObj::C_HEAP, mtClass,
                                               symbol_hash, symbol_equals>;
using BytecodeEnhancementAddTable = ResourceHashtable<Symbol*, ClassAddAction*,
                                                  107, AnyObj::C_HEAP, mtClass,
                                                  symbol_hash, symbol_equals>;
using BytecodeEnhancementOwnerTable = ResourceHashtable<Symbol*, OwnerClassLoaderBinding,
                                                    107, AnyObj::C_HEAP, mtClass,
                                                    symbol_hash, symbol_equals>;

static BytecodeEnhancementTable* _bytecode_enhancement_actions = nullptr;
static BytecodeEnhancementAddTable* _class_add_actions = nullptr;
static BytecodeEnhancementOwnerTable* _owner_class_loaders = nullptr;

bool BytecodeEnhancement::_bytecode_enhancement_enabled = false;

// ------------------------------ [common utils] ------------------------------

static char* make_resource_copy(const char* s) {
  size_t len = strlen(s) + 1;
  char* copy = NEW_RESOURCE_ARRAY(char, len);
  memcpy(copy, s, len);
  return copy;
}

static const char* builtin_loader_name(BuiltinLoader loader) {
  switch (loader) {
    case BuiltinLoader::BootLoader:
      return "boot";
    case BuiltinLoader::PlatformLoader:
      return "platform";
    case BuiltinLoader::AppLoader:
      return "app";
    case BuiltinLoader::NotBuiltinLoader:
      return "not-builtin";
    case BuiltinLoader::Unknown:
      return "unknown";
    default:
      ShouldNotReachHere();
      return "invalid";
  }
}

static BuiltinLoader builtin_loader(ClassLoaderData* loader_data) {
  if (loader_data == nullptr) {
    return BuiltinLoader::NotBuiltinLoader;
  }
  if (loader_data->is_the_null_class_loader_data()) {
    return BuiltinLoader::BootLoader;
  }
  if (loader_data->is_platform_class_loader_data()) {
    return BuiltinLoader::PlatformLoader;
  }
  if (loader_data->is_system_class_loader_data()) {
    return BuiltinLoader::AppLoader;
  }
  return BuiltinLoader::NotBuiltinLoader;
}

static char* class_file_name_from_internal(const char* internal_name) {
  size_t len = strlen(internal_name) + 7;
  char* name = NEW_RESOURCE_ARRAY(char, len);
  jio_snprintf(name, len, "%s.class", internal_name);
  return name;
}

static ClassFileStream* open_entry_class_stream(ClassPathEntry* path_entry, Symbol* name, TRAPS) {
  assert(path_entry != nullptr, "sanity");
  assert(name != nullptr, "sanity");
  const char* internal_name = name->as_C_string();
  char* file_name = class_file_name_from_internal(internal_name);
  return path_entry->open_stream(THREAD, file_name);
}

static void record_owner_class_loader(Symbol* owner, ClassLoaderData* loader_data) {
  if (owner == nullptr || _owner_class_loaders == nullptr) {
    return;
  }
  OwnerClassLoaderBinding* binding = _owner_class_loaders->get(owner);
  if (binding == nullptr) {
    return;
  }
  BuiltinLoader loader = builtin_loader(loader_data);
  if (loader == BuiltinLoader::NotBuiltinLoader) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg(
        "Bytecode enhancement class-add for owner class %s is not supported with a custom class loader",
        owner->as_C_string()));
  }
  binding->set_loader(loader, owner);
}

// -------------------------- [file parsing helpers] --------------------------

static char* trim(char* s) {
  while (*s != '\0' && isspace((unsigned char)*s)) {
    s++;
  }
  char* end = s + strlen(s);
  while (end > s && isspace((unsigned char)end[-1])) {
    *--end = '\0';
  }
  return s;
}

static char* next_token(char*& p) {
  while (*p != '\0' && isspace((unsigned char)*p)) {
    p++;
  }
  if (*p == '\0' || *p == '#') {
    return nullptr;
  }
  char* token = p;
  while (*p != '\0' && !isspace((unsigned char)*p)) {
    p++;
  }
  if (*p != '\0') {
    *p++ = '\0';
  }
  return token;
}

static char* normalized_name(const char* name, bool dotted) {
  size_t len = strlen(name);
  if (len > 6 && strcmp(name + len - 6, ".class") == 0) {
    len -= 6;
  }
  char* result = NEW_RESOURCE_ARRAY(char, len + 1);
  for (size_t i = 0; i < len; i++) {
    char c = name[i];
    if (dotted && c == '/') {
      c = '.';
    } else if (!dotted && c == '.') {
      c = '/';
    }
    result[i] = c;
  }
  result[len] = '\0';
  return result;
}

static EnhancementKind parse_kind(const char* token) {
  if (strcmp(token, "class-rep") == 0) {
    return EnhancementKind::ClassReplacement;
  }
  if (strcmp(token, "class-add") == 0) {
    return EnhancementKind::ClassAdd;
  }
  return EnhancementKind::Unknown;
}

static bool parse_crc32(const char* token, uint32_t* crc) {
  char* end = nullptr;
  unsigned long value = strtoul(token, &end, 0);
  if (end == token || *end != '\0') {
    return false;
  }
  *crc = (uint32_t) value;
  return true;
}

static bool parse_owner_loader(const char* token, BuiltinLoader* loader) {
  const char* prefix = "owner-loader=";
  size_t prefix_len = strlen(prefix);
  if (strncmp(token, prefix, prefix_len) != 0) {
    return false;
  }
  const char* value = token + prefix_len;
  if (strcmp(value, "boot") == 0) {
    *loader = BuiltinLoader::BootLoader;
    return true;
  }
  if (strcmp(value, "platform") == 0) {
    *loader = BuiltinLoader::PlatformLoader;
    return true;
  }
  if (strcmp(value, "app") == 0) {
    *loader = BuiltinLoader::AppLoader;
    return true;
  }
  return false;
}

static bool parse_owner_class(const char* token, char** owner_class) {
  const char* prefix = "owner-class=";
  size_t prefix_len = strlen(prefix);
  if (strncmp(token, prefix, prefix_len) != 0) {
    return false;
  }
  *owner_class = normalized_name(token + prefix_len, false);
  return true;
}

static void validate_unique_action(Symbol* name) {
  if (BytecodeEnhancement::should_enhance_class(name) || BytecodeEnhancement::should_add_class(name)) {
    vm_exit_during_initialization(err_msg(
        "Duplicate BytecodeEnhancement action for class %s", name->as_C_string()));
  }
}

static void append_class_replacement_entry(ClassPathEntry* path_entry, const char* name, uint32_t crc) {
  char* internal_name = normalized_name(name, false);
  Symbol* symbol = SymbolTable::new_symbol(internal_name);
  validate_unique_action(symbol);
  ClassEnhancementAction* action = new ClassReplacementAction(symbol, crc, path_entry);
  _bytecode_enhancement_actions->put(symbol, action);
}

static void append_class_add_entry(ClassPathEntry* path_entry, const char* name,
                                   const char* owner_token, int line_no, const char* line) {
  BuiltinLoader owner_loader = BuiltinLoader::Unknown;
  Symbol* owner_symbol = nullptr;
  char* owner_class = nullptr;
  if (parse_owner_loader(owner_token, &owner_loader)) {
    // Direct owner-loader binding.
  } else if (parse_owner_class(owner_token, &owner_class)) {
    owner_symbol = SymbolTable::new_symbol(owner_class);
    bool created;
    _owner_class_loaders->put_if_absent(owner_symbol, &created);
  } else {
    log_warning(class, load, enhancement)("Ignoring malformed class-add owner at line %d: %s", line_no, line);
    return;
  }

  char* internal_name = normalized_name(name, false);
  Symbol* symbol = SymbolTable::new_symbol(internal_name);
  validate_unique_action(symbol);
  ClassAddAction* action = new ClassAddAction(symbol, owner_symbol, owner_loader, path_entry);
  _class_add_actions->put(symbol, action);
}

static void parse_bytecode_enhancement_list(ClassPathEntry* path_entry, char* content, size_t size) {
  ResourceMark rm;
  int line_no = 0;
  char* line = content;
  while (line < content + size) {
    line_no++;
    char* next = strchr(line, '\n');
    if (next != nullptr) {
      *next++ = '\0';
    } else {
      next = content + size;
    }

    char* t = trim(line);
    if (*t != '\0' && *t != '#') {
      // Keep a copy because next_token() mutates the line while tokenizing.
      char* log_line = make_resource_copy(t);
      char* cursor = t;
      char* kind_token = next_token(cursor);
      EnhancementKind kind = kind_token == nullptr ? EnhancementKind::Unknown : parse_kind(kind_token);
      if (kind == EnhancementKind::ClassReplacement) {
        char* crc_token = next_token(cursor);
        char* class_name = next_token(cursor);
        uint32_t crc = 0;
        if (crc_token == nullptr || class_name == nullptr || !parse_crc32(crc_token, &crc)) {
          log_warning(class, load, enhancement)(
              "Ignoring malformed class-rep bytecode enhancement line %d: %s",
              line_no, log_line);
        } else {
          append_class_replacement_entry(path_entry, class_name, crc);
        }
      } else if (kind == EnhancementKind::ClassAdd) {
        char* class_name = next_token(cursor);
        char* owner_token = next_token(cursor);
        if (class_name == nullptr || owner_token == nullptr) {
          log_warning(class, load, enhancement)(
              "Ignoring malformed class-add bytecode enhancement line %d: %s",
              line_no, log_line);
        } else {
          append_class_add_entry(path_entry, class_name, owner_token, line_no, log_line);
        }
      } else {
        log_warning(class, load, enhancement)("Ignoring unknown bytecode enhancement line %d: %s", line_no, log_line);
      }
    }
    line = next;
  }
}

static ClassPathEntry* make_bytecode_enhancement_path_entry(const char* path) {
  const char* effective_path = path[0] == '\0' ? "." : path;
  struct stat st;
  if (os::stat(effective_path, &st) != 0) {
    log_warning(class, load, enhancement)("Ignoring non-existent bytecode enhancement path: %s", effective_path);
    return nullptr;
  }

  if ((st.st_mode & S_IFMT) == S_IFDIR) {
    return new ClassPathDirEntry(effective_path);
  }
  if ((st.st_mode & S_IFMT) == S_IFREG) {
    ClassPathZipEntry* entry = ClassLoader::create_class_path_zip_entry(effective_path, false);
    if (entry != nullptr) {
      return entry;
    }
  }
  log_warning(class, load, enhancement)("Ignoring invalid bytecode enhancement path: %s", effective_path);
  return nullptr;
}

static void make_bytecode_enhancement_path_entries(GrowableArray<ClassPathEntry*>* entries) {
  char* paths = make_resource_copy(BytecodeEnhancementPaths);
  const char separator = os::path_separator()[0];
  char* start = paths;
  while (true) {
    char* end = strchr(start, separator);
    if (end != nullptr) {
      *end = '\0';
    }
    ClassPathEntry* entry = make_bytecode_enhancement_path_entry(start);
    if (entry != nullptr) {
      entries->append(entry);
    }
    if (end == nullptr) {
      break;
    }
    start = end + 1;
  }
}

static bool parse_bytecode_enhancement_lists(GrowableArray<ClassPathEntry*>* path_entries) {
  bool found = false;
  JavaThread* current = JavaThread::current();
  ResourceMark rm(current);
  for (int i = 0; i < path_entries->length(); i++) {
    ClassPathEntry* path_entry = path_entries->at(i);
    ClassFileStream* stream = path_entry->open_stream(current, bytecode_enhancement_list_name);
    if (stream == nullptr) {
      continue;
    }
    found = true;
    int size = stream->length();
    char* content = NEW_RESOURCE_ARRAY(char, size + 1);
    memcpy(content, stream->buffer(), size);
    content[size] = '\0';
    parse_bytecode_enhancement_list(path_entry, content, size);
  }
  if (!found) {
    log_warning(class, load, enhancement)(
        "Cannot read bytecode enhancement list from BytecodeEnhancementPaths: %s",
        BytecodeEnhancementPaths);
  }
  return found;
}

// ------------------------- [action implementations] -------------------------

ClassFileStream* ClassReplacementAction::enhance_class_stream(
    ClassFileStream* stream, ClassLoaderData* loader_data, TRAPS) {
  // Do not put a ResourceMark here or the returned ClassFileStream will be freed.
  if (!can_apply(stream)) {
    return stream;
  }

  const char* class_name = _name->as_C_string();
  ClassFileStream* replacement = load_class_stream(CHECK_NULL);
  if (replacement == nullptr) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg("Bytecode enhancement class file not found for %s", class_name));
    return stream;
  }

  record_owner_class_loader(_name, loader_data);
  log_info(class, load, enhancement)(
      "Bytecode enhancement replacing class %s with %s",
      class_name, replacement->source());
  return new ClassFileStream(replacement->buffer(),
                             replacement->length(),
                             replacement->source(),
                             /* from_boot_loader_modules_image */ false,
                             /* from_class_file_load_hook */ true);
}

bool ClassReplacementAction::can_apply(ClassFileStream* stream) const {
  uint32_t actual_crc = (uint32_t) ClassLoader::crc32(0, (const char*)stream->buffer(), stream->length());
  if (actual_crc != _crc32) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg(
        "Bytecode enhancement skipped for %s: CRC32 mismatch, expected 0x%08x actual 0x%08x",
        _name->as_C_string(), _crc32, actual_crc));
    return false;
  }
  return true;
}

ClassFileStream* ClassReplacementAction::load_class_stream(TRAPS) {
  return open_entry_class_stream(_path_entry, _name, CHECK_NULL);
}

void ClassReplacementAction::print_debug() const {
  ResourceMark rm;
  log_debug(class, load, enhancement)("  class-replacement name=%s crc32=0x%08x path=%s",
                                _name->as_C_string(), _crc32, _path_entry->name());
}

ClassFileStream* ClassReplacementActionInternal::load_class_stream(TRAPS) {
  if (!ClassLoader::has_jrt_entry()) {
    return nullptr;
  }

  const char* resource_name = internal_classdata_resource_name(name());
  return ClassLoader::get_jrt_entry()->open_stream(THREAD, resource_name);
}

void ClassReplacementActionInternal::print_debug() const {
  ResourceMark rm;
  const char* resource_name = internal_classdata_resource_name(name());
  log_debug(class, load, enhancement)("  class-replacement name=%s crc32=0x%08x resource=%s",
                                name()->as_C_string(), crc32(), resource_name);
}

bool ClassAddAction::can_apply(BuiltinLoader loader) const {
  if (_owner_class != nullptr) {
    if (_owner_class_loaders == nullptr) {
      BytecodeEnhancement::handle_failure("Bytecode enhancement owner class loader table is unavailable");
      return false;
    }
    OwnerClassLoaderBinding* binding = _owner_class_loaders->get(_owner_class);
    if (binding == nullptr) {
      ResourceMark rm;
      BytecodeEnhancement::handle_failure(err_msg(
          "Bytecode enhancement owner class loader binding is unavailable for %s",
          _owner_class->as_C_string()));
      return false;
    }
    return binding->loader() == loader;
  }

  if (_owner_loader == BuiltinLoader::Unknown) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg("Bytecode enhancement added class %s has unknown owner loader",
                                            _name->as_C_string()));
    return false;
  }
  return _owner_loader == loader;
}

ClassFileStream* ClassAddAction::open_class_stream(TRAPS) {
  // Do not put a ResourceMark here or the returned ClassFileStream will be freed.
  const char* class_name = _name->as_C_string();
  ClassFileStream* stream = load_class_stream(CHECK_NULL);
  if (stream == nullptr) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg("Bytecode enhancement added class file not found for %s", class_name));
    return nullptr;
  }

  log_info(class, load, enhancement)("Bytecode enhancement adding class %s from %s", class_name, stream->source());
  return new ClassFileStream(stream->buffer(), stream->length(), stream->source(),
                             /* from_boot_loader_modules_image */ false,
                             /* from_class_file_load_hook */ true);
}

ClassFileStream* ClassAddAction::load_class_stream(TRAPS) {
  return open_entry_class_stream(_path_entry, _name, CHECK_NULL);
}

void ClassAddAction::print_debug() const {
  ResourceMark rm;
  const char* owner_class = _owner_class == nullptr ? "" : _owner_class->as_C_string();
  log_debug(class, load, enhancement)(
      "  class-add name=%s owner-loader=%s owner-class=%s path=%s",
      _name->as_C_string(), builtin_loader_name(_owner_loader),
      owner_class, _path_entry->name());
}

ClassFileStream* ClassAddActionInternal::load_class_stream(TRAPS) {
  if (!ClassLoader::has_jrt_entry()) {
    return nullptr;
  }

  const char* resource_name = internal_classdata_resource_name(name());
  return ClassLoader::get_jrt_entry()->open_stream(THREAD, resource_name);
}

void ClassAddActionInternal::print_debug() const {
  ResourceMark rm;
  const char* resource_name = internal_classdata_resource_name(name());
  log_debug(class, load, enhancement)("  class-add name=%s owner-loader=%s resource=%s",
                                name()->as_C_string(), builtin_loader_name(owner_loader()), resource_name);
}

void OwnerClassLoaderBinding::set_loader(BuiltinLoader loader, Symbol* owner) {
  assert(loader != BuiltinLoader::Unknown, "sanity");
  int old = Atomic::cmpxchg(&_loader, (int) BuiltinLoader::Unknown, (int) loader);
  if (old != (int) BuiltinLoader::Unknown && old != (int) loader) {
    ResourceMark rm;
    BytecodeEnhancement::handle_failure(err_msg(
        "Bytecode enhancement owner class %s was already bound to %s loader, ignoring %s loader",
        owner->as_C_string(), builtin_loader_name((BuiltinLoader) old), builtin_loader_name(loader)));
  }
}

// ------------------------- [enhancement implementations] -------------------------

void BytecodeEnhancement::handle_failure(const char* message) {
  if (ExitOnBytecodeEnhancementFailure) {
    log_error(class, load, enhancement)("%s", message);
    vm_exit(1);
  } else {
    log_warning(class, load, enhancement)("%s", message);
  }
}

void BytecodeEnhancement::add_internal_class_replacement(const char* name, uint32_t crc32) {
  assert(name != nullptr, "sanity");
  assert(_bytecode_enhancement_actions != nullptr, "must add during initialization");
  Symbol* symbol = SymbolTable::new_symbol(name);
  validate_unique_action(symbol);
  _bytecode_enhancement_actions->put(symbol, new ClassReplacementActionInternal(symbol, crc32));
}

void BytecodeEnhancement::add_internal_class_addition(const char* name, BuiltinLoader loader) {
  assert(name != nullptr, "sanity");
  assert(loader != BuiltinLoader::Unknown && loader != BuiltinLoader::NotBuiltinLoader, "sanity");
  assert(_class_add_actions != nullptr, "must add during initialization");
  Symbol* symbol = SymbolTable::new_symbol(name);
  validate_unique_action(symbol);
  _class_add_actions->put(symbol, new ClassAddActionInternal(symbol, loader));
}

static void dump_bytecode_enhancement_actions() {
  if (!log_is_enabled(Debug, class, load, enhancement)) {
    return;
  }
  if (BytecodeEnhancementPaths != nullptr) {
    log_debug(class, load, enhancement)("Loaded bytecode enhancement actions from %s", BytecodeEnhancementPaths);
  }
  if (_bytecode_enhancement_actions != nullptr) {
    _bytecode_enhancement_actions->iterate_all([&](Symbol* name, ClassEnhancementAction* action) {
      action->print_debug();
    });
  }
  if (_class_add_actions != nullptr) {
    _class_add_actions->iterate_all([&](Symbol* name, ClassAddAction* action) {
      action->print_debug();
    });
  }
}

void BytecodeEnhancement::initialize() {
  assert(_bytecode_enhancement_actions == nullptr, "must initialize once");
  assert(is_enabled(), "must be enabled");

  ResourceMark rm;
  _bytecode_enhancement_actions = new (mtClass) BytecodeEnhancementTable();
  _class_add_actions = new (mtClass) BytecodeEnhancementAddTable();
  _owner_class_loaders = new (mtClass) BytecodeEnhancementOwnerTable();
  if (UsePrimitiveHashSet) {
    add_internal_class_replacement("java/util/HashSet", 0xf477dc75U);
    add_internal_class_addition("java/util/HashSet$PrimitiveHashSet", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$PrimitiveHashSet$1", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$LongHashSet", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$LongHashSet$LongIterator", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$LongHashSet$LongSpliterator", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$LongHashSet$LongArrayList", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$IntHashSet", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$IntHashSet$IntIterator", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$IntHashSet$IntSpliterator", BuiltinLoader::BootLoader);
    add_internal_class_addition("java/util/HashSet$IntHashSet$IntArrayList", BuiltinLoader::BootLoader);
  }
  if (BytecodeEnhancementPaths != nullptr) {
    GrowableArray<ClassPathEntry*> path_entries;
    make_bytecode_enhancement_path_entries(&path_entries);
    if (path_entries.is_empty()) {
      log_warning(class, load, enhancement)(
          "No valid bytecode enhancement paths in BytecodeEnhancementPaths: %s",
          BytecodeEnhancementPaths);
    } else {
      parse_bytecode_enhancement_lists(&path_entries);
    }
  }

  dump_bytecode_enhancement_actions();
}

static ClassEnhancementAction* find_class_enhancement_action(Symbol* name) {
  if (name == nullptr || _bytecode_enhancement_actions == nullptr) {
    return nullptr;
  }
  ClassEnhancementAction** action = _bytecode_enhancement_actions->get(name);
  return action == nullptr ? nullptr : *action;
}

static bool has_class_enhancement_action(Symbol* name) {
  return find_class_enhancement_action(name) != nullptr;
}

static ClassAddAction* find_add_action(Symbol* name) {
  if (name == nullptr || _class_add_actions == nullptr) {
    return nullptr;
  }
  ClassAddAction** action = _class_add_actions->get(name);
  return action == nullptr ? nullptr : *action;
}

bool BytecodeEnhancement::should_enhance_class(Symbol* name) {
  return has_class_enhancement_action(name);
}

bool BytecodeEnhancement::should_add_class(Symbol* name) {
  return find_add_action(name) != nullptr;
}

bool BytecodeEnhancement::should_add_class(Symbol* name, ClassLoaderData* loader_data) {
  ClassAddAction* action = find_add_action(name);
  if (action == nullptr) {
    return false;
  }
  BuiltinLoader requested_loader = builtin_loader(loader_data);
  if (requested_loader == BuiltinLoader::NotBuiltinLoader) {
    return false;
  }
  return action->can_apply(requested_loader);
}

bool BytecodeEnhancement::should_bypass_cds(Symbol* name) {
  return should_enhance_class(name) || should_add_class(name);
}

static Symbol* find_vm_class_replacement(InstanceKlass* klass) {
  if (klass == nullptr) {
    return nullptr;
  }
  if (BytecodeEnhancement::should_enhance_class(klass->name())) {
    return klass->name();
  }

  Symbol* match = find_vm_class_replacement(klass->java_super());
  if (match != nullptr) {
    return match;
  }
  Array<InstanceKlass*>* interfaces = klass->local_interfaces();
  for (int i = 0; i < interfaces->length(); i++) {
    match = find_vm_class_replacement(interfaces->at(i));
    if (match != nullptr) {
      return match;
    }
  }
  return nullptr;
}

static Symbol* find_vm_class_root_replacement() {
#define BYTECODE_ENHANCEMENT_CHECK_VM_CLASS(vm_name, symbol_name) \
  if (BytecodeEnhancement::should_enhance_class(vmSymbols::symbol_name())) { \
    return vmSymbols::symbol_name(); \
  }
  VM_CLASSES_DO(BYTECODE_ENHANCEMENT_CHECK_VM_CLASS)
#undef BYTECODE_ENHANCEMENT_CHECK_VM_CLASS
  return nullptr;
}

void BytecodeEnhancement::validate_vm_class_replacements_for_cds() {
  // VM well-known classes and their type hierarchy cannot be replaced when CDS
  // is used or an archive is being created.
  Symbol* unsupported = find_vm_class_root_replacement();
  if (unsupported == nullptr) {
    for (auto id : EnumRange<vmClassID>{}) {
      unsupported = find_vm_class_replacement(vmClasses::klass_at(id));
      if (unsupported != nullptr) {
        break;
      }
    }
  }
  if (unsupported != nullptr) {
    ResourceMark rm;
    vm_exit_during_initialization(err_msg(
        "BytecodeEnhancement replacement of VM bootstrap class or supertype %s "
        "is not supported while using or creating a CDS archive",
        unsupported->as_C_string()));
  }
}

ClassFileStream* BytecodeEnhancement::enhance_class_stream(
    Symbol* name, ClassFileStream* stream, ClassLoaderData* loader_data, TRAPS) {
  assert(stream != nullptr, "invariant");
  ClassEnhancementAction* action = find_class_enhancement_action(name);
  assert(action != nullptr, "should have been checked by caller");
  return action->enhance_class_stream(stream, loader_data, CHECK_NULL);
}

ClassFileStream* BytecodeEnhancement::open_added_class_stream(
    Symbol* name, ClassLoaderData* loader_data, TRAPS) {
  ClassAddAction* action = find_add_action(name);
  assert(action != nullptr, "should have been checked by caller");
  return action->open_class_stream(CHECK_NULL);
}

#endif // AARCH64
