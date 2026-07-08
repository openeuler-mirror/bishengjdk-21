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
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#ifndef SHARE_CLASSFILE_BYTECODEENHANCEMENT_HPP
#define SHARE_CLASSFILE_BYTECODEENHANCEMENT_HPP

#ifdef AARCH64
#include "memory/allStatic.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/globalDefinitions.hpp"

class ClassFileStream;
class ClassLoaderData;
class Symbol;
enum class BuiltinLoader : int;

class BytecodeEnhancement : AllStatic {
 private:
  static bool _bytecode_enhancement_enabled;

  static void add_internal_class_replacement(const char* name, uint32_t crc32);
  static void add_internal_class_addition(const char* name, BuiltinLoader loader);

 public:
  static void enable() { _bytecode_enhancement_enabled = true; }
  // True after bytecode enhancement has been requested, even if no valid action is found.
  static bool is_enabled() { return _bytecode_enhancement_enabled; }
  static void initialize();
  static bool should_enhance_class(Symbol* name);
  static bool should_add_class(Symbol* name);
  static bool should_add_class(Symbol* name, ClassLoaderData* loader_data);
  static bool should_bypass_cds(Symbol* name);
  static void validate_vm_class_replacements_for_cds();
  static void handle_failure(const char* message);
  static ClassFileStream* enhance_class_stream(Symbol* name, ClassFileStream* stream,
                                              ClassLoaderData* loader_data, TRAPS);
  static ClassFileStream* open_added_class_stream(Symbol* name,
                                                  ClassLoaderData* loader_data, TRAPS);

};

#endif // AARCH64
#endif // SHARE_CLASSFILE_BYTECODEENHANCEMENT_HPP
