/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, 2023, Loongson Technology. All rights reserved.
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

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "classfile/vmIntrinsics.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/arguments.hpp"
#include "runtime/java.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/vm_version.hpp"
#include "os_linux.hpp"
#ifdef TARGET_OS_FAMILY_linux
# include "os_linux.inline.hpp"
#endif

VM_Version::CpuidInfo VM_Version::_cpuid_info   = { 0, };
bool VM_Version::_cpu_info_is_initialized = false;

static BufferBlob* stub_blob;
static const int stub_size = 600;

extern "C" {
  typedef void (*get_cpu_info_stub_t)(void*);
}
static get_cpu_info_stub_t get_cpu_info_stub = nullptr;


class VM_Version_StubGenerator: public StubCodeGenerator {
 public:

  VM_Version_StubGenerator(CodeBuffer *c) : StubCodeGenerator(c) {}

  address generate_get_cpu_info() {
    assert(!VM_Version::cpu_info_is_initialized(), "VM_Version should not be initialized");
    StubCodeMark mark(this, "VM_Version", "get_cpu_info_stub");
#   define __ _masm->

    address start = __ pc();

    __ enter();
    __ push(AT);
    __ push(T5);

    __ li(AT, (long)0);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id0_offset()));

    __ li(AT, 1);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id1_offset()));

    __ li(AT, 2);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id2_offset()));

    __ li(AT, 3);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id3_offset()));

    __ li(AT, 4);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id4_offset()));

    __ li(AT, 5);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id5_offset()));

    __ li(AT, 6);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id6_offset()));

    __ li(AT, 10);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id10_offset()));

    __ li(AT, 11);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id11_offset()));

    __ li(AT, 12);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id12_offset()));

    __ li(AT, 13);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id13_offset()));

    __ li(AT, 14);
    __ cpucfg(T5, AT);
    __ st_w(T5, A0, in_bytes(VM_Version::Loongson_Cpucfg_id14_offset()));

    __ pop(T5);
    __ pop(AT);
    __ leave();
    __ jr(RA);
#   undef __
    return start;
  };
};

uint32_t VM_Version::get_feature_flags_by_cpucfg() {
  uint32_t result = 0;
  if (_cpuid_info.cpucfg_info_id1.bits.ARCH == 0b00 || _cpuid_info.cpucfg_info_id1.bits.ARCH == 0b01 ) {
    result |= CPU_LA32;
  } else if (_cpuid_info.cpucfg_info_id1.bits.ARCH == 0b10 ) {
    result |= CPU_LA64;
  }

  if (_cpuid_info.cpucfg_info_id2.bits.FP_CFG != 0)
    result |= CPU_FP;
  if (_cpuid_info.cpucfg_info_id2.bits.LAM_BH != 0)
    result |= CPU_LAM_BH;
  if (_cpuid_info.cpucfg_info_id2.bits.LAMCAS != 0)
    result |= CPU_LAMCAS;

  if (_cpuid_info.cpucfg_info_id3.bits.CCDMA != 0)
    result |= CPU_CCDMA;
  if (_cpuid_info.cpucfg_info_id3.bits.LLDBAR != 0)
    result |= CPU_LLDBAR;
  if (_cpuid_info.cpucfg_info_id3.bits.SCDLY != 0)
    result |= CPU_SCDLY;
  if (_cpuid_info.cpucfg_info_id3.bits.LLEXC != 0)
    result |= CPU_LLEXC;

  result |= CPU_ULSYNC;

  return result;
}

void VM_Version::get_processor_features() {

  clean_cpuFeatures();

  get_os_cpu_info();

  get_cpu_info_stub(&_cpuid_info);
  _features |= get_feature_flags_by_cpucfg();

  _supports_cx8 = true;

  if (UseG1GC && FLAG_IS_DEFAULT(MaxGCPauseMillis)) {
    FLAG_SET_DEFAULT(MaxGCPauseMillis, 150);
  }

  if (supports_lsx()) {
    if (FLAG_IS_DEFAULT(UseLSX)) {
      FLAG_SET_DEFAULT(UseLSX, true);
    }
  } else if (UseLSX) {
    warning("LSX instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseLSX, false);
  }

  if (supports_lasx()) {
    if (FLAG_IS_DEFAULT(UseLASX)) {
      FLAG_SET_DEFAULT(UseLASX, true);
    }
  } else if (UseLASX) {
    warning("LASX instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseLASX, false);
  }

  if (UseLASX && !UseLSX) {
    warning("LASX instructions depends on LSX, setting UseLASX to false");
    FLAG_SET_DEFAULT(UseLASX, false);
  }

  if (supports_lam_bh()) {
    if (FLAG_IS_DEFAULT(UseAMBH)) {
      FLAG_SET_DEFAULT(UseAMBH, true);
    }
  } else if (UseAMBH) {
    warning("AM{SWAP/ADD}{_DB}.{B/H} instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseAMBH, false);
  }

  if (supports_lamcas()) {
    if (FLAG_IS_DEFAULT(UseAMCAS)) {
      FLAG_SET_DEFAULT(UseAMCAS, true);
    }
  } else if (UseAMCAS) {
    warning("AMCAS{_DB}.{B/H/W/D} instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseAMCAS, false);
  }
#ifdef COMPILER2
  int max_vector_size = 0;
  int min_vector_size = 0;
  if (UseLASX) {
    max_vector_size = 32;
    min_vector_size = 4;
  }
  else if (UseLSX) {
    max_vector_size = 16;
    min_vector_size = 4;
  }

  if (!FLAG_IS_DEFAULT(MaxVectorSize)) {
    if (MaxVectorSize == 0) {
      // do nothing
    } else if (MaxVectorSize > max_vector_size) {
      warning("MaxVectorSize must be at most %i on this platform", max_vector_size);
      FLAG_SET_DEFAULT(MaxVectorSize, max_vector_size);
    } else if (MaxVectorSize < min_vector_size) {
      warning("MaxVectorSize must be at least %i or 0 on this platform, setting to: %i", min_vector_size, min_vector_size);
      FLAG_SET_DEFAULT(MaxVectorSize, min_vector_size);
    } else if (!is_power_of_2(MaxVectorSize)) {
      warning("MaxVectorSize must be a power of 2, setting to default: %i", max_vector_size);
      FLAG_SET_DEFAULT(MaxVectorSize, max_vector_size);
    }
  } else {
    // If default, use highest supported configuration
    FLAG_SET_DEFAULT(MaxVectorSize, max_vector_size);
  }
#endif

  char buf[256];

  // A note on the _features_string format:
  //   There are jtreg tests checking the _features_string for various properties.
  //   For some strange reason, these tests require the string to contain
  //   only _lowercase_ characters. Keep that in mind when being surprised
  //   about the unusual notation of features - and when adding new ones.
  //   Features may have one comma at the end.
  //   Furthermore, use one, and only one, separator space between features.
  //   Multiple spaces are considered separate tokens, messing up everything.
  jio_snprintf(buf, sizeof(buf), "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s, "
    "0x%lx, fp_ver: %d, lvz_ver: %d, ",
    (is_la64()             ?  "la64"  : ""),
    (is_la32()             ?  "la32"  : ""),
    (supports_lsx()        ?  ", lsx" : ""),
    (supports_lasx()       ?  ", lasx" : ""),
    (supports_crypto()     ?  ", crypto" : ""),
    (supports_lam()        ?  ", am" : ""),
    (supports_ual()        ?  ", ual" : ""),
    (supports_lldbar()     ?  ", lldbar" : ""),
    (supports_scdly()      ?  ", scdly" : ""),
    (supports_llexc()      ?  ", llexc" : ""),
    (supports_lbt_x86()    ?  ", lbt_x86" : ""),
    (supports_lbt_arm()    ?  ", lbt_arm" : ""),
    (supports_lbt_mips()   ?  ", lbt_mips" : ""),
    (needs_llsync()        ?  ", needs_llsync" : ""),
    (needs_tgtsync()       ?  ", needs_tgtsync": ""),
    (needs_ulsync()        ?  ", needs_ulsync": ""),
    (supports_lam_bh()     ?  ", lam_bh" : ""),
    (supports_lamcas()     ?  ", lamcas" : ""),
    _cpuid_info.cpucfg_info_id0.bits.PRID,
    _cpuid_info.cpucfg_info_id2.bits.FP_VER,
    _cpuid_info.cpucfg_info_id2.bits.LVZ_VER);
  _features_string = os::strdup(buf);

  assert(!is_la32(), "Should Not Reach Here, what is the cpu type?");
  assert( is_la64(), "Should be LoongArch64");

  if (FLAG_IS_DEFAULT(AllocatePrefetchStyle)) {
    FLAG_SET_DEFAULT(AllocatePrefetchStyle, 1);
  }

  if (FLAG_IS_DEFAULT(AllocatePrefetchLines)) {
    FLAG_SET_DEFAULT(AllocatePrefetchLines, 3);
  }

  if (FLAG_IS_DEFAULT(AllocatePrefetchStepSize)) {
    FLAG_SET_DEFAULT(AllocatePrefetchStepSize, 64);
  }

  if (FLAG_IS_DEFAULT(AllocatePrefetchDistance)) {
    FLAG_SET_DEFAULT(AllocatePrefetchDistance, 192);
  }

  if (FLAG_IS_DEFAULT(AllocateInstancePrefetchLines)) {
    FLAG_SET_DEFAULT(AllocateInstancePrefetchLines, 1);
  }

  // Basic instructions are used to implement SHA Intrinsics on LA, so sha
  // instructions support is not needed.
  if (/*supports_crypto()*/ 1) {
    if (FLAG_IS_DEFAULT(UseSHA)) {
      FLAG_SET_DEFAULT(UseSHA, true);
    }
  } else if (UseSHA) {
    warning("SHA instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseSHA, false);
  }

  if (UseSHA/* && supports_crypto()*/) {
    if (FLAG_IS_DEFAULT(UseSHA1Intrinsics)) {
      FLAG_SET_DEFAULT(UseSHA1Intrinsics, true);
    }
  } else if (UseSHA1Intrinsics) {
    warning("Intrinsics for SHA-1 crypto hash functions not available on this CPU.");
    FLAG_SET_DEFAULT(UseSHA1Intrinsics, false);
  }

  if (UseSHA/* && supports_crypto()*/) {
    if (FLAG_IS_DEFAULT(UseSHA256Intrinsics)) {
      FLAG_SET_DEFAULT(UseSHA256Intrinsics, true);
    }
  } else if (UseSHA256Intrinsics) {
    warning("Intrinsics for SHA-224 and SHA-256 crypto hash functions not available on this CPU.");
    FLAG_SET_DEFAULT(UseSHA256Intrinsics, false);
  }

  if (UseSHA512Intrinsics) {
    warning("Intrinsics for SHA-384 and SHA-512 crypto hash functions not available on this CPU.");
    FLAG_SET_DEFAULT(UseSHA512Intrinsics, false);
  }

  if (UseSHA3Intrinsics) {
    warning("Intrinsics for SHA3-224, SHA3-256, SHA3-384 and SHA3-512 crypto hash functions not available on this CPU.");
    FLAG_SET_DEFAULT(UseSHA3Intrinsics, false);
  }

  if (!(UseSHA1Intrinsics || UseSHA256Intrinsics || UseSHA3Intrinsics || UseSHA512Intrinsics)) {
    FLAG_SET_DEFAULT(UseSHA, false);
  }

  if (FLAG_IS_DEFAULT(UseMD5Intrinsics)) {
    FLAG_SET_DEFAULT(UseMD5Intrinsics, true);
  }

  // Basic instructions are used to implement AES Intrinsics on LA, so AES
  // instructions support is not needed.
  if (/*supports_crypto()*/ 1) {
    if (FLAG_IS_DEFAULT(UseAES)) {
      FLAG_SET_DEFAULT(UseAES, true);
    }
  } else if (UseAES) {
    if (!FLAG_IS_DEFAULT(UseAES))
      warning("AES instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseAES, false);
  }

  if (UseAES/* && supports_crypto()*/) {
    if (FLAG_IS_DEFAULT(UseAESIntrinsics)) {
      FLAG_SET_DEFAULT(UseAESIntrinsics, true);
    }
  } else if (UseAESIntrinsics) {
    if (!FLAG_IS_DEFAULT(UseAESIntrinsics))
      warning("AES intrinsics are not available on this CPU");
    FLAG_SET_DEFAULT(UseAESIntrinsics, false);
  }

  if (UseAESCTRIntrinsics) {
    warning("AES/CTR intrinsics are not available on this CPU");
    FLAG_SET_DEFAULT(UseAESCTRIntrinsics, false);
  }

  if (FLAG_IS_DEFAULT(UseCRC32)) {
    FLAG_SET_DEFAULT(UseCRC32, true);
  }

  if (UseCRC32) {
    if (FLAG_IS_DEFAULT(UseCRC32Intrinsics)) {
      UseCRC32Intrinsics = true;
    }

    if (FLAG_IS_DEFAULT(UseCRC32CIntrinsics)) {
      UseCRC32CIntrinsics = true;
    }
  }

  if (UseLSX) {
      if (FLAG_IS_DEFAULT(UseChaCha20Intrinsics)) {
          UseChaCha20Intrinsics = true;
      }
  } else if (UseChaCha20Intrinsics) {
      if (!FLAG_IS_DEFAULT(UseChaCha20Intrinsics))
          warning("ChaCha20 intrinsic requires LSX instructions");
      FLAG_SET_DEFAULT(UseChaCha20Intrinsics, false);
  }

#ifdef COMPILER2
  if (FLAG_IS_DEFAULT(UseMulAddIntrinsic)) {
    FLAG_SET_DEFAULT(UseMulAddIntrinsic, true);
  }

  if (FLAG_IS_DEFAULT(UseMontgomeryMultiplyIntrinsic)) {
    UseMontgomeryMultiplyIntrinsic = true;
  }
  if (FLAG_IS_DEFAULT(UseMontgomerySquareIntrinsic)) {
    UseMontgomerySquareIntrinsic = true;
  }

  if (UseFPUForSpilling && !FLAG_IS_DEFAULT(UseFPUForSpilling)) {
    if (UseCompressedOops || UseCompressedClassPointers) {
      warning("UseFPUForSpilling not supported when UseCompressedOops or UseCompressedClassPointers is on");
      UseFPUForSpilling = false;
    }
  }
#endif

  // This machine allows unaligned memory accesses
  if (FLAG_IS_DEFAULT(UseUnalignedAccesses)) {
    FLAG_SET_DEFAULT(UseUnalignedAccesses, true);
  }

  if (FLAG_IS_DEFAULT(UseFMA)) {
    FLAG_SET_DEFAULT(UseFMA, true);
  }

  if (FLAG_IS_DEFAULT(UseCopySignIntrinsic)) {
    FLAG_SET_DEFAULT(UseCopySignIntrinsic, true);
  }

  if (UseLSX) {
    if (FLAG_IS_DEFAULT(UsePopCountInstruction)) {
      FLAG_SET_DEFAULT(UsePopCountInstruction, true);
    }
  } else if (UsePopCountInstruction) {
    if (!FLAG_IS_DEFAULT(UsePopCountInstruction))
      warning("PopCountI/L/VI(4) employs LSX whereas PopCountVI(8) hinges on LASX.");
    FLAG_SET_DEFAULT(UsePopCountInstruction, false);
  }

  if (UseLASX) {
    if (FLAG_IS_DEFAULT(UseBigIntegerShiftIntrinsic)) {
      FLAG_SET_DEFAULT(UseBigIntegerShiftIntrinsic, true);
    }
  } else if (UseBigIntegerShiftIntrinsic) {
    if (!FLAG_IS_DEFAULT(UseBigIntegerShiftIntrinsic))
      warning("Intrinsic for BigInteger.shiftLeft/Right() employs LASX.");
    FLAG_SET_DEFAULT(UseBigIntegerShiftIntrinsic, false);
  }

  if (UseActiveCoresMP) {
    if (os::Linux::sched_active_processor_count() != 1) {
      if (!FLAG_IS_DEFAULT(UseActiveCoresMP))
        warning("UseActiveCoresMP disabled because active processors are more than one.");
      FLAG_SET_DEFAULT(UseActiveCoresMP, false);
    }
  } else { // !UseActiveCoresMP
    if (FLAG_IS_DEFAULT(UseActiveCoresMP) && !os::is_MP()) {
      FLAG_SET_DEFAULT(UseActiveCoresMP, true);
    }
  }

#ifdef COMPILER2
  if (FLAG_IS_DEFAULT(AlignVector)) {
    AlignVector = false;
  }
#endif // COMPILER2
}

void VM_Version::initialize() {
  ResourceMark rm;
  // Making this stub must be FIRST use of assembler

  stub_blob = BufferBlob::create("get_cpu_info_stub", stub_size);
  if (stub_blob == nullptr) {
    vm_exit_during_initialization("Unable to allocate get_cpu_info_stub");
  }
  CodeBuffer c(stub_blob);
  VM_Version_StubGenerator g(&c);
  get_cpu_info_stub = CAST_TO_FN_PTR(get_cpu_info_stub_t,
                                     g.generate_get_cpu_info());

  get_processor_features();
}

void VM_Version::initialize_cpu_information(void) {
  // do nothing if cpu info has been initialized
  if (_initialized) {
    return;
  }

  _no_of_cores  = os::processor_count();
  _no_of_threads = _no_of_cores;
  _no_of_sockets = _no_of_cores;
  snprintf(_cpu_name, CPU_TYPE_DESC_BUF_SIZE - 1, "LoongArch");
  snprintf(_cpu_desc, CPU_DETAILED_DESC_BUF_SIZE, "LoongArch %s", features_string());
  _initialized = true;
}

bool VM_Version::is_intrinsic_supported(vmIntrinsicID id) {
  assert(id != vmIntrinsics::_none, "must be a VM intrinsic");
  switch (id) {
  case vmIntrinsics::_floatToFloat16:
  case vmIntrinsics::_float16ToFloat:
    if (!supports_float16()) {
      return false;
    }
    break;
  default:
    break;
  }
  return true;
}
