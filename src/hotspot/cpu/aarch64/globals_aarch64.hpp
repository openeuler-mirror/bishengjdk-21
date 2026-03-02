/*
 * Copyright (c) 2000, 2023, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, 2019, Red Hat Inc. All rights reserved.
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

#ifndef CPU_AARCH64_GLOBALS_AARCH64_HPP
#define CPU_AARCH64_GLOBALS_AARCH64_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the runtime system.
// (see globals.hpp)

define_pd_global(bool, ImplicitNullChecks,       true);  // Generate code for implicit null checks
define_pd_global(bool, TrapBasedNullChecks,     false);
define_pd_global(bool, UncommonNullCast,         true);  // Uncommon-trap nulls past to check cast

define_pd_global(bool, DelayCompilerStubsGeneration, COMPILER2_OR_JVMCI);

define_pd_global(uintx, CodeCacheSegmentSize,    64 COMPILER1_AND_COMPILER2_PRESENT(+64)); // Tiered compilation has large code-entry alignment.
define_pd_global(intx, CodeEntryAlignment,       64);
define_pd_global(intx, OptoLoopAlignment,        16);

#define DEFAULT_STACK_YELLOW_PAGES (2)
#define DEFAULT_STACK_RED_PAGES (1)
// Java_java_net_SocketOutputStream_socketWrite0() uses a 64k buffer on the
// stack if compiled for unix and LP64. To pass stack overflow tests we need
// 20 shadow pages.
#define DEFAULT_STACK_SHADOW_PAGES (20 DEBUG_ONLY(+5))
#define DEFAULT_STACK_RESERVED_PAGES (1)

#define MIN_STACK_YELLOW_PAGES DEFAULT_STACK_YELLOW_PAGES
#define MIN_STACK_RED_PAGES    DEFAULT_STACK_RED_PAGES
#define MIN_STACK_SHADOW_PAGES DEFAULT_STACK_SHADOW_PAGES
#define MIN_STACK_RESERVED_PAGES (0)

define_pd_global(bool, VMContinuations, true);

define_pd_global(intx, StackYellowPages, DEFAULT_STACK_YELLOW_PAGES);
define_pd_global(intx, StackRedPages, DEFAULT_STACK_RED_PAGES);
define_pd_global(intx, StackShadowPages, DEFAULT_STACK_SHADOW_PAGES);
define_pd_global(intx, StackReservedPages, DEFAULT_STACK_RESERVED_PAGES);

define_pd_global(bool, RewriteBytecodes,     true);
define_pd_global(bool, RewriteFrequentPairs, true);

define_pd_global(bool, PreserveFramePointer, false);

define_pd_global(uintx, TypeProfileLevel, 111);

define_pd_global(bool, CompactStrings, true);

// Clear short arrays bigger than one word in an arch-specific way
define_pd_global(intx, InitArrayShortSize, BytesPerLong);

#if defined(COMPILER1) || defined(COMPILER2)
define_pd_global(intx, InlineSmallCode,          1000);
#endif

#define ARCH_FLAGS(develop,                                             \
                   product,                                             \
                   notproduct,                                          \
                   range,                                               \
                   constraint)                                          \
                                                                        \
  product(bool, NearCpool, true,                                        \
         "constant pool is close to instructions")                      \
  product(bool, UseNeon, false,                                         \
          "Use Neon for CRC32 computation")                             \
  product(bool, UseCRC32, false,                                        \
          "Use CRC32 instructions for CRC32 computation")               \
  product(bool, UseCryptoPmullForCRC32, false,                          \
          "Use Crypto PMULL instructions for CRC32 computation")        \
  product(bool, UseSIMDForMemoryOps, false,                             \
          "Use SIMD instructions in generated memory move code")        \
  product(bool, UseSIMDForArrayEquals, true,                            \
          "Use SIMD instructions in generated array equals code")       \
  product(bool, UseSimpleArrayEquals, false,                            \
          "Use simplest and shortest implementation for array equals")  \
  product(bool, UseSIMDForBigIntegerShiftIntrinsics, true,              \
          "Use SIMD instructions for left/right shift of BigInteger")   \
  product(bool, AvoidUnalignedAccesses, false,                          \
          "Avoid generating unaligned memory accesses")                 \
  product(bool, UseLSE, false,                                          \
          "Use LSE instructions")                                       \
  product(uint, UseSVE, 0,                                              \
          "Highest supported SVE instruction set version")              \
          range(0, 2)                                                   \
  product(bool, UseCompactObjectHeaders, false, EXPERIMENTAL,           \
          "Use compact 64-bit object headers in 64-bit VM")             \
  product(bool, UseBlockZeroing, true,                                  \
          "Use DC ZVA for block zeroing")                               \
  product(intx, BlockZeroingLowLimit, 256,                              \
          "Minimum size in bytes when block zeroing will be used")      \
          range(wordSize, max_jint)                                     \
  product(bool, TraceTraps, false, "Trace all traps the signal handler")\
                                                                        \
  product(bool, ExitVMProfileCacheFlush, false, EXPERIMENTAL,           \
          "ExitVMProfileCacheFlush")                                    \
                                                                        \
  product(bool, JProfilingCacheRecording, false, EXPERIMENTAL,          \
          "Collect profiling information for JProfilingCache")          \
                                                                        \
  product(bool, JProfilingCacheCompileAdvance, false, EXPERIMENTAL,     \
          "Enable JProfilingCacheCompileAdvance from a log file")       \
                                                                        \
  product(bool, ProfileCacheAggressiveInit, false, EXPERIMENTAL,         \
          "JProfileCache replay precompile strategy: "\
          "false=conservative (link-only verify+prepare, no proactive <clinit>); "\
          "true=aggressive (proactively initialize replay classes)")      \
                                                                        \
  product(ccstr, CompilationProfileCacheExclude, nullptr, EXPERIMENTAL, \
          "JProfilingCacheCompileAdvance excluding list ")              \
                                                                        \
  product(bool,  UseJProfilingCacheSystemBlackList, true, EXPERIMENTAL, \
          "Block Some System Classes loaded by jprofilecache")          \
                                                                        \
  product(uintx, JProfilingCacheDelayLoadTime, 1000, EXPERIMENTAL,      \
          "Sleep time (in milliseconds) before JProfileCache loads "    \
          "classes and methods profile. In aggressive replay mode, "    \
          "values smaller than 50 are adjusted to 50 automatically.")   \
          range(0, 3600000)                                             \
                                                                        \
  develop(bool, CompilationProfileCacheResolveClassEagerly, true,       \
          "resolve class from constant pool eagerly")                   \
                                                                        \
  product(ccstr, ProfilingCacheFile, nullptr, EXPERIMENTAL,             \
          "Log file name for JProfilingCache")                          \
                                                                        \
  product(uintx, CompilationProfileCacheAppID, 0, EXPERIMENTAL,         \
          "Application ID written in log file for verification ")       \
          range(0, 4294967295)                                          \
                                                                        \
  product(ccstr, JProfilingCacheAutoArchiveDir, nullptr, EXPERIMENTAL,  \
          "Specify JProfilingCache directory under which the "          \
          "jprofilecache file will be auto generated and replayed")     \
                                                                        \
  product(int, JProfilingCacheMaxTierLimit, 3, EXPERIMENTAL,            \
          "If compile_level is higher than the option, method will "    \
          "be precompiled by the option level")                         \
          range(1, 4)                                                   \
                                                                        \
  product(bool, JProfilingCacheReplayProfileData, false, EXPERIMENTAL,  \
          "Load method data with dumped ProfileData in the "            \
          "jprofilecache file if exists")                               \
                                                                        \
  product(int, SoftwarePrefetchHintDistance, -1,                        \
          "Use prfm hint with specified distance in compiled code."     \
          "Value -1 means off.")                                        \
          range(-1, 4096)                                               \
  product(ccstr, OnSpinWaitInst, "none", DIAGNOSTIC,                    \
          "The instruction to use to implement "                        \
          "java.lang.Thread.onSpinWait()."                              \
          "Options: none, nop, isb, yield.")                            \
  product(uint, OnSpinWaitInstCount, 1, DIAGNOSTIC,                     \
          "The number of OnSpinWaitInst instructions to generate."      \
          "It cannot be used with OnSpinWaitInst=none.")                \
          range(1, 99)                                                  \
  product(ccstr, UseBranchProtection, "none",                           \
          "Branch Protection to use: none, standard, pac-ret")          \
                                                                        \
  product(size_t, DynamicMaxHeapSizeLimit, ScaleForWordSize(96*M),      \
          "The limit of Dynamic maximum heap size (in bytes)")          \
                                                                        \
  product(uintx, DynamicMaxHeapShrinkMinFreeRatio, 40,                  \
          "Minimal ratio of free bytes after dynamic max heap shirnk")  \
                                                                        \
  product(size_t, ElasticMaxHeapSize, ScaleForWordSize(96*M),           \
          "Elastic maximum heap size (in bytes)")                       \
                                                                        \
  product(bool, ElasticMaxHeap, false,                                  \
          "Allow change max heap size during runtime with jcmd")        \
                                                                        \
  product(bool, TraceElasticMaxHeap, false,                             \
          "Trace Elastic Max Heap adjustion logs and failure reasons")  \
                                                                        \
  product(uintx, ElasticMaxHeapShrinkMinFreeRatio, 40,                  \
          "minimal ratio of free bytes after elastic max heap shirnk")  \
                                                                        \
  product(bool, LogNUMANodes, false,                                    \
          "Print NUMANodes")                                            \
                                                                        \
  product(ccstr, NUMANodes, NULL,                                       \
          "This parameter provides the same functionality as"           \
          "'numactl --all -N <nodes> -m <nodes>'."                      \
          "<nodes> can be '0-2', '0,1,2', 'all' and so on.")            \
                                                                        \
  product(uintx, NUMANodesRandom, 0,                                    \
          "Number of continuous nodes to bind"                          \
          "with the first node randomly chosen."                        \
          "NUMANodesRandom has higher priority than NUMANodes")         \

// end of ARCH_FLAGS

#endif // CPU_AARCH64_GLOBALS_AARCH64_HPP
