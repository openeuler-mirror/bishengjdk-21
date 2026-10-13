/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
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
 */

#include "precompiled.hpp"
#include "gc/g1/g1RegionToSpaceMapper.hpp"
#include "gc/g1/heapRegionManager.inline.hpp"
#include "memory/allocation.hpp"
#include "memory/universe.hpp"
#include "runtime/os.hpp"
#include "unittest.hpp"

// The dynamic max heap feature guards region commits with "available() >= 1"
// in HeapRegionManager::find_highest_free() and allocate_containing_regions().
// When the cap is exhausted (available() == 0) a region can be uncommitted AND
// not committable, a state that does not exist in vanilla OpenJDK. Both
// functions must handle that state gracefully instead of dereferencing the
// null HeapRegion* of an uncommitted region (SIGSEGV in product builds).
//
// The tests build their own HeapRegionManager so the exhausted-cap state can
// be constructed deterministically, without relying on CDS archive placement
// or other address-layout luck. The dynamic max heap gate is hard-false on
// non-aarch64 platforms (Universe::is_dynamic_max_heap_enable()), so the
// tests skip themselves there.

class DynamicMaxHeapCapGuard : public StackObj {
  bool _saved;
public:
  DynamicMaxHeapCapGuard() {
    _saved = Universe::is_dynamic_max_heap_enable();
    Universe::set_dynamic_max_heap_enable(true);
  }
  bool active() const { return Universe::is_dynamic_max_heap_enable(); }
  ~DynamicMaxHeapCapGuard() { Universe::set_dynamic_max_heap_enable(_saved); }
};

class TestHeapRegionManager {
  static const size_t NUM_REGIONS = 8;
  ReservedSpace _rs;
  G1RegionToSpaceMapper* _mapper;
  HeapRegionManager* _hrm;

public:
  TestHeapRegionManager()
      : _rs(NUM_REGIONS * HeapRegion::GrainBytes, os::vm_page_size()) {
    _mapper = G1RegionToSpaceMapper::create_mapper(_rs,
                                                   _rs.size(),
                                                   os::vm_page_size(),
                                                   HeapRegion::GrainBytes,
                                                   1,
                                                   mtJavaHeap);
    _hrm = new HeapRegionManager();
#ifdef AARCH64
    _hrm->initialize(_mapper, _mapper, _mapper, _mapper, _mapper);
#else
    _hrm->initialize(_mapper, _mapper, _mapper, _mapper);
#endif
  }

  ~TestHeapRegionManager() {
    delete _hrm;
    delete _mapper;
    _rs.release();
  }

  HeapRegionManager* hrm() { return _hrm; }
};

TEST_VM(G1HeapRegionManager, allocate_containing_regions_cap_exhausted) {
  if (!UseG1GC) {
    return;
  }
  DynamicMaxHeapCapGuard guard;
  if (!guard.active()) {
    // Dynamic max heap is AArch64-only: the exhausted-cap state cannot be
    // constructed here. (Early return rather than GTEST_SKIP: the test
    // harness counts skipped gtests as errors.)
    return;
  }
  TestHeapRegionManager t;
  // With a fresh HeapRegionManager length() == 0; setting the cap to zero
  // (equal to the committed length) gives available() == 0 while every
  // region stays uncommitted.
  t.hrm()->set_dynamic_max_heap_length(0);
  ASSERT_EQ(0u, t.hrm()->available());

  size_t commits = 0;
  // Without the fix this dereferences a null HeapRegion* (SIGSEGV).
  MemRegion range = t.hrm()->reserved();
  EXPECT_FALSE(t.hrm()->allocate_containing_regions(range, &commits, nullptr));
}

TEST_VM(G1HeapRegionManager, find_highest_free_cap_exhausted) {
  if (!UseG1GC) {
    return;
  }
  DynamicMaxHeapCapGuard guard;
  if (!guard.active()) {
    // See above: cannot construct the exhausted-cap state off AArch64.
    return;
  }
  TestHeapRegionManager t;
  t.hrm()->set_dynamic_max_heap_length(0);
  ASSERT_EQ(0u, t.hrm()->available());

  // Without the fix this dereferences a null HeapRegion* (SIGSEGV) at the
  // first uncommitted region scanned from the top of the reserved heap.
  bool expanded = false;
  EXPECT_EQ(G1_NO_HRM_INDEX, t.hrm()->find_highest_free(&expanded));
}
