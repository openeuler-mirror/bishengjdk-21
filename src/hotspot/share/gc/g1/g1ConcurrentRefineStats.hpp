/*
 * Copyright (c) 2020, 2025, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_GC_G1_G1CONCURRENTREFINESTATS_HPP
#define SHARE_GC_G1_G1CONCURRENTREFINESTATS_HPP

#include "memory/allocation.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ticks.hpp"

// Collection of statistics for concurrent refinement processing.
// Used for collecting per-thread statistics and for summaries over a
// collection of threads.
class G1ConcurrentRefineStats : public CHeapObj<mtGC> {
#ifndef AARCH64
  Tickspan _refinement_time;
  size_t _refined_cards;
  size_t _precleaned_cards;
  size_t _dirtied_cards;
#else /* AARCH64 */
  jlong _sweep_duration;              // Time spent sweeping the table finding non-clean cards
                                      // and refining them.
  jlong _yield_during_sweep_duration; // Time spent yielding during the sweep (not doing the sweep).

  size_t _cards_scanned;              // Total number of cards scanned.
  size_t _cards_clean;                // Number of cards found clean.
  size_t _cards_not_parsable;         // Number of cards we could not parse and left unrefined.
  size_t _cards_already_refer_to_cset;// Number of cards marked found to be already young.
  size_t _cards_refer_to_cset;        // Number of dirty cards that were recently found to contain a to-cset reference.
  size_t _cards_no_cross_region;      // Number of dirty cards that were dirtied, but then cleaned again by the mutator.

  jlong _refine_duration;             // Time spent during actual refinement.
#endif /* AARCH64 */

public:
  G1ConcurrentRefineStats();

#ifndef AARCH64
  // Time spent performing concurrent refinement.
  Tickspan refinement_time() const { return _refinement_time; }
#else /* AARCH64 */
  // Time spent performing sweeping the refinement table (includes actual refinement,
  // but not yield time).
  jlong sweep_duration() const { return _sweep_duration - _yield_during_sweep_duration; }
  jlong yield_during_sweep_duration() const { return _yield_during_sweep_duration; }
  jlong refine_duration() const { return _refine_duration; }
#endif /* AARCH64 */

  // Number of refined cards.
#ifndef AARCH64
  size_t refined_cards() const { return _refined_cards; }
#else /* AARCH64 */
  size_t refined_cards() const { return cards_not_clean(); }
#endif /* AARCH64 */

#ifndef AARCH64
  // Refinement rate, in cards per ms.
  double refinement_rate_ms() const;
#else /* AARCH64 */
  size_t cards_scanned() const { return _cards_scanned; }
  size_t cards_clean() const { return _cards_clean; }
  size_t cards_not_clean() const { return _cards_scanned - _cards_clean; }
  size_t cards_not_parsable() const { return _cards_not_parsable; }
  size_t cards_already_refer_to_cset() const { return _cards_already_refer_to_cset; }
  size_t cards_refer_to_cset() const { return _cards_refer_to_cset; }
  size_t cards_no_cross_region() const { return _cards_no_cross_region; }
  // Number of cards that were marked dirty and in need of refinement. This includes cards recently
  // found to refer to the collection set as they originally were dirty.
  size_t cards_pending() const { return cards_not_clean() - _cards_already_refer_to_cset; }

  size_t cards_to_cset() const { return _cards_already_refer_to_cset + _cards_refer_to_cset; }

  void inc_sweep_time(jlong t) { _sweep_duration += t; }
  void inc_yield_during_sweep_duration(jlong t) { _yield_during_sweep_duration += t; }
  void inc_refine_duration(jlong t) { _refine_duration += t; }

  void inc_cards_scanned(size_t increment) { _cards_scanned += increment; }
  void inc_cards_clean(size_t increment) { _cards_clean += increment; }
  void inc_cards_not_parsable() { _cards_not_parsable++; }
  void inc_cards_already_refer_to_cset() { _cards_already_refer_to_cset++; }
  void inc_cards_refer_to_cset() { _cards_refer_to_cset++; }
  void inc_cards_no_cross_region() { _cards_no_cross_region++; }
#endif /* AARCH64 */

#ifndef AARCH64
  // Number of cards for which refinement was skipped because some other
  // thread had already refined them.
  size_t precleaned_cards() const { return _precleaned_cards; }

  // Number of cards marked dirty and in need of refinement.
  size_t dirtied_cards() const { return _dirtied_cards; }

  void inc_refinement_time(Tickspan t) { _refinement_time += t; }
  void inc_refined_cards(size_t cards) { _refined_cards += cards; }
  void inc_precleaned_cards(size_t cards) { _precleaned_cards += cards; }
  void inc_dirtied_cards(size_t cards) { _dirtied_cards += cards; }

  G1ConcurrentRefineStats& operator+=(const G1ConcurrentRefineStats& other);
  G1ConcurrentRefineStats& operator-=(const G1ConcurrentRefineStats& other);

  friend G1ConcurrentRefineStats operator+(G1ConcurrentRefineStats x,
                                           const G1ConcurrentRefineStats& y) {
    return x += y;
  }

  friend G1ConcurrentRefineStats operator-(G1ConcurrentRefineStats x,
                                           const G1ConcurrentRefineStats& y) {
    return x -= y;
  }
#else /* AARCH64 */
  void add_atomic(G1ConcurrentRefineStats* other);
#endif /* AARCH64 */

  void reset();
};

#endif // SHARE_GC_G1_G1CONCURRENTREFINESTATS_HPP
