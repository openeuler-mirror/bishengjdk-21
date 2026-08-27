/*
 * Copyright (c) 2001, 2025, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_GC_G1_G1CONCURRENTREFINE_HPP
#define SHARE_GC_G1_G1CONCURRENTREFINE_HPP

#include "gc/g1/g1ConcurrentRefineStats.hpp"
#include "gc/g1/g1ConcurrentRefineThreadsNeeded.hpp"
#include "memory/allocation.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#ifdef AARCH64
#include "utilities/growableArray.hpp"
#endif /* AARCH64 */
#include "utilities/macros.hpp"

// Forward decl
#ifdef AARCH64
class G1CardTableClaimTable;
class G1CollectedHeap;
#endif /* AARCH64 */
class G1ConcurrentRefine;
class G1ConcurrentRefineThread;
#ifndef AARCH64
class G1DirtyCardQueueSet;
#endif /* ! AARCH64 */
class G1Policy;
class ThreadClosure;
#ifdef AARCH64
class WorkerTask;
class WorkerThreads;
#endif /* AARCH64 */

// Helper class for refinement thread management. Used to start, stop and
// iterate over them.
class G1ConcurrentRefineThreadControl {
  G1ConcurrentRefine* _cr;
#ifdef AARCH64
  G1ConcurrentRefineThread* _control_thread;

  WorkerThreads* _workers;
#else /* AARCH64 */
  G1ConcurrentRefineThread** _threads;
#endif /* AARCH64 */
  uint _max_num_threads;

  // Create the refinement thread for the given worker id.
  // If initializing is true, ignore InjectGCWorkerCreationFailure.
#ifdef AARCH64
  G1ConcurrentRefineThread* create_refinement_thread();
#else /* AARCH64 */
  G1ConcurrentRefineThread* create_refinement_thread(uint worker_id, bool initializing);
#endif /* AARCH64 */

  NONCOPYABLE(G1ConcurrentRefineThreadControl);

public:
#ifdef AARCH64
  G1ConcurrentRefineThreadControl(uint max_num_threads);
#else /* AARCH64 */
  G1ConcurrentRefineThreadControl();
#endif /* AARCH64 */
  ~G1ConcurrentRefineThreadControl();

#ifdef AARCH64
  jint initialize(G1ConcurrentRefine* cr);
#else /* AARCH64 */
  jint initialize(G1ConcurrentRefine* cr, uint max_num_threads);
#endif /* AARCH64 */

#ifdef AARCH64
  void assert_current_thread_is_control_refinement_thread() const NOT_DEBUG_RETURN;
#else /* AARCH64 */
  void assert_current_thread_is_primary_refinement_thread() const NOT_DEBUG_RETURN;
#endif /* AARCH64 */

  uint max_num_threads() const { return _max_num_threads; }
#ifdef AARCH64
  bool is_refinement_enabled() const { return _max_num_threads > 0; }

  // Activate the control thread.
  void activate();
#endif /* AARCH64 */

#ifdef AARCH64
  void run_task(WorkerTask* task, uint num_workers);
#else /* AARCH64 */
  // Activate the indicated thread.  If the thread has not yet been allocated,
  // allocate and then activate.  If allocation is needed and fails, return
  // false.  Otherwise return true.
  // precondition: worker_id < max_num_threads().
  // precondition: current thread is not the designated worker.
  bool activate(uint worker_id);
#endif /* AARCH64 */

#ifdef AARCH64
  void control_thread_do(ThreadClosure* tc);
#endif /* AARCH64 */
  void worker_threads_do(ThreadClosure* tc);
  void stop();
};

#ifdef AARCH64
// Tracks the current state of re-examining the dirty cards from idle to completion
// (and reset back to idle).
//
// The process steps are as follows:
//
// 1) Swap global card table pointers
//
// 2) Swap Java Thread's card table pointers
//
// 3) Synchronize GC Threads
//      Ensures memory visibility
//
// After this point mutator threads should not mark the refinement table.
//
// 4) Snapshot the heap
//      Determines which regions need to be swept.
//
// 5) Sweep Refinement table
//      Examines non-Clean cards on the refinement table.
//
// 6) Completion Work
//      Calculates statistics about the process to be used in various parts of
//      the garbage collection.
//
// All but step 4 are interruptible by safepoints. In case of a garbage collection,
// the garbage collection will interrupt this process, and go to Idle state.
//
class G1ConcurrentRefineSweepState {

  enum class State : uint {
    Idle,                        // Refinement is doing nothing.
    SwapGlobalCT,                // Swap global card table.
    SwapJavaThreadsCT,           // Swap java thread's card tables.
    SynchronizeGCThreads,        // Synchronize GC thread's memory view.
    SnapshotHeap,                // Take a snapshot of the region's top() values.
    SweepRT,                     // Sweep the refinement table for pending (dirty) cards.
    CompleteRefineWork,          // Cleanup of refinement work, reset to idle.
    Last
  } _state;

  static const char* state_name(State state) {
    static const char* _state_names[] = {
      "Idle",
      "Swap Global Card Table",
      "Swap JavaThread Card Table",
      "Synchronize GC Threads",
      "Snapshot Heap",
      "Sweep Refinement Table",
      "Complete Sweep Work"
    };

    return _state_names[static_cast<uint>(state)];
  }

  // Current heap snapshot.
  G1CardTableClaimTable* _sweep_table;

  // Start times for all states.
  Ticks _state_start[static_cast<uint>(State::Last)];

  void set_state_start_time();
  Tickspan get_duration(State start, State end);

  G1ConcurrentRefineStats _stats;

  // Advances the state to next_state if not interrupted by a changed epoch. Returns
  // to Idle otherwise.
  bool advance_state(State next_state);

  void assert_state(State expected);

  void snapshot_heap_inner();

public:
  G1ConcurrentRefineSweepState(uint max_reserved_regions);
  ~G1ConcurrentRefineSweepState();

  void start_work();

  bool swap_global_card_table();
  bool swap_java_threads_ct();
  bool swap_gc_threads_ct();
  void snapshot_heap(bool concurrent = true);
  void sweep_refinement_table_start();
  bool sweep_refinement_table_step();

  bool complete_work(bool concurrent, bool print_log = true);

  G1CardTableClaimTable* sweep_table() { return _sweep_table; }
  G1ConcurrentRefineStats* stats() { return &_stats; }
  void reset_stats();

  void add_yield_during_sweep_duration(jlong duration);

  bool is_in_progress() const;
  bool are_java_threads_synched() const;
};

#endif /* AARCH64 */
// Controls concurrent refinement.
//
// Mutator threads produce dirty cards, which need to be examined for updates
// to the remembered sets (refinement).  There is a pause-time budget for
// processing these dirty cards (see -XX:G1RSetUpdatingPauseTimePercent).  The
// purpose of concurrent refinement is to (attempt to) ensure the number of
// pending dirty cards at the start of a GC can be processed within that time
// budget.
//
// Concurrent refinement uses dedicated threads and, on the legacy path, mutator
// threads can also refine cards as they produce them.  If configured to not have
// any dedicated threads (-XX:G1ConcRefinementThreads=0), the legacy path lets
// mutators do all refinement work while the AArch64 path performs no refinement.
//
// This class determines the target number of dirty cards pending for the next
// GC.  It also owns the dedicated refinement threads and controls their
// activation in order to achieve that target.
//
// The legacy path has a primary thread and secondary threads; the AArch64 path
// has a control thread and refinement worker threads.  The controlling thread
// decides how many refinement threads should be active and starts or stops work
// to keep pending dirty cards near the target.
class G1ConcurrentRefine : public CHeapObj<mtGC> {
  G1Policy* _policy;
#ifdef AARCH64
  volatile uint _num_threads_wanted;
#else /* AARCH64 */
  volatile uint _threads_wanted;
#endif /* AARCH64 */
  size_t _pending_cards_target;
  Ticks _last_adjust;
  Ticks _last_deactivate;
  bool _needs_adjust;
#ifdef AARCH64
  bool _heap_was_locked;                // The heap has been locked the last time we tried to adjust the number of refinement threads.

#endif /* AARCH64 */
  G1ConcurrentRefineThreadsNeeded _threads_needed;
  G1ConcurrentRefineThreadControl _thread_control;
#ifndef AARCH64
  G1DirtyCardQueueSet& _dcqs;
#endif /* ! AARCH64 */

#ifdef AARCH64
  G1ConcurrentRefineSweepState _sweep_state;
#else /* AARCH64 */
  G1ConcurrentRefine(G1Policy* policy);
#endif /* AARCH64 */

#ifdef AARCH64
  G1ConcurrentRefine(G1CollectedHeap* g1h);
#else /* AARCH64 */
  static uint worker_id_offset();
#endif /* AARCH64 */

  jint initialize();

#ifdef AARCH64
  void assert_current_thread_is_control_refinement_thread() const {
    _thread_control.assert_current_thread_is_control_refinement_thread();
  }
#else /* AARCH64 */
  void assert_current_thread_is_primary_refinement_thread() const {
    _thread_control.assert_current_thread_is_primary_refinement_thread();
  }
#endif /* AARCH64 */

  // For the first few collection cycles we don't have a target (and so don't
  // do any concurrent refinement), because there hasn't been enough pause
  // time refinement work to be done to make useful predictions.  We use
  // SIZE_MAX as a special marker value to indicate we're in this state.
  static const size_t PendingCardsTargetUninitialized = SIZE_MAX;
  bool is_pending_cards_target_initialized() const {
    return _pending_cards_target != PendingCardsTargetUninitialized;
  }

#ifdef AARCH64
  void update_pending_cards_target(double pending_cards_scan_time_ms,
                                   size_t processed_pending_cards,
#else /* AARCH64 */
  void update_pending_cards_target(double logged_cards_scan_time_ms,
                                   size_t processed_logged_cards,
                                   size_t predicted_thread_buffer_cards,
#endif /* AARCH64 */
                                   double goal_ms);

  uint64_t adjust_threads_period_ms() const;
#ifndef AARCH64
  bool is_in_last_adjustment_period() const;

  class RemSetSamplingClosure;  // Helper class for adjusting young length.
  void adjust_young_list_target_length();
#endif /* ! AARCH64 */

  void adjust_threads_wanted(size_t available_bytes);

  NONCOPYABLE(G1ConcurrentRefine);

public:
  ~G1ConcurrentRefine();

#ifdef AARCH64
  G1ConcurrentRefineSweepState& sweep_state() { return _sweep_state; }

  G1ConcurrentRefineSweepState& sweep_state_for_merge();

  void run_with_refinement_workers(WorkerTask* task);

  void notify_region_reclaimed(HeapRegion* r);

#endif /* AARCH64 */
  // Returns a G1ConcurrentRefine instance if succeeded to create/initialize the
  // G1ConcurrentRefine instance. Otherwise, returns null with error code.
#ifdef AARCH64
  static G1ConcurrentRefine* create(G1CollectedHeap* g1h, jint* ecode);
#else /* AARCH64 */
  static G1ConcurrentRefine* create(G1Policy* policy, jint* ecode);
#endif /* AARCH64 */

  // Stop all the refinement threads.
  void stop();

  // Called at the end of a GC to prepare for refinement during the next
  // concurrent phase.  Updates the target for the number of pending dirty
  // cards.  Updates the mutator refinement threshold.  Ensures the refinement
  // adjustment thread (primary on legacy, control on AArch64) is active, so it
  // will adjust the number
  // of running threads.
#ifdef AARCH64
  void adjust_after_gc(double pending_cards_scan_time_ms,
                       size_t processed_pending_cards,
#else /* AARCH64 */
  void adjust_after_gc(double logged_cards_scan_time_ms,
                       size_t processed_logged_cards,
                       size_t predicted_thread_buffer_cards,
#endif /* AARCH64 */
                       double goal_ms);

  // Target number of pending dirty cards at the start of the next GC.
  size_t pending_cards_target() const { return _pending_cards_target; }

#ifdef AARCH64
  // Recalculates the number of refinement threads that should be active in
  // order to meet the pending cards target.
  // Returns true if it could recalculate the number of threads and
  // refinement threads should be started.
  // Returns false if the adjustment period has not expired, or because a timed
  // or requested adjustment could not be performed immediately and so was deferred.
  bool adjust_num_threads_periodically();
#else /* AARCH64 */
  // May recalculate the number of refinement threads that should be active in
  // order to meet the pending cards target.  Returns true if adjustment was
  // performed, and clears any pending request.  Returns false if the
  // adjustment period has not expired, or because a timed or requested
  // adjustment could not be performed immediately and so was deferred.
  // precondition: current thread is the primary refinement thread.
  bool adjust_threads_periodically();
#endif /* AARCH64 */

  // The amount of time (in ms) the thread responsible for refinement adjustment
  // should sleep when it is inactive.  It requests adjustment whenever it is
  // reactivated.
  // precondition: current thread performs refinement adjustment.
  uint64_t adjust_threads_wait_ms() const;

  // Record a request for thread adjustment as soon as possible.
  // precondition: current thread performs refinement adjustment.
  void record_thread_adjustment_needed();

  // Test whether there is a pending request for thread adjustment.
  // precondition: current thread performs refinement adjustment.
  bool is_thread_adjustment_needed() const;

#ifdef AARCH64
  // Indicate that last refinement adjustment had been deferred due to not
  // obtaining the heap lock.
  bool heap_was_locked() const { return _heap_was_locked; }

  uint num_threads_wanted() const { return _num_threads_wanted; }
  uint max_num_threads() const { return _thread_control.max_num_threads(); }
#else /* AARCH64 */
  // Reduce the number of active threads wanted.
  // precondition: current thread is the primary refinement thread.
  void reduce_threads_wanted();

  // Test whether the thread designated by worker_id should be active.
  bool is_thread_wanted(uint worker_id) const;

  // Return total of concurrent refinement stats for the
  // ConcurrentRefineThreads.  Also reset the stats for the threads.
  G1ConcurrentRefineStats get_and_reset_refinement_stats();

  // Perform a single refinement step; called by the refinement
  // threads.  Returns true if there was refinement work available.
  // Updates stats.
  bool try_refinement_step(uint worker_id,
                           size_t stop_at,
                           G1ConcurrentRefineStats* stats);
#endif /* AARCH64 */

  // Iterate over all concurrent refinement threads applying the given closure.
  void threads_do(ThreadClosure *tc);
#ifdef AARCH64
  // Iterate over specific refinement threads applying the given closure.
  void worker_threads_do(ThreadClosure *tc);
  void control_thread_do(ThreadClosure *tc);
#else /* AARCH64 */
  // Maximum number of refinement threads.
  static uint max_num_threads();
#endif /* AARCH64 */
};

#endif // SHARE_GC_G1_G1CONCURRENTREFINE_HPP
