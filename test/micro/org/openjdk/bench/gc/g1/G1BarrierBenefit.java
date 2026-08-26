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
 */
package org.openjdk.bench.gc.g1;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.CompilerControl;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Threads;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.ThreadParams;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 3, jvmArgs = {
        "-XX:+UseG1GC",
        "-Xms4g",
        "-Xmx4g",
        "-XX:+AlwaysPreTouch"
})
@Threads(32)
public class G1BarrierBenefit {

    private static final int DESTINATION_ARRAY_COUNT = 128;
    private static final int ARRAY_LENGTH = 1 << 20;
    private static final int CHUNK_SIZE = 1024;
    private static final int POOL_SIZE = 1 << 20;
    private static final int SOURCE_ARRAY_COUNT = 32;
    private static final int FILL_STRIDE = 128;

    private static final int DESTINATION_ARRAY_MASK =
            DESTINATION_ARRAY_COUNT - 1;
    private static final int ARRAY_LENGTH_MASK = ARRAY_LENGTH - 1;
    private static final int CHUNK_COUNT_MASK =
            (ARRAY_LENGTH / CHUNK_SIZE) - 1;
    private static final int POOL_MASK = POOL_SIZE - 1;

    private static final long SEQUENCE_INCREMENT = 0x9E3779B97F4A7C15L;
    private static final long CHUNK_MIXER = 0xBF58476D1CE4E5B9L;

    static final class Node {
        Object ref;
        long a;
        long b;
        long c;
        long d;

        Node(Object ref, long value) {
            this.ref = ref;
            this.a = value;
            this.b = value * 3 + 1;
            this.c = value * 5 + 7;
            this.d = value * 11 + 13;
        }
    }

    @State(Scope.Benchmark)
    public static class SharedState {
        Object[][] destinations;
        Object[][] sources;
        Object[] pool;
        Object[] youngPool;

        @Setup(Level.Trial)
        public void setup() throws InterruptedException {
            destinations = new Object[DESTINATION_ARRAY_COUNT][];
            Object filler = new Node(null, 42);
            for (int i = 0; i < destinations.length; i++) {
                Object[] destination = new Object[ARRAY_LENGTH];
                for (int j = 0; j < destination.length; j += FILL_STRIDE) {
                    destination[j] = filler;
                }
                destinations[i] = destination;
            }

            pool = new Object[POOL_SIZE];
            for (int i = 0; i < pool.length; i++) {
                pool[i] = new Node(filler, i);
            }

            sources = new Object[SOURCE_ARRAY_COUNT][];
            for (int i = 0; i < sources.length; i++) {
                Object[] source = new Object[CHUNK_SIZE];
                for (int j = 0; j < source.length; j++) {
                    source[j] = pool[(i * 131 + j) & POOL_MASK];
                }
                sources[i] = source;
            }

            for (int i = 0; i < 3; i++) {
                System.gc();
                Thread.sleep(1000L);
            }

            youngPool = new Object[POOL_SIZE];
            for (int i = 0; i < youngPool.length; i++) {
                youngPool[i] = new Node(pool[i], i);
            }
        }
    }

    @State(Scope.Thread)
    public static class WorkerState {
        long sequence;
        int sourceIndex;

        @Setup(Level.Trial)
        public void setup(ThreadParams threadParams) {
            int threadIndex = threadParams.getThreadIndex();
            sequence = ((long) threadIndex << 32) ^ SEQUENCE_INCREMENT;
            sourceIndex = threadIndex & (SOURCE_ARRAY_COUNT - 1);
        }
    }

    @Benchmark
    @Threads(4)
    @OperationsPerInvocation(CHUNK_SIZE)
    @CompilerControl(CompilerControl.Mode.DONT_INLINE)
    public void arrayCopy(SharedState shared, WorkerState worker) {
        long sequence = worker.sequence;
        int destinationIndex =
                (int) ((sequence >>> 16) & DESTINATION_ARRAY_MASK);
        int chunkIndex =
                (int) ((sequence * CHUNK_MIXER) & CHUNK_COUNT_MASK);

        System.arraycopy(shared.sources[worker.sourceIndex], 0,
                shared.destinations[destinationIndex],
                chunkIndex * CHUNK_SIZE, CHUNK_SIZE);
        worker.sequence = sequence + SEQUENCE_INCREMENT;
    }

    @Benchmark
    @CompilerControl(CompilerControl.Mode.DONT_INLINE)
    public void scalarOldToOld(SharedState shared, WorkerState worker) {
        long sequence = worker.sequence;
        int destinationIndex =
                (int) ((sequence >>> 20) & DESTINATION_ARRAY_MASK);
        int slot =
                (int) ((sequence * FILL_STRIDE + worker.sourceIndex)
                        & ARRAY_LENGTH_MASK);

        shared.destinations[destinationIndex][slot] =
                shared.pool[(int) sequence & POOL_MASK];
        worker.sequence = sequence + SEQUENCE_INCREMENT;
    }

    @Benchmark
    @CompilerControl(CompilerControl.Mode.DONT_INLINE)
    public void scalarOldToYoung(SharedState shared, WorkerState worker) {
        long sequence = worker.sequence;
        int destinationIndex =
                (int) ((sequence >>> 20) & DESTINATION_ARRAY_MASK);
        int slot =
                (int) ((sequence * FILL_STRIDE + worker.sourceIndex)
                        & ARRAY_LENGTH_MASK);

        shared.destinations[destinationIndex][slot] =
                shared.youngPool[(int) sequence & POOL_MASK];
        worker.sequence = sequence + SEQUENCE_INCREMENT;
    }
}
