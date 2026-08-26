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

package org.openjdk.bench.java.util;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 1, time = 15, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 1, time = 15, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = { "-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch" })
public class HashSetApiBenchmark {
    private static final int BATCH = 32;

    public enum Kind {
        LONG, INT, DOUBLE
    }

    @State(Scope.Thread)
    public static class CommonState {
        @Param({ "LONG", "INT", "DOUBLE" })
        public Kind kind;

        @Param({ "1000" })
        public int size;

        Object[] initialKeys;
        Object[] hitKeys;
        Object[] missKeys;
        Object[] addKeys;
        Object[] removeKeys;

        public void setupKeys() {
            initialKeys = new Object[size];
            for (int i = 0; i < size; i++) {
                initialKeys[i] = key(i + 1);
            }
            hitKeys = new Object[BATCH];
            missKeys = new Object[BATCH];
            addKeys = new Object[BATCH];
            removeKeys = new Object[BATCH];
            for (int i = 0; i < BATCH; i++) {
                hitKeys[i] = key((i * 61 % size) + 1);
                missKeys[i] = key(size + 1 + i);
                addKeys[i] = key(size + 1 + i);
                removeKeys[i] = key((i * 61 % size) + 1);
            }
        }

        Object key(int value) {
            if (kind == Kind.LONG) {
                return Long.valueOf(value);
            }
            if (kind == Kind.INT) {
                return Integer.valueOf(value);
            }
            return Double.valueOf(value);
        }

        HashSet<Object> newSet() {
            HashSet<Object> set = new HashSet<>(size * 2);
            for (Object key : initialKeys) {
                set.add(key);
            }
            return set;
        }
    }

    @State(Scope.Thread)
    public static class ReadState extends CommonState {
        HashSet<Object> set;
        SumConsumer consumer;

        @Setup(Level.Trial)
        public void setupSet() {
            setupKeys();
            set = newSet();
            consumer = new SumConsumer();
        }
    }

    @State(Scope.Thread)
    public static class AddState extends CommonState {
        HashSet<Object> set;

        @Setup(Level.Trial)
        public void setupSet() {
            setupKeys();
            set = newSet();
        }

        @TearDown(Level.Invocation)
        public void restoreSet() {
            for (Object key : addKeys) {
                set.remove(key);
            }
        }
    }

    @State(Scope.Thread)
    public static class RemoveState extends CommonState {
        HashSet<Object> set;

        @Setup(Level.Trial)
        public void setupSet() {
            setupKeys();
            set = newSet();
        }

        @TearDown(Level.Invocation)
        public void restoreSet() {
            for (Object key : removeKeys) {
                set.add(key);
            }
        }
    }

    static final class SumConsumer implements Consumer<Object> {
        long sum;

        @Override
        public void accept(Object value) {
            sum += ((Number) value).longValue();
        }
    }

    @Benchmark
    @OperationsPerInvocation(BATCH)
    public int add(AddState state) {
        for (Object key : state.addKeys) {
            state.set.add(key);
        }
        return state.set.size();
    }

    @Benchmark
    @OperationsPerInvocation(BATCH)
    public int remove(RemoveState state) {
        for (Object key : state.removeKeys) {
            state.set.remove(key);
        }
        return state.set.size();
    }

    @Benchmark
    @OperationsPerInvocation(BATCH)
    public int containsHit(ReadState state) {
        int found = 0;
        for (Object key : state.hitKeys) {
            if (state.set.contains(key)) {
                found++;
            }
        }
        return found;
    }

    @Benchmark
    @OperationsPerInvocation(BATCH)
    public int containsMiss(ReadState state) {
        int found = 0;
        for (Object key : state.missKeys) {
            if (state.set.contains(key)) {
                found++;
            }
        }
        return found;
    }

    @Benchmark
    public long iterator(ReadState state) {
        long sum = 0;
        Iterator<Object> iterator = state.set.iterator();
        while (iterator.hasNext()) {
            sum += ((Number) iterator.next()).longValue();
        }
        return sum;
    }

    @Benchmark
    public long forEach(ReadState state) {
        state.consumer.sum = 0;
        state.set.forEach(state.consumer);
        return state.consumer.sum;
    }

    @Benchmark
    public long spliterator(ReadState state) {
        state.consumer.sum = 0;
        Spliterator<Object> spliterator = state.set.spliterator();
        spliterator.forEachRemaining(state.consumer);
        return state.consumer.sum;
    }
}
