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
package org.openjdk.bench.java.util;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Threads;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
public class VectorizedMismatch {

    public enum Scenario {
        MATCH,
        EARLY,
        MIDDLE,
        LAST
    }

    @Param({"32", "256", "4096"})
    private int byteSize;

    @Param
    private Scenario scenario;

    private byte[] bytesA;
    private byte[] bytesB;
    private char[] charsA;
    private char[] charsB;
    private int[] intsA;
    private int[] intsB;
    private long[] longsA;
    private long[] longsB;

    @Setup(Level.Trial)
    public void setup() {
        bytesA = new byte[byteSize];
        charsA = new char[byteSize / Character.BYTES];
        intsA = new int[byteSize / Integer.BYTES];
        longsA = new long[byteSize / Long.BYTES];

        for (int i = 0; i < bytesA.length; i++) {
            bytesA[i] = (byte) (i * 31 + 17);
        }
        for (int i = 0; i < charsA.length; i++) {
            charsA[i] = (char) (i * 31 + 17);
        }
        for (int i = 0; i < intsA.length; i++) {
            intsA[i] = i * 31 + 17;
        }
        for (int i = 0; i < longsA.length; i++) {
            longsA[i] = i * 31L + 17L;
        }

        bytesB = Arrays.copyOf(bytesA, bytesA.length);
        charsB = Arrays.copyOf(charsA, charsA.length);
        intsB = Arrays.copyOf(intsA, intsA.length);
        longsB = Arrays.copyOf(longsA, longsA.length);

        int byteMismatch = mismatchIndex(bytesB.length);
        int charMismatch = mismatchIndex(charsB.length);
        int intMismatch = mismatchIndex(intsB.length);
        int longMismatch = mismatchIndex(longsB.length);

        if (byteMismatch >= 0) {
            bytesB[byteMismatch] ^= 1;
            charsB[charMismatch] ^= 1;
            intsB[intMismatch] ^= 1;
            longsB[longMismatch] ^= 1L;
        }

        verify("byte", byteMismatch, Arrays.mismatch(bytesA, bytesB));
        verify("char", charMismatch, Arrays.mismatch(charsA, charsB));
        verify("int", intMismatch, Arrays.mismatch(intsA, intsB));
        verify("long", longMismatch, Arrays.mismatch(longsA, longsB));
    }

    private int mismatchIndex(int length) {
        return switch (scenario) {
            case MATCH -> -1;
            case EARLY -> 1;
            case MIDDLE -> length / 2;
            case LAST -> length - 1;
        };
    }

    private static void verify(String type, int expected, int actual) {
        if (actual != expected) {
            throw new IllegalStateException(type + " mismatch: expected " +
                    expected + ", actual " + actual);
        }
    }

    @Benchmark
    public int bytes() {
        return Arrays.mismatch(bytesA, bytesB);
    }

    @Benchmark
    public int chars() {
        return Arrays.mismatch(charsA, charsB);
    }

    @Benchmark
    public int ints() {
        return Arrays.mismatch(intsA, intsB);
    }

    @Benchmark
    public int longs() {
        return Arrays.mismatch(longsA, longsB);
    }
}
