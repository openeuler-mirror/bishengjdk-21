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

package org.openjdk.bench.java.lang;

import java.util.Random;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Measures {@link Integer#toHexString(int)} on 50 synthetic keys that all
 * produce eight hexadecimal digits. The keys are either spread across the
 * range or placed in two clusters near its endpoints.
 *
 * On a supported AArch64 VM, compare separate forks with
 * {@code -XX:-UseIntegerToHexStringIntrinsic} and
 * {@code -XX:+UseIntegerToHexStringIntrinsic}.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(value = 3, jvmArgsAppend = "-XX:+CompactStrings")
@State(Scope.Thread)
public class RangeHex50 {

    private static final int LOW = 1_027_167_994;
    private static final int HIGH = 1_777_437_144;
    private static final int SIZE = 4096;

    @Param({"SPREAD_50", "TWO_CLUSTERS_50"})
    private String distribution;

    private int[] values;
    private int cursor;

    @Setup
    public void setup() {
        int[] keys = new int[50];
        for (int i = 0; i < keys.length; i++) {
            keys[i] = switch (distribution) {
                case "SPREAD_50" -> LOW + (int) ((long) (HIGH - LOW) * i / (keys.length - 1));
                // Two clusters of 25 keys, each within 984 of an endpoint.
                case "TWO_CLUSTERS_50" -> i < 25 ? LOW + i * 41 : HIGH - (i - 25) * 41;
                default -> throw new IllegalArgumentException(
                        "Unknown distribution: " + distribution);
            };
        }

        values = new int[SIZE];
        for (int i = 0; i < SIZE; i++) {
            values[i] = keys[i % keys.length];
        }
        Random random = new Random(0x1234abcdL);
        for (int i = SIZE - 1; i > 0; i--) {
            int j = random.nextInt(i + 1);
            int value = values[i];
            values[i] = values[j];
            values[j] = value;
        }
        for (int value : values) {
            String expected = Long.toHexString(Integer.toUnsignedLong(value));
            if (value < LOW || value > HIGH || expected.length() != 8
                    || !Integer.toHexString(value).equals(expected)) {
                throw new AssertionError("Invalid workload value: " + value);
            }
        }
        cursor = 0;
    }

    @Benchmark
    public String convert() {
        return Integer.toHexString(values[cursor++ & (SIZE - 1)]);
    }
}
