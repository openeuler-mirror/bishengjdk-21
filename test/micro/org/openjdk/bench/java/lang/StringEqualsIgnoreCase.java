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

import java.util.Arrays;
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
 * Measures {@link String#equalsIgnoreCase(String)} for every compact-string
 * coder direction around the intrinsic threshold and at representative
 * longer lengths.
 *
 * Run separate forks with VM options to compare paths, for example:
 * {@code -XX:-UseStringEqualsIgnoreCaseIntrinsic} for the scalar path and
 * {@code -XX:+UseStringEqualsIgnoreCaseIntrinsic} for the intrinsic path.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(value = 3, jvmArgsAppend = "-XX:+CompactStrings")
@State(Scope.Thread)
public class StringEqualsIgnoreCase {

    @Param({"LL", "LU", "UL", "UU"})
    private String coderPair;

    @Param({"15", "16", "17", "64", "256", "1024"})
    private int length;

    @Param({"equal", "last"})
    private String relation;

    private String left;
    private String right;

    @Setup
    public void setup() {
        char leftChar;
        char rightChar;
        switch (coderPair) {
            case "LL" -> {
                leftChar = 's';
                rightChar = 'S';
            }
            case "LU" -> {
                leftChar = 's';
                rightChar = '\u017f';
            }
            case "UL" -> {
                leftChar = '\u017f';
                rightChar = 's';
            }
            case "UU" -> {
                leftChar = '\u0100';
                rightChar = '\u0101';
            }
            default -> throw new IllegalArgumentException(
                    "Unknown coder pair: " + coderPair);
        }

        char[] leftChars = new char[length];
        char[] rightChars = new char[length];
        Arrays.fill(leftChars, leftChar);
        Arrays.fill(rightChars, rightChar);

        boolean expected;
        switch (relation) {
            case "equal" -> expected = true;
            case "last" -> {
                expected = false;
                rightChars[length - 1] = '#';
            }
            default -> throw new IllegalArgumentException(
                    "Unknown relation: " + relation);
        }

        left = new String(leftChars);
        right = new String(rightChars);
        if (left.equalsIgnoreCase(right) != expected) {
            throw new AssertionError("Unexpected equalsIgnoreCase result for "
                    + coderPair + "/" + length + "/" + relation);
        }
    }

    @Benchmark
    public boolean equalsIgnoreCase() {
        return left.equalsIgnoreCase(right);
    }
}
