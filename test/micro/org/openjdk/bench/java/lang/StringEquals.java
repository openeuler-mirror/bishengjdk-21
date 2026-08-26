/*
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
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

import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

/*
 * This benchmark naively explores String::equals performance
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(value = 3)
public class StringEquals {

    public String test = new String("0123456789");
    public String test2 = new String("tgntogjnrognagronagroangroarngorngaorng");
    public String test3 = new String(test); // equal to test, but not same
    public String test4 = new String("0123\u01FF");
    public String test5 = new String(test4); // equal to test4, but not same
    public String test6 = new String("0123456780");
    public String test7 = new String("0123\u01FE");
    public String longLatin1 = longLatin1(512, -1);
    public String longLatin1Equal = new String(longLatin1);
    public String longLatin1DifferentFirst = longLatin1(512, 0);
    public String longLatin1DifferentMiddle = longLatin1(512, 256);
    public String longLatin1DifferentLast = longLatin1(512, 511);
    public String longUTF16 = longUTF16(512, -1);
    public String longUTF16Equal = new String(longUTF16);
    public String longUTF16DifferentFirst = longUTF16(512, 0);
    public String longUTF16DifferentMiddle = longUTF16(512, 256);
    public String longUTF16DifferentLast = longUTF16(512, 511);

    @Benchmark
    public boolean different() {
        return test.equals(test2);
    }

    @Benchmark
    public boolean equal() {
        return test.equals(test3);
    }

    @Benchmark
    public boolean almostEqual() {
        return test.equals(test6);
    }

    @Benchmark
    public boolean almostEqualUTF16() {
        return test4.equals(test7);
    }

    @Benchmark
    public boolean differentCoders() {
        return test.equals(test4);
    }

    @Benchmark
    public boolean equalsUTF16() {
        return test5.equals(test4);
    }

    @Benchmark
    public boolean longLatin1Equal() {
        return longLatin1.equals(longLatin1Equal);
    }

    @Benchmark
    public boolean longLatin1DifferentFirst() {
        return longLatin1.equals(longLatin1DifferentFirst);
    }

    @Benchmark
    public boolean longLatin1DifferentMiddle() {
        return longLatin1.equals(longLatin1DifferentMiddle);
    }

    @Benchmark
    public boolean longLatin1DifferentLast() {
        return longLatin1.equals(longLatin1DifferentLast);
    }

    @Benchmark
    public boolean longUTF16Equal() {
        return longUTF16.equals(longUTF16Equal);
    }

    @Benchmark
    public boolean longUTF16DifferentFirst() {
        return longUTF16.equals(longUTF16DifferentFirst);
    }

    @Benchmark
    public boolean longUTF16DifferentMiddle() {
        return longUTF16.equals(longUTF16DifferentMiddle);
    }

    @Benchmark
    public boolean longUTF16DifferentLast() {
        return longUTF16.equals(longUTF16DifferentLast);
    }

    private static String longLatin1(int length, int mismatchAt) {
        char[] chars = new char[length];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = (char) ('A' + (i % 26));
        }
        if (mismatchAt >= 0) {
            chars[mismatchAt] = (char) (chars[mismatchAt] == 'Z' ? 'Y' : 'Z');
        }
        return new String(chars);
    }

    private static String longUTF16(int length, int mismatchAt) {
        char[] chars = new char[length];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = (char) ('\u0100' + (i % 127));
        }
        if (mismatchAt >= 0) {
            chars[mismatchAt] = (char) (chars[mismatchAt] == '\u017f' ? '\u0180' : '\u017f');
        }
        return new String(chars);
    }
}
