/*
 * Copyright (c) 2026, Huawei Technologies Co. Ltd. All rights reserved.
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

package compiler.memoryinitialization;

import java.util.Arrays;

/**
 * @test
 * @summary Test AArch64 SVE small-block zeroing and its disabled fallback.
 * @requires os.arch == "aarch64"
 *
 * @run main/othervm/timeout=300 -Xbatch -XX:-TieredCompilation -XX:CompileThreshold=100
 *      -XX:+UnlockDiagnosticVMOptions -XX:+IgnoreUnrecognizedVMOptions
 *      -XX:+UseSVESmallBlockZeroing -XX:SVESmallBlockZeroingMaxWords=256
 *      compiler.memoryinitialization.TestSVESmallBlockZeroing
 * @run main/othervm/timeout=300 -Xbatch -XX:-TieredCompilation -XX:CompileThreshold=100
 *      -XX:+UnlockDiagnosticVMOptions -XX:+IgnoreUnrecognizedVMOptions
 *      -XX:-UseSVESmallBlockZeroing
 *      compiler.memoryinitialization.TestSVESmallBlockZeroing
 * @run main/othervm/timeout=300 -Xbatch -XX:-TieredCompilation -XX:CompileThreshold=100
 *      -XX:+UnlockDiagnosticVMOptions -XX:+IgnoreUnrecognizedVMOptions
 *      -XX:UseSVE=0 -XX:+UseSVESmallBlockZeroing
 *      compiler.memoryinitialization.TestSVESmallBlockZeroing
 */
public class TestSVESmallBlockZeroing {
    private static final int[] LENGTHS = {
            0, 1, 2, 3, 4, 5, 6, 7,
            8, 9, 10, 11, 12, 13, 14, 15,
            16, 17, 18, 31, 32, 33,
            63, 64, 65, 95, 96, 97,
            127, 128, 129, 191, 192, 193,
            255, 256, 257, 511, 512, 513,
            1023, 1024, 1025
    };

    private static volatile Object sink;
    private static volatile long checksum;

    public static void main(String[] args) {
        for (int i = 0; i < 200; i++) {
            runSubset();
        }
        runAll();
    }

    private static void runSubset() {
        for (int i = 0; i < 24; i++) {
            int len = LENGTHS[i];
            checkLongArrayAllocation(len);
            checkLongArrayFill(len);
        }
    }

    private static void runAll() {
        for (int len : LENGTHS) {
            checkLongArrayAllocation(len);
            checkIntAndObjectArrayAllocation(len);
            checkLongArrayFill(len);
        }
    }

    private static void checkLongArrayAllocation(int len) {
        long[] dirty = new long[len + 17];
        Arrays.fill(dirty, 0x7F7F7F7F7F7F7F7FL);
        sink = dirty;

        long[] zero = new long[len];
        for (int i = 0; i < zero.length; i++) {
            if (zero[i] != 0L) {
                throw new AssertionError("new long[] not zero len=" + len +
                        " index=" + i + " value=" + zero[i]);
            }
        }
        sink = zero;
        checksum += zero.length;
    }

    private static void checkIntAndObjectArrayAllocation(int len) {
        int[] ints = new int[len];
        Object[] refs = new Object[len];
        for (int i = 0; i < len; i++) {
            if (ints[i] != 0) {
                throw new AssertionError("new int[] not zero len=" + len + " index=" + i);
            }
            if (refs[i] != null) {
                throw new AssertionError("new Object[] not null len=" + len + " index=" + i);
            }
        }
        sink = refs;
        checksum += ints.length + refs.length;
    }

    private static void checkLongArrayFill(int len) {
        long[] array = new long[len + 16];
        Arrays.fill(array, 0x123456789ABCDEFL);
        Arrays.fill(array, 0L);
        for (int i = 0; i < array.length; i++) {
            if (array[i] != 0L) {
                throw new AssertionError("Arrays.fill whole len=" + len + " index=" + i);
            }
        }

        Arrays.fill(array, 0xCAFEBABECAFEL);
        int from = Math.min(7, array.length);
        int to = Math.max(from, array.length - 5);
        Arrays.fill(array, from, to, 0L);
        for (int i = 0; i < array.length; i++) {
            long expected = (i >= from && i < to) ? 0L : 0xCAFEBABECAFEL;
            if (array[i] != expected) {
                throw new AssertionError("Arrays.fill range len=" + len + " index=" + i);
            }
        }
        sink = array;
        checksum += array.length;
    }
}
