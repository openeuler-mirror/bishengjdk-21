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

package compiler.arraycopy;

import java.util.Arrays;

/**
 * @test
 * @summary Test long[] arraycopy with the AArch64 streaming-prefetch copy path enabled.
 * @requires os.arch == "aarch64"
 *
 * @run main/othervm/timeout=300 -Xbatch -XX:-TieredCompilation -XX:CompileThreshold=100
 *      -XX:+UnlockDiagnosticVMOptions -XX:+IgnoreUnrecognizedVMOptions
 *      -XX:+UseStreamPrefetchForArrayCopy -XX:StreamPrefetchArrayCopyMinLongs=8
 *      compiler.arraycopy.TestLongArrayCopyWithStreamPrefetch
 * @run main/othervm/timeout=300 -Xbatch -XX:-TieredCompilation -XX:CompileThreshold=100
 *      -XX:+UnlockDiagnosticVMOptions -XX:+IgnoreUnrecognizedVMOptions -XX:-UseSIMDForMemoryOps
 *      -XX:+UseStreamPrefetchForArrayCopy -XX:StreamPrefetchArrayCopyMinLongs=8
 *      compiler.arraycopy.TestLongArrayCopyWithStreamPrefetch
 */
public class TestLongArrayCopyWithStreamPrefetch {
    private static final int[] LENGTHS = {
            0, 1, 2, 3, 4, 5, 6, 7,
            8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
            31, 32, 33, 63, 64, 65,
            95, 96, 97, 127, 128, 129,
            255, 256, 257, 511, 512, 513,
            1023, 1024, 1025, 2048, 4097
    };

    private static volatile long checksum;
    private static volatile Object sink;

    public static void main(String[] args) {
        for (int i = 0; i < 150; i++) {
            runSubset();
        }
        runAll();
    }

    private static void runSubset() {
        for (int i = 0; i < 18; i++) {
            testDisjoint(LENGTHS[i], i & 7, (i * 3) & 7);
            testConjoint(LENGTHS[i], i & 7, ((i * 5) & 7) + 1);
        }
    }

    private static void runAll() {
        for (int len : LENGTHS) {
            for (int srcOff = 0; srcOff < 8; srcOff++) {
                for (int dstOff = 0; dstOff < 8; dstOff++) {
                    testDisjoint(len, srcOff, dstOff);
                }
            }
            int[][] overlaps = {
                    {0, 0}, {0, 1}, {1, 0}, {0, 2}, {2, 0},
                    {3, 5}, {5, 3}, {7, 11}, {11, 7}, {8, 24}, {24, 8}
            };
            for (int[] pair : overlaps) {
                testConjoint(len, pair[0], pair[1]);
            }
            testCopyOf(len);
        }
    }

    private static void testDisjoint(int len, int srcOff, int dstOff) {
        long[] src = new long[len + srcOff + 16];
        long[] dst = new long[len + dstOff + 16];
        fill(src, 17 + len);
        fill(dst, 97 + len);

        long[] expected = dst.clone();
        for (int i = 0; i < len; i++) {
            expected[dstOff + i] = src[srcOff + i];
        }

        System.arraycopy(src, srcOff, dst, dstOff, len);
        checkEquals("disjoint len=" + len + " srcOff=" + srcOff + " dstOff=" + dstOff,
                expected, dst);
        sink = dst;
        checksum += dst.length;
    }

    private static void testConjoint(int len, int srcOff, int dstOff) {
        long[] actual = new long[len + Math.max(srcOff, dstOff) + 32];
        fill(actual, 211 + len);
        long[] expected = actual.clone();

        manualMemmove(expected, srcOff, dstOff, len);
        System.arraycopy(actual, srcOff, actual, dstOff, len);
        checkEquals("conjoint len=" + len + " srcOff=" + srcOff + " dstOff=" + dstOff,
                expected, actual);
        sink = actual;
        checksum += actual.length;
    }

    private static void testCopyOf(int len) {
        long[] src = new long[len];
        fill(src, 311 + len);
        int[] newLengths = {Math.max(0, len - 3), len, len + 5, len + 129};
        for (int newLen : newLengths) {
            long[] copy = Arrays.copyOf(src, newLen);
            for (int i = 0; i < Math.min(len, newLen); i++) {
                if (copy[i] != src[i]) {
                    throw new AssertionError("copyOf data len=" + len + " newLen=" + newLen +
                            " index=" + i);
                }
            }
            for (int i = len; i < newLen; i++) {
                if (copy[i] != 0L) {
                    throw new AssertionError("copyOf zero tail len=" + len + " newLen=" + newLen +
                            " index=" + i);
                }
            }
            sink = copy;
            checksum += copy.length;
        }
    }

    private static void manualMemmove(long[] array, int srcOff, int dstOff, int len) {
        if (len == 0 || srcOff == dstOff) {
            return;
        }
        if (dstOff > srcOff && dstOff < srcOff + len) {
            for (int i = len - 1; i >= 0; i--) {
                array[dstOff + i] = array[srcOff + i];
            }
        } else {
            for (int i = 0; i < len; i++) {
                array[dstOff + i] = array[srcOff + i];
            }
        }
    }

    private static void fill(long[] array, int salt) {
        for (int i = 0; i < array.length; i++) {
            array[i] = value(i + salt * 8191);
        }
    }

    private static long value(int i) {
        long x = 0x9E3779B97F4A7C15L * (i + 1L);
        x ^= x >>> 27;
        x *= 0xC2B2AE3D27D4EB4FL;
        return x ^ (x >>> 31);
    }

    private static void checkEquals(String where, long[] expected, long[] actual) {
        if (expected.length != actual.length) {
            throw new AssertionError(where + " length mismatch");
        }
        for (int i = 0; i < expected.length; i++) {
            if (expected[i] != actual[i]) {
                throw new AssertionError(where + " index=" + i +
                        " expected=" + expected[i] + " actual=" + actual[i]);
            }
        }
    }
}
