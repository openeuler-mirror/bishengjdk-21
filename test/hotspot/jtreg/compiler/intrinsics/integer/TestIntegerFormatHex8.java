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

/*
 * @test
 * @summary Integer.formatHex8 semantics, exact store bounds, and fallback paths
 * @modules java.base/java.lang:open
 * @run main/othervm -XX:-TieredCompilation -Xbatch --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8
 * @run main/othervm -XX:-TieredCompilation -Xbatch -XX:+UnlockDiagnosticVMOptions -XX:DisableIntrinsic=_formatHex8 --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8
 * @run main/othervm -XX:-TieredCompilation -Xbatch -XX:-CompactStrings --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8
 * @run main/othervm -XX:TieredStopAtLevel=1 -Xbatch --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8
 * @run main/othervm -Xint --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8
 */

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.Arrays;
import java.util.Random;

public class TestIntegerFormatHex8 {
    private static final MethodHandle FORMAT_HEX8 = findHelper();
    private static final byte SENTINEL = (byte) 0xa5;
    private static final int[] VALUES = {
        0, 1, 15, 16, 255, 256, 0x0fffffff, 0x10000000,
        Integer.MAX_VALUE, Integer.MIN_VALUE, -1, 0x1234abcd,
        0x01234567, 0x89abcdef, 0x55555555, 0xaaaaaaaa
    };
    private static final int[] LENGTHS = { 8, 9, 16, 31 };
    private static int assertions;
    private static volatile int warmupSink;

    private static MethodHandle findHelper() {
        try {
            return MethodHandles.privateLookupIn(Integer.class, MethodHandles.lookup())
                    .findStatic(Integer.class, "formatHex8",
                            MethodType.methodType(void.class, int.class, byte[].class));
        } catch (NoSuchMethodException e) {
            throw new AssertionError("Required Integer.formatHex8(int, byte[]) helper is missing", e);
        } catch (IllegalAccessException e) {
            throw new AssertionError("Run with --add-opens=java.base/java.lang=ALL-UNNAMED", e);
        }
    }

    // A constant MethodHandle lets C2 see the private helper's call site.
    public static void format(int value, byte[] buffer) throws Throwable {
        FORMAT_HEX8.invokeExact(value, buffer);
    }

    public static String publicHex(int value) {
        return Integer.toHexString(value);
    }

    private static String oracle(int value) {
        return Long.toHexString(Integer.toUnsignedLong(value));
    }

    private static String fixedOracle(int value) {
        String hex = oracle(value);
        return "0".repeat(8 - hex.length()) + hex;
    }

    private static void checkPublic(int value) {
        long unsigned = Integer.toUnsignedLong(value);
        equal(publicHex(value), oracle(value), "public hex", value);
        equal(Integer.toBinaryString(value), Long.toBinaryString(unsigned), "binary fallback", value);
        equal(Integer.toOctalString(value), Long.toOctalString(unsigned), "octal fallback", value);
    }

    private static void equal(String actual, String expected, String operation, int value) {
        assertions++;
        if (!actual.equals(expected)) {
            throw new AssertionError(operation + " value=" + value + " expected="
                    + expected + " actual=" + actual);
        }
    }

    private static void checkBuffer(byte[] buffer, int value) {
        String expected = fixedOracle(value);
        for (int index = 0; index < 8; index++) {
            assertions++;
            if (buffer[index] != (byte) expected.charAt(index)) {
                throw new AssertionError("helper value=" + value + " index=" + index
                        + " expected=" + expected + " actual=" + Arrays.toString(buffer));
            }
        }
        for (int index = 8; index < buffer.length; index++) {
            assertions++;
            if (buffer[index] != SENTINEL) {
                throw new AssertionError("helper overwrote tail: length=" + buffer.length
                        + " value=" + value + " index=" + index);
            }
        }
    }

    private static void checkHelper(int value) throws Throwable {
        for (int length : LENGTHS) {
            byte[] buffer = new byte[length];
            Arrays.fill(buffer, SENTINEL);
            format(value, buffer);
            checkBuffer(buffer, value);
        }
    }

    private static void checkExceptions() throws Throwable {
        for (int value : VALUES) {
            assertions++;
            try {
                format(value, null);
                throw new AssertionError("helper accepted null buffer");
            } catch (NullPointerException expected) {
                // Reexecution must retain the Java helper's exception.
            }
            for (int length = 0; length < 8; length++) {
                byte[] buffer = new byte[length];
                Arrays.fill(buffer, SENTINEL);
                assertions++;
                try {
                    format(value, buffer);
                    throw new AssertionError("helper accepted buffer length " + length);
                } catch (ArrayIndexOutOfBoundsException expected) {
                    // The first Java write is buf[7], so shorter arrays are untouched.
                }
                for (byte element : buffer) {
                    assertions++;
                    if (element != SENTINEL) {
                        throw new AssertionError("helper changed short buffer length " + length);
                    }
                }
            }
        }
    }

    private static void checkAliasing() throws Throwable {
        byte[] first = new byte[31];
        byte[] second = new byte[31];
        for (int index = 0; index < 10_000; index++) {
            int value = index * 0x9e3779b9;
            Arrays.fill(first, SENTINEL);
            Arrays.fill(second, SENTINEL);
            format(value, first);
            format(~value, second);
            checkBuffer(first, value);
            checkBuffer(second, ~value);
            // A later store to the same byte[] must be visible to subsequent reads.
            format(value ^ 0x13579bdf, first);
            checkBuffer(first, value ^ 0x13579bdf);
            checkBuffer(second, ~value);
        }
    }

    private static void warmup() throws Throwable {
        byte[] buffer = new byte[8];
        int checksum = 0;
        for (int index = 0; index < 100_000; index++) {
            int value = index * 0x9e3779b9;
            format(value, buffer);
            checksum += buffer[index & 7];
            checksum += publicHex(value).length();
            checksum += Integer.toBinaryString(value).length();
            checksum += Integer.toOctalString(value).length();
        }
        warmupSink = checksum;
    }

    public static void main(String[] args) throws Throwable {
        for (int value : VALUES) {
            checkPublic(value);
            checkHelper(value);
        }
        warmup();
        for (int value : VALUES) {
            checkPublic(value);
            checkHelper(value);
        }
        Random random = new Random(0x1234abcdL);
        for (int index = 0; index < 32_768; index++) {
            int value = random.nextInt();
            checkPublic(value);
            checkHelper(value);
        }
        checkAliasing();
        checkExceptions();
        System.out.println("TestIntegerFormatHex8 assertions=" + assertions
                + " boundaryValues=" + VALUES.length + " randomValues=32768"
                + " helperLengths=" + Arrays.toString(LENGTHS) + " warmupSink=" + warmupSink);
    }
}
