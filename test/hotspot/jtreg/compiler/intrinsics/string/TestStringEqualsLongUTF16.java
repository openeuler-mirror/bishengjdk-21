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

/*
 * @test
 * @summary Test long UTF16 String.equals intrinsic correctness on AArch64.
 * @requires os.arch == "aarch64"
 * @requires vm.compiler2.enabled
 * @requires vm.flavor == "server" & (vm.opt.TieredStopAtLevel == null | vm.opt.TieredStopAtLevel == 4)
 * @library /test/lib /
 * @modules java.base/jdk.internal.misc
 *
 * @build jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 *
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI
 *      -XX:CompileCommand=inline,java.lang.String::equals
 *      -XX:-UseOnStackReplacement -XX:-BackgroundCompilation
 *      -XX:-UseSimpleArrayEquals -XX:+UseSIMDForArrayEquals -XX:-UseSIMDForStringEquals
 *      compiler.intrinsics.string.TestStringEqualsLongUTF16
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI
 *      -XX:CompileCommand=inline,java.lang.String::equals
 *      -XX:-UseOnStackReplacement -XX:-BackgroundCompilation
 *      -XX:-UseSimpleArrayEquals -XX:+UseSIMDForArrayEquals
 *      -XX:+UseHisiOptimizations -XX:+UseSIMDForStringEquals
 *      compiler.intrinsics.string.TestStringEqualsLongUTF16
 */

package compiler.intrinsics.string;

import java.lang.reflect.Method;
import jdk.test.whitebox.WhiteBox;

import static compiler.whitebox.CompilerWhiteBoxTest.COMP_LEVEL_FULL_OPTIMIZATION;
import static jdk.test.lib.Asserts.assertEQ;
import static jdk.test.lib.Asserts.assertTrue;

public class TestStringEqualsLongUTF16 {
    private static final int STUB_THRESHOLD = 3 * 64;
    private static final int ITERS = 20_000;
    private static final WhiteBox WHITE_BOX = WhiteBox.getWhiteBox();
    private static volatile boolean sink;
    private static final int[] LENGTHS = {
            STUB_THRESHOLD - 10, STUB_THRESHOLD - 9,
            STUB_THRESHOLD - 8, STUB_THRESHOLD - 7,
            STUB_THRESHOLD - 1, STUB_THRESHOLD,
            STUB_THRESHOLD + 1, STUB_THRESHOLD + 7,
            STUB_THRESHOLD + 8, STUB_THRESHOLD + 9,
            255, 256, 257, 511, 512, 513, 1024
    };
    private static final int[] EXHAUSTIVE_LENGTHS = {
            STUB_THRESHOLD - 10, STUB_THRESHOLD - 9,
            STUB_THRESHOLD - 8, STUB_THRESHOLD - 7,
            STUB_THRESHOLD - 1, STUB_THRESHOLD,
            STUB_THRESHOLD + 1, STUB_THRESHOLD + 7,
            STUB_THRESHOLD + 8, STUB_THRESHOLD + 9
    };
    private static final char[] UTF16_ALPHABET = {
            '\u4e2d', '\u6587', '\u6d4b', '\u8bd5',
            '\ud55c', '\uae00', '\ud14c', '\uc2a4',
            '\u65e5', '\u672c', '\u8a9e', '\u30c6',
            '\u03a9', '\u20ac'
    };

    public static void main(String[] args) throws Exception {
        warmupEqualsHot();
        compileEqualsHot();

        for (int length : EXHAUSTIVE_LENGTHS) {
            testEveryMismatch(length, length);
        }

        for (int i = 0; i < ITERS; i++) {
            for (int length : LENGTHS) {
                testLength(length, i);
            }
        }
    }

    private static void warmupEqualsHot() {
        String left = utf16String(STUB_THRESHOLD + 8, 0, -1);
        String equal = utf16String(STUB_THRESHOLD + 8, 0, -1);
        String different = utf16String(STUB_THRESHOLD + 8, 0, STUB_THRESHOLD);
        for (int i = 0; i < 20_000; i++) {
            sink = equalsHot(left, equal);
            sink = equalsHot(left, different);
        }
    }

    private static void compileEqualsHot() throws NoSuchMethodException {
        Method method = TestStringEqualsLongUTF16.class.getDeclaredMethod(
                "equalsHot", String.class, String.class);
        WHITE_BOX.enqueueMethodForCompilation(method, COMP_LEVEL_FULL_OPTIMIZATION);
        assertTrue(WHITE_BOX.isMethodCompiled(method), "equalsHot must be compiled");
        assertEQ(WHITE_BOX.getMethodCompilationLevel(method), COMP_LEVEL_FULL_OPTIMIZATION,
                "equalsHot must be compiled by C2");
    }

    private static void testEveryMismatch(int length, int salt) {
        String expected = utf16String(length, salt, -1);
        for (int mismatchAt = 0; mismatchAt < length; mismatchAt++) {
            checkEquals(expected, utf16String(length, salt, mismatchAt), false,
                    "utf16 exhaustive mismatch at " + mismatchAt);
        }
    }

    private static void testLength(int length, int salt) {
        String expected = utf16String(length, salt, -1);
        checkEquals(expected, utf16String(length, salt, -1), true, "utf16 equal");

        for (int mismatchAt : mismatchIndexes(length)) {
            checkEquals(expected, utf16String(length, salt, mismatchAt), false,
                    "utf16 mismatch at " + mismatchAt);
        }
        checkEquals(expected, utf16String(length + 1, salt, -1), false, "utf16 longer");
        checkEquals(expected, utf16String(length - 1, salt, -1), false, "utf16 shorter");
        checkEquals(latin1String(length), expected, false, "mixed coder");
    }

    private static int[] mismatchIndexes(int length) {
        return new int[] {
                0,
                Math.min(1, length - 1),
                Math.min(7, length - 1),
                Math.min(8, length - 1),
                Math.min(9, length - 1),
                length / 2,
                Math.max(0, length - 9),
                Math.max(0, length - 8),
                Math.max(0, length - 7),
                Math.max(0, length - 2),
                length - 1
        };
    }

    private static void checkEquals(String left, String right, boolean expected, String context) {
        boolean actual = equalsHot(left, right);
        if (actual != expected) {
            throw new AssertionError(context + ": expected " + expected + " for lengths "
                    + left.length() + " and " + right.length());
        }
        actual = equalsHot(right, left);
        if (actual != expected) {
            throw new AssertionError(context + ": expected symmetric " + expected + " for lengths "
                    + right.length() + " and " + left.length());
        }
    }

    private static boolean equalsHot(String left, String right) {
        return left.equals(right);
    }

    private static String utf16String(int length, int salt, int mismatchAt) {
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = UTF16_ALPHABET[Math.floorMod(i * 5 + salt, UTF16_ALPHABET.length)];
        }
        if (mismatchAt >= 0) {
            chars[mismatchAt] = differentUtf16(chars[mismatchAt]);
        }
        return new String(chars);
    }

    private static String latin1String(int length) {
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = (char) ('A' + (i % 26));
        }
        return new String(chars);
    }

    private static char differentUtf16(char ch) {
        return ch == '\u4e2d' ? '\ud55c' : '\u4e2d';
    }
}
