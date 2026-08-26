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

package compiler.c2.irTests;

import compiler.lib.ir_framework.*;
import jdk.test.lib.Asserts;

/*
 * @test
 * @summary Test reassociation of a narrow four-way vector long multiply pattern.
 * @requires vm.compiler2.enabled & vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @run driver compiler.c2.irTests.TestLongVectorMultiplyReassociation
 */

public class TestLongVectorMultiplyReassociation {
    private static final int SIZE = 3000;

    private static final long[] SRC_A = new long[SIZE];
    private static final long[] SRC_B = new long[SIZE];
    private static final long[] DST = new long[SIZE];

    public static void main(String[] args) {
        for (int i = 0; i < SIZE; i++) {
            SRC_A[i] = i * 0x1234_5678_9ABCL + 0x1020_3040_5060_7080L;
            SRC_B[i] = i * -0x3344_5566_7788L + 0x7FFF_0000_1234_5678L;
        }
        TestFramework.runWithFlags("-XX:UseSVE=1", "-XX:MaxVectorSize=32");
    }

    @Test
    // MaxVectorSize=32 emits nine vector bodies for this loop on SVE. The
    // reassociation keeps each body to two MulVL nodes instead of the old
    // three-deep left-associated chain.
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {IRNode.MUL_VL, ">0"})
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {IRNode.MUL_VL, "<=18"})
    private static void testSameLeafFourMul() {
        for (int i = 0; i < SIZE; i++) {
            long v = SRC_A[i];
            DST[i] = v * v * v * v;
        }
    }

    @Test
    private static void testDifferentLeafFourMul() {
        for (int i = 0; i < SIZE; i++) {
            DST[i] = SRC_A[i] * SRC_A[i] * SRC_A[i] * SRC_B[i];
        }
    }

    @Test
    private static void testTwoSquares() {
        for (int i = 0; i < SIZE; i++) {
            DST[i] = SRC_A[i] * SRC_A[i] * SRC_B[i] * SRC_B[i];
        }
    }

    @Run(test = {"testSameLeafFourMul", "testDifferentLeafFourMul", "testTwoSquares"})
    private void run() {
        testSameLeafFourMul();
        for (int i = 0; i < SIZE; i++) {
            long v = SRC_A[i];
            Asserts.assertEquals(DST[i], v * v * v * v);
        }

        testDifferentLeafFourMul();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(DST[i], SRC_A[i] * SRC_A[i] * SRC_A[i] * SRC_B[i]);
        }

        testTwoSquares();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(DST[i], SRC_A[i] * SRC_A[i] * SRC_B[i] * SRC_B[i]);
        }
    }
}
