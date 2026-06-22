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
 * @summary Test AArch64 SVE multiply reduction matcher rules.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @run driver compiler.c2.irTests.TestAArch64SVEMultiplyReduction
 */

public class TestAArch64SVEMultiplyReduction {
    private static final int SIZE = 2048;

    private static final int[] INT_A = new int[SIZE];
    private static final int[] INT_B = new int[SIZE];
    private static final long[] LONG_A = new long[SIZE];
    private static final long[] LONG_B = new long[SIZE];

    public static void main(String[] args) {
        for (int i = 0; i < SIZE; i++) {
            INT_A[i] = ((i % 7) << 1) + 1;
            INT_B[i] = ((i % 5) << 1) + 1;
            LONG_A[i] = ((i % 7) << 1) + 1L;
            LONG_B[i] = ((i % 5) << 1) + 1L;
        }
        TestFramework.runWithFlags("-XX:UseSVE=1",
                                   "-XX:MaxVectorSize=32",
                                   "-XX:+SuperWordReductions",
                                   "-XX:LoopMaxUnroll=8",
                                   "-XX:LoopUnrollLimit=250");
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"reduce_mulI_sve", ">0"},
        phase = CompilePhase.MATCHING)
    private static int testIntMulReduction() {
        int acc = 1;
        for (int i = 0; i < SIZE; i++) {
            int val = INT_A[i] * INT_B[i];
            acc *= val;
        }
        return acc;
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"reduce_mulL_sve", ">0"},
        phase = CompilePhase.MATCHING)
    private static long testLongMulReduction() {
        long acc = 1;
        for (int i = 0; i < SIZE; i++) {
            long val = LONG_A[i] * LONG_B[i];
            acc *= val;
        }
        return acc;
    }

    @Run(test = {"testIntMulReduction", "testLongMulReduction"})
    private void run() {
        Asserts.assertEquals(testIntMulReduction(), expectedIntMulReduction());
        Asserts.assertEquals(testLongMulReduction(), expectedLongMulReduction());
    }

    @DontCompile
    private static int expectedIntMulReduction() {
        int acc = 1;
        for (int i = 0; i < SIZE; i++) {
            int val = INT_A[i] * INT_B[i];
            acc *= val;
        }
        return acc;
    }

    @DontCompile
    private static long expectedLongMulReduction() {
        long acc = 1;
        for (int i = 0; i < SIZE; i++) {
            long val = LONG_A[i] * LONG_B[i];
            acc *= val;
        }
        return acc;
    }
}
