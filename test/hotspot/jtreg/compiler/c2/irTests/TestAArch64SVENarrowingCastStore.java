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
 * @summary Test AArch64 SVE fused narrowing cast-store matcher rules.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @run driver compiler.c2.irTests.TestAArch64SVENarrowingCastStore
 */

public class TestAArch64SVENarrowingCastStore {
    private static final int SIZE = 3000;
    private static final int OFFSET = 3;

    private static final double[] DOUBLES = new double[SIZE];
    private static final long[] LONGS = new long[SIZE];
    private static final short[] SHORTS = new short[SIZE + OFFSET];
    private static final float[] FLOATS = new float[SIZE + OFFSET];
    private static final byte[] BYTES = new byte[SIZE + OFFSET];

    public static void main(String[] args) {
        for (int i = 0; i < SIZE; i++) {
            DOUBLES[i] = ((i & 1) == 0 ? i * 17.25 : -i * 19.75) + 0.5;
            LONGS[i] = i * 0x1234_5678L - 0x7654_3210L;
        }
        TestFramework.runWithFlags("-XX:UseSVE=1", "-XX:MaxVectorSize=32");
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"storeV_vcastDtoS_sve8", ">0"},
        phase = CompilePhase.MATCHING)
    private static void testD2S() {
        for (int i = 0; i < SIZE; i++) {
            SHORTS[i + OFFSET] = (short) DOUBLES[i];
        }
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"storeV_vcastDtoF_sve16", ">0"},
        phase = CompilePhase.MATCHING)
    private static void testD2F() {
        for (int i = 0; i < SIZE; i++) {
            FLOATS[i + OFFSET] = (float) DOUBLES[i];
        }
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"storeV_vcastLtoF_sve16", ">0"},
        phase = CompilePhase.MATCHING)
    private static void testL2F() {
        for (int i = 0; i < SIZE; i++) {
            FLOATS[i + OFFSET] = (float) LONGS[i];
        }
    }

    @Test
    @IR(applyIfAnd = {"UseSVE", "> 0", "MaxVectorSize", "32"},
        counts = {"storeV_vcastLtoB_sve4", ">0"},
        phase = CompilePhase.MATCHING)
    private static void testL2B() {
        for (int i = 0; i < SIZE; i++) {
            BYTES[i + OFFSET] = (byte) LONGS[i];
        }
    }

    @Run(test = {"testD2S", "testD2F", "testL2F", "testL2B"})
    private void run() {
        testD2S();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(SHORTS[i + OFFSET], (short) DOUBLES[i]);
        }

        testD2F();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(Float.floatToRawIntBits(FLOATS[i + OFFSET]),
                                 Float.floatToRawIntBits((float) DOUBLES[i]));
        }

        testL2F();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(Float.floatToRawIntBits(FLOATS[i + OFFSET]),
                                 Float.floatToRawIntBits((float) LONGS[i]));
        }

        testL2B();
        for (int i = 0; i < SIZE; i++) {
            Asserts.assertEquals(BYTES[i + OFFSET], (byte) LONGS[i]);
        }
    }
}
