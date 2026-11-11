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

import java.math.BigInteger;
import java.util.Random;

import compiler.lib.ir_framework.CompLevel;
import compiler.lib.ir_framework.IR;
import compiler.lib.ir_framework.IRNode;
import compiler.lib.ir_framework.Run;
import compiler.lib.ir_framework.RunInfo;
import compiler.lib.ir_framework.Test;
import compiler.lib.ir_framework.TestFramework;
import compiler.lib.ir_framework.Warmup;
import jdk.incubator.vector.LongVector;
import jdk.incubator.vector.VectorMask;
import jdk.incubator.vector.VectorSpecies;

/*
 * @test
 * @summary Preserve inactive lanes when reassociating long vector multiplies.
 * @requires vm.compiler2.enabled & os.arch == "aarch64" & vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @modules jdk.incubator.vector
 * @run driver compiler.c2.irTests.TestMaskedLongVectorMultiplyReassociation
 */

public class TestMaskedLongVectorMultiplyReassociation {
    private static final VectorSpecies<Long> SPECIES = LongVector.SPECIES_MAX;
    private static final int SIZE = 256;
    private static final long[] INPUT = new long[SIZE];
    private static final long[] OUTPUT = new long[SIZE];
    private static final long[] CUBE = new long[SIZE];
    private static final long[] FOURTH_POWER = new long[SIZE];
    private static final String[] MASK_NAMES = {
        "none", "all", "alternating", "first-lane", "last-lane", "random"
    };
    private static final boolean[][] MASKS = new boolean[MASK_NAMES.length][SIZE];

    static {
        Random random = new Random(0x13579bdf2468aceL);
        long[] boundaries = {
            2, 3, 5, -7, 11, -13, 0, 1, -1, Long.MIN_VALUE,
            Long.MIN_VALUE + 1, Long.MAX_VALUE, 0x100000001L, -0x100000003L
        };
        for (int i = 0; i < SIZE; i++) {
            INPUT[i] = i < boundaries.length ? boundaries[i] : random.nextLong();
            // Compute the oracle outside the tested C2 graph. longValue keeps
            // the low 64 bits, matching Java long multiplication on overflow.
            BigInteger value = BigInteger.valueOf(INPUT[i]);
            CUBE[i] = value.pow(3).longValue();
            FOURTH_POWER[i] = value.pow(4).longValue();
            MASKS[1][i] = true;
            MASKS[2][i] = (i & 1) == 0;
            MASKS[3][i] = i % SPECIES.length() == 0;
            MASKS[4][i] = i % SPECIES.length() == SPECIES.length() - 1;
            MASKS[5][i] = random.nextBoolean();
        }
    }

    public static void main(String[] args) {
        TestFramework.runWithFlags("--add-modules=jdk.incubator.vector",
                                   "-XX:UseSVE=1", "-XX:MaxVectorSize=32");
    }

    @Test(compLevel = CompLevel.C2)
    @IR(counts = {IRNode.MUL_VL, IRNode.VECTOR_SIZE_ANY, ">0"})
    public static void maskedChain(long[] input, boolean[] maskBits, long[] output) {
        for (int i = 0; i < input.length; i += SPECIES.length()) {
            LongVector value = LongVector.fromArray(SPECIES, input, i);
            VectorMask<Long> mask = VectorMask.fromArray(SPECIES, maskBits, i);
            value.mul(value, mask).mul(value).mul(value).intoArray(output, i);
        }
    }

    @Test(compLevel = CompLevel.C2)
    @IR(counts = {IRNode.MUL_VL, IRNode.VECTOR_SIZE_ANY, ">0"})
    public static void mixedSquares(long[] input, boolean[] maskBits, long[] output) {
        for (int i = 0; i < input.length; i += SPECIES.length()) {
            LongVector value = LongVector.fromArray(SPECIES, input, i);
            VectorMask<Long> mask = VectorMask.fromArray(SPECIES, maskBits, i);
            LongVector maskedSquare = value.mul(value, mask);
            LongVector square = value.mul(value);
            maskedSquare.mul(square).intoArray(output, i);
        }
    }

    @Run(test = "maskedChain")
    @Warmup(5000)
    public static void runMaskedChain(RunInfo info) {
        if (!info.isWarmUp()) {
            TestFramework.assertCompiledByC2(info.getTest());
        }
        for (int mask = 0; mask < MASKS.length; mask++) {
            maskedChain(INPUT, MASKS[mask], OUTPUT);
            check("maskedChain", mask);
        }
    }

    @Run(test = "mixedSquares")
    @Warmup(5000)
    public static void runMixedSquares(RunInfo info) {
        if (!info.isWarmUp()) {
            TestFramework.assertCompiledByC2(info.getTest());
        }
        for (int mask = 0; mask < MASKS.length; mask++) {
            mixedSquares(INPUT, MASKS[mask], OUTPUT);
            check("mixedSquares", mask);
        }
    }

    private static void check(String method, int mask) {
        for (int i = 0; i < SIZE; i++) {
            // Both expressions produce x^4 in active lanes. The first masked
            // multiply preserves x in inactive lanes, so those lanes yield x^3.
            long expected = MASKS[mask][i] ? FOURTH_POWER[i] : CUBE[i];
            if (OUTPUT[i] != expected) {
                throw new AssertionError(method + " mask=" + MASK_NAMES[mask]
                        + " index=" + i + " input=" + INPUT[i]
                        + " expected=" + expected + " actual=" + OUTPUT[i]);
            }
        }
    }
}
