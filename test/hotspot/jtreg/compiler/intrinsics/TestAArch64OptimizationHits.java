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

/**
 * @test
 * @summary Verify optional AArch64 optimization hit recording.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.flagless
 * @library /test/lib
 * @modules java.base/jdk.internal.util
 * @run driver compiler.intrinsics.TestAArch64OptimizationHits
 */

package compiler.intrinsics;

import jdk.internal.util.ArraysSupport;
import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class TestAArch64OptimizationHits {
    private static final String HEADER = "AArch64 optimization hit:";
    private static final String WORKLOAD = "workload";
    private static volatile int sink;

    public static void main(String[] args) throws Exception {
        if (args.length != 0 && WORKLOAD.equals(args[0])) {
            runWorkload();
            return;
        }

        OutputAnalyzer disabled = runChild(false);
        disabled.shouldHaveExitValue(0)
                .shouldContain("workload complete")
                .shouldNotContain(HEADER);

        OutputAnalyzer enabled = runChild(true);
        enabled.shouldHaveExitValue(0)
                .shouldContain("workload complete");
        String expectedHit = HEADER + " vectorized_hashcode_neon_byte";
        long hitReports = enabled.asLines().stream()
                .filter(expectedHit::equals)
                .count();
        if (hitReports != 1) {
            throw new RuntimeException("Expected one hit report, got " + hitReports);
        }
    }

    private static OutputAnalyzer runChild(boolean enablePrinting) throws Exception {
        String printingFlag = enablePrinting
                ? "-XX:+PrintAArch64OptimizationHits"
                : "-XX:-PrintAArch64OptimizationHits";
        return ProcessTools.executeTestJava(
                "--add-exports=java.base/jdk.internal.util=ALL-UNNAMED",
                "-Xbatch",
                "-XX:-TieredCompilation",
                "-XX:CompileThreshold=100",
                "-XX:+UnlockDiagnosticVMOptions",
                printingFlag,
                "-XX:+UseVectorizedHashCodeIntrinsic",
                "-XX:-UseSVEHashCodeIntrinsic",
                "-XX:CompileCommand=compileonly," +
                        "compiler.intrinsics.TestAArch64OptimizationHits::vectorHashCode",
                TestAArch64OptimizationHits.class.getName(),
                WORKLOAD);
    }

    private static void runWorkload() {
        byte[] value = new byte[1024];
        int result = 0;
        for (int i = 0; i < 20_000; i++) {
            value[i & (value.length - 1)] = (byte)i;
            result ^= vectorHashCode(value, i);
        }
        sink = result;
        System.out.println("workload complete: " + sink);
    }

    private static int vectorHashCode(byte[] value, int initialValue) {
        return ArraysSupport.vectorizedHashCode(value, 0, value.length,
                                                initialValue, ArraysSupport.T_BYTE);
    }
}
