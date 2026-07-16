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
 * @summary AArch64 String case intrinsic flags are isolated and reject unsupported backends
 * @requires os.arch == "aarch64" | os.arch == "amd64" | os.arch == "x86_64"
 * @requires vm.flagless
 * @library /test/lib
 * @modules java.base/jdk.internal.misc
 *          java.management
 * @build jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions
 *      -XX:+WhiteBoxAPI TestAArch64StringCaseFlags
 */

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

import java.util.Arrays;

public class TestAArch64StringCaseFlags {
    public static void main(String[] args) throws Exception {
        if (System.getProperty("os.arch").equals("aarch64")) {
            String cpuFeatures = WhiteBox.getWhiteBox().getCPUFeatures();
            boolean isHiSilicon = cpuFeatures.startsWith("0x48:");
            boolean supportsSVE = Arrays.asList(
                    cpuFeatures.split("[, ]+")).contains("sve");
            boolean supportsSVE2 = Arrays.asList(
                    cpuFeatures.split("[, ]+")).contains("sve2");
            boolean supportsSVEBitPerm = Arrays.asList(
                    cpuFeatures.split("[, ]+")).contains("svebitperm");
            testAArch64BackendGating(isHiSilicon, supportsSVE,
                    supportsSVE2, supportsSVEBitPerm);
            assertEffectiveMinLength(0);
            assertEffectiveMinLength(8);
            assertEffectiveMinLength(32);
            return;
        }
        assertUnrecognized("StringCaseIntrinsicBackend=1");
        assertUnrecognized("StringCaseIntrinsicMinLength=8");
    }

    private static void testAArch64BackendGating(boolean isHiSilicon,
                                                 boolean supportsSVE,
                                                 boolean supportsSVE2,
                                                 boolean supportsSVEBitPerm)
            throws Exception {
        assertEffectiveBackend(0, 0, 0, false);
        assertEffectiveBackend(0, 3, 0, false);
        assertEffectiveBackend(0, 1, 0, true);
        assertEffectiveBackend(0, 2, 0, true);
        assertEffectiveBackend(1, 2, 0, true);
        boolean sveSupported = isHiSilicon && supportsSVE;
        assertEffectiveBackend(1, 1, sveSupported ? 1 : 0,
                !sveSupported);
        assertEffectiveBackend(1, 3, sveSupported ? 1 : 0, false);
        int autoBackend = isHiSilicon && supportsSVE
                ? (supportsSVE2 && supportsSVEBitPerm ? 2 : 1)
                : 0;
        assertEffectiveBackend(2, 3, autoBackend, false);
        boolean sve2Supported = isHiSilicon && supportsSVE2
                && supportsSVEBitPerm;
        assertEffectiveBackend(2, 2, sve2Supported ? 2 : 0,
                !sve2Supported);
    }

    private static void assertEffectiveBackend(int useSVE, int requestedBackend,
                                               int expectedBackend,
                                               boolean expectDisableWarning)
            throws Exception {
        ProcessBuilder pb = ProcessTools.createLimitedTestJavaProcessBuilder(
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:UseSVE=" + useSVE,
                "-XX:StringCaseIntrinsicBackend=" + requestedBackend,
                "-XX:+PrintFlagsFinal",
                "-version");
        OutputAnalyzer output = new OutputAnalyzer(pb.start());
        output.shouldHaveExitValue(0);
        output.shouldMatch("(?m)^\\s*uint\\s+StringCaseIntrinsicBackend\\s+=\\s+"
                + expectedBackend + "\\s+.*$");
        if (expectDisableWarning) {
            output.stderrShouldMatch("StringCaseIntrinsicBackend[^\\r\\n]*"
                    + "(not supported on this hardware|requires "
                    + "(?:UseSVE >=|SVEBitPerm))");
        }
    }

    private static void assertEffectiveMinLength(int minLength)
            throws Exception {
        ProcessBuilder pb = ProcessTools.createLimitedTestJavaProcessBuilder(
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:StringCaseIntrinsicBackend=0",
                "-XX:StringCaseIntrinsicMinLength=" + minLength,
                "-XX:+PrintFlagsFinal",
                "-version");
        OutputAnalyzer output = new OutputAnalyzer(pb.start());
        output.shouldHaveExitValue(0);
        output.shouldMatch("(?m)^\\s*int\\s+StringCaseIntrinsicMinLength\\s+=\\s+"
                + minLength + "\\s+.*$");
    }

    private static void assertUnrecognized(String option) throws Exception {
        ProcessBuilder pb = ProcessTools.createLimitedTestJavaProcessBuilder(
                "-XX:+UnlockDiagnosticVMOptions", "-XX:" + option, "-version");
        OutputAnalyzer output = new OutputAnalyzer(pb.start());
        output.shouldContain("Unrecognized VM option '" + option + "'");
        output.shouldHaveExitValue(1);
    }
}
