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
 * @summary Test the HiSilicon AArch64 intrinsic optimization umbrella flag.
 * @requires os.arch == "aarch64" & vm.flagless
 * @library /test/lib /
 * @modules java.management
 *
 * @build jdk.test.whitebox.WhiteBox
 * @build compiler.intrinsics.TestHisiIntrinsicOptimizationFlags
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver compiler.intrinsics.TestHisiIntrinsicOptimizationFlags
 */

package compiler.intrinsics;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

public class TestHisiIntrinsicOptimizationFlags {
    private static final String CLASS_NAME =
            TestHisiIntrinsicOptimizationFlags.class.getName();
    private static final String PROBE_ARG = "probe";
    private static final String UMBRELLA_FLAG = "UseHisiOptimizations";
    private static final String STRING_CASE_BACKEND_FLAG =
            "StringCaseIntrinsicBackend";
    private static final String[] CONTROLLED_FLAGS = {
            "UseSIMDForStringEquals",
            "UseUTFConversionIntrinsics",
            "UseStlrForRelease",
            "UseSVEHashCodeIntrinsic",
            "UseStreamPrefetchForArrayCopy",
            "UseSVESmallBlockZeroing",
            "UseLSEPrefetch"
    };
    private static final String[] ENABLE_ALL_FLAGS = {
            "-XX:-UseSimpleArrayEquals",
            "-XX:+UseSIMDForArrayEquals",
            "-XX:+UseSIMDForStringEquals",
            "-XX:+UseUTFConversionIntrinsics",
            "-XX:+UseStlrForRelease",
            "-XX:+UseVectorizedHashCodeIntrinsic",
            "-XX:+UseSVEHashCodeIntrinsic",
            "-XX:+UseStreamPrefetchForArrayCopy",
            "-XX:+UseSVESmallBlockZeroing",
            "-XX:+UseLSE",
            "-XX:+UseLSEPrefetch",
            "-XX:" + STRING_CASE_BACKEND_FLAG + "=3",
            "-XX:UseSVE=2"
    };
    private static final String[] ENABLE_DEPENDENCY_FLAGS = {
            "-XX:-UseSimpleArrayEquals",
            "-XX:+UseSIMDForArrayEquals",
            "-XX:+UseVectorizedHashCodeIntrinsic",
            "-XX:UseSVE=2"
    };
    private static final Pattern CPU_FEATURES_PATTERN = Pattern.compile(
            "(?m)^0x([0-9a-fA-F]+):0x[0-9a-fA-F]+:0x([0-9a-fA-F]+):\\d+(?:\\(0x([0-9a-fA-F]+)\\))?.*");
    private static final int HISILICON_IMPLEMENTER = 0x48;
    private static final int HISILICON_950_MODEL = 0xd06;

    public static void main(String[] args) throws Exception {
        if (args.length >= 1 && PROBE_ARG.equals(args[0])) {
            runProbe();
            return;
        }

        CpuInfo cpu = probeCpuInfo();
        checkDefaultFlags(cpu);
        checkIndividualFlagsWithoutUmbrella(cpu);
        checkUmbrellaDefaultFlags(cpu);
        checkUmbrellaWithExplicitLSE(cpu);
        checkUmbrellaAndIndividualFlags(cpu);
        checkUmbrellaRespectsExplicitStringCaseOff();
        checkStringEqualsArrayEqualsDependency();
        checkSVEHashCodeRequiresSVE2(cpu);
    }

    private static void runProbe() {
        System.out.println(WhiteBox.getWhiteBox().getCPUFeatures());
    }

    private static CpuInfo probeCpuInfo() throws Exception {
        ProcessBuilder pb = ProcessTools.createTestJavaProcessBuilder(
                "-Xbootclasspath/a:.",
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                CLASS_NAME,
                PROBE_ARG);
        OutputAnalyzer output = ProcessTools.executeProcess(pb);
        output.shouldHaveExitValue(0);
        return CpuInfo.parse(output.getOutput().trim());
    }

    private static void checkDefaultFlags(CpuInfo cpu) throws Exception {
        OutputAnalyzer output = runPrintFlags();
        expectFlag(output, UMBRELLA_FLAG, false, "default flags");
        for (String flag : CONTROLLED_FLAGS) {
            expectFlag(output, flag, false, "default flags");
        }
        expectUintFlag(output, STRING_CASE_BACKEND_FLAG, 0,
                "default flags: String case backend");
        expectIntFlag(output, "StringCaseIntrinsicMinLength", 8,
                "default flags: String case minimum length");
        expectFlag(output, "UseLSE", cpu.supportsFeature("lse"),
                "default flags: LSE flag");
    }

    private static void checkIndividualFlagsWithoutUmbrella(CpuInfo cpu) throws Exception {
        OutputAnalyzer output = runPrintFlags(ENABLE_ALL_FLAGS);

        expectFlag(output, UMBRELLA_FLAG, false, "individual flags only");
        if (cpu.isHiSilicon()) {
            checkControlledFlags(cpu, output, "individual flags only",
                    true /* individualFlagsSpecified */);
        } else {
            for (String flag : CONTROLLED_FLAGS) {
                expectFlag(output, flag, false, "individual flags only");
            }

            for (String flag : CONTROLLED_FLAGS) {
                expectDisabledWarning(output, flag, "individual flags only");
            }
            expectUintFlag(output, STRING_CASE_BACKEND_FLAG, 0,
                    "individual flags only: String case backend");
            expectDisabledWarning(output, STRING_CASE_BACKEND_FLAG,
                    "individual flags only");
        }
    }

    private static void checkUmbrellaDefaultFlags(CpuInfo cpu) throws Exception {
        List<String> flags = new ArrayList<>();
        flags.add("-XX:+" + UMBRELLA_FLAG);
        for (String flag : ENABLE_DEPENDENCY_FLAGS) {
            flags.add(flag);
        }

        OutputAnalyzer output = runPrintFlags(flags.toArray(new String[0]));
        checkEnabledFlags(cpu, output, "umbrella default flags",
                false /* individualFlagsSpecified */);
        expectFlag(output, "UseLSE",
                cpu.supportsFeature("lse") && !cpu.isHiSiliconBefore950(),
                "umbrella default flags: LSE flag");
        output.shouldNotContain(
                "UseLSE specified, but is not supported on this hardware. Disabling.");
    }

    private static void checkUmbrellaWithExplicitLSE(CpuInfo cpu) throws Exception {
        OutputAnalyzer output = runPrintFlags(
                "-XX:+" + UMBRELLA_FLAG,
                "-XX:+UseLSE");
        expectFlag(output, "UseLSE", cpu.supportsFeature("lse"),
                "umbrella with explicit LSE: LSE flag");
        expectFlag(output, "UseLSEPrefetch",
                cpu.isHiSiliconBefore950() && cpu.supportsFeature("lse"),
                "umbrella with explicit LSE");
    }

    private static void checkUmbrellaAndIndividualFlags(CpuInfo cpu) throws Exception {
        List<String> flags = new ArrayList<>();
        flags.add("-XX:+" + UMBRELLA_FLAG);
        for (String flag : ENABLE_ALL_FLAGS) {
            flags.add(flag);
        }

        OutputAnalyzer output = runPrintFlags(flags.toArray(new String[0]));
        checkEnabledFlags(cpu, output, "umbrella and individual flags",
                true /* individualFlagsSpecified */);
    }

    private static void checkUmbrellaRespectsExplicitStringCaseOff()
            throws Exception {
        OutputAnalyzer output = runPrintFlags(
                "-XX:+" + UMBRELLA_FLAG,
                "-XX:" + STRING_CASE_BACKEND_FLAG + "=0",
                "-XX:UseSVE=2");
        expectUintFlag(output, STRING_CASE_BACKEND_FLAG, 0,
                "umbrella with explicitly disabled String case backend");
    }

    private static void checkEnabledFlags(CpuInfo cpu, OutputAnalyzer output,
                                          String context,
                                          boolean individualFlagsSpecified) {
        expectFlag(output, UMBRELLA_FLAG, cpu.isHiSilicon(), context);
        checkControlledFlags(cpu, output, context, individualFlagsSpecified);
    }

    private static void checkControlledFlags(CpuInfo cpu, OutputAnalyzer output,
                                             String context,
                                             boolean individualFlagsSpecified) {
        boolean avoidUnalignedAccesses = flagValue(output, "AvoidUnalignedAccesses");
        long useSVE = uintFlagValue(output, "UseSVE");

        expectFlag(output, "UseSIMDForStringEquals", cpu.isHiSilicon(),
                context + ": string equals flag");
        expectFlag(output, "UseUTFConversionIntrinsics", cpu.isHiSilicon(),
                context + ": UTF conversion flag");
        expectFlag(output, "UseStlrForRelease", cpu.isHiSilicon(),
                context + ": stlr release flag");
        expectFlag(output, "UseSVEHashCodeIntrinsic",
                cpu.isHiSilicon() && useSVE >= 2,
                context + ": SVE hashCode flag");
        expectFlag(output, "UseStreamPrefetchForArrayCopy",
                cpu.isHiSilicon()
                        && (individualFlagsSpecified || cpu.isHiSilicon950())
                        && !avoidUnalignedAccesses,
                context + ": arraycopy prefetch flag");
        expectFlag(output, "UseSVESmallBlockZeroing",
                cpu.isHiSilicon()
                        && (individualFlagsSpecified || cpu.isHiSilicon950())
                        && useSVE > 0,
                context + ": SVE zeroing flag");
        expectFlag(output, "UseLSEPrefetch",
                individualFlagsSpecified && cpu.isHiSilicon(),
                context + ": LSE prefetch flag");
        expectUintFlag(output, STRING_CASE_BACKEND_FLAG,
                expectedStringCaseBackend(cpu, useSVE),
                context + ": String case backend");
    }

    private static long expectedStringCaseBackend(CpuInfo cpu, long useSVE) {
        if (!cpu.isHiSilicon() || useSVE < 1) {
            return 0;
        }
        return useSVE >= 2 && cpu.supportsFeature("svebitperm") ? 2 : 1;
    }

    private static void checkStringEqualsArrayEqualsDependency() throws Exception {
        OutputAnalyzer output = runPrintFlags(
                "-XX:+" + UMBRELLA_FLAG,
                "-XX:+UseSIMDForArrayEquals",
                "-XX:+UseSimpleArrayEquals",
                "-XX:+UseSIMDForStringEquals");
        expectFlag(output, "UseSIMDForStringEquals", false,
                "string equals array equals dependency");
    }

    private static void checkSVEHashCodeRequiresSVE2(CpuInfo cpu) throws Exception {
        OutputAnalyzer output = runPrintFlags(
                "-XX:+UseVectorizedHashCodeIntrinsic",
                "-XX:+UseSVEHashCodeIntrinsic",
                "-XX:UseSVE=1");
        expectFlag(output, "UseSVEHashCodeIntrinsic", false,
                "SVE hashCode SVE2 dependency");
        if (cpu.isHiSilicon() && cpu.supportsFeature("sve2")
                && uintFlagValue(output, "UseSVE") != 1) {
            throw new AssertionError("UseSVE was not 1 for SVE2 dependency test\n"
                    + output.getOutput());
        }
    }

    private static OutputAnalyzer runPrintFlags(String... flags) throws Exception {
        List<String> command = new ArrayList<>();
        command.add("-XX:+UnlockDiagnosticVMOptions");
        for (String flag : flags) {
            command.add(flag);
        }
        command.add("-XX:+PrintFlagsFinal");
        command.add("-version");

        ProcessBuilder pb = ProcessTools.createTestJavaProcessBuilder(
                command.toArray(new String[0]));
        OutputAnalyzer output = ProcessTools.executeProcess(pb);
        output.shouldHaveExitValue(0);
        return output;
    }

    private static void expectFlag(OutputAnalyzer output, String name, boolean expected,
                                   String context) {
        boolean actual = flagValue(output, name);
        if (actual != expected) {
            throw new AssertionError(name + " is " + actual + " but expected "
                    + expected + " for " + context + "\n" + output.getOutput());
        }
    }

    private static void expectDisabledWarning(OutputAnalyzer output, String name,
                                              String context) {
        String expected = name + " specified, but is not supported on this hardware. "
                + "Disabling.";
        if (!output.getOutput().contains(expected)) {
            throw new AssertionError("Could not find warning '" + expected
                    + "' for " + context + "\n" + output.getOutput());
        }
    }

    private static boolean flagValue(OutputAnalyzer output, String name) {
        String value = output.firstMatch(
                "(?m)^\\s*bool\\s+" + Pattern.quote(name)
                        + "\\s+:?=\\s+(true|false)\\b", 1);
        if (value == null) {
            throw new AssertionError("Could not find bool flag " + name
                    + " in PrintFlagsFinal output\n" + output.getOutput());
        }
        return Boolean.parseBoolean(value);
    }

    private static long uintFlagValue(OutputAnalyzer output, String name) {
        String value = output.firstMatch(
                "(?m)^\\s*uint\\s+" + Pattern.quote(name)
                        + "\\s+:?=\\s+([0-9]+)\\b", 1);
        if (value == null) {
            throw new AssertionError("Could not find uint flag " + name
                    + " in PrintFlagsFinal output\n" + output.getOutput());
        }
        return Long.parseLong(value);
    }

    private static void expectUintFlag(OutputAnalyzer output, String name,
                                       long expected, String context) {
        long actual = uintFlagValue(output, name);
        if (actual != expected) {
            throw new AssertionError(name + " is " + actual + " but expected "
                    + expected + " for " + context + "\n" + output.getOutput());
        }
    }

    private static void expectIntFlag(OutputAnalyzer output, String name,
                                      long expected, String context) {
        String value = output.firstMatch(
                "(?m)^\\s*int\\s+" + Pattern.quote(name)
                        + "\\s+:?=\\s+(-?[0-9]+)\\b", 1);
        if (value == null || Long.parseLong(value) != expected) {
            throw new AssertionError(name + " is " + value + " but expected "
                    + expected + " for " + context + "\n" + output.getOutput());
        }
    }

    private static class CpuInfo {
        private final String features;
        private final int implementer;
        private final int model;
        private final int model2;

        private CpuInfo(String features, int implementer, int model, int model2) {
            this.features = features;
            this.implementer = implementer;
            this.model = model;
            this.model2 = model2;
        }

        private static CpuInfo parse(String features) {
            Matcher matcher = CPU_FEATURES_PATTERN.matcher(features);
            if (!matcher.find()) {
                return new CpuInfo(features, -1, -1, -1);
            }

            int implementer = parseHex(matcher.group(1));
            int model = parseHex(matcher.group(2));
            int model2 = matcher.group(3) == null ? -1 : parseHex(matcher.group(3));
            return new CpuInfo(features, implementer, model, model2);
        }

        private static int parseHex(String hex) {
            return Integer.parseInt(hex, 16);
        }

        private boolean isHiSilicon() {
            return implementer == HISILICON_IMPLEMENTER;
        }

        private boolean isHiSilicon950() {
            return isHiSilicon()
                    && (model == HISILICON_950_MODEL || model2 == HISILICON_950_MODEL);
        }

        private boolean isHiSiliconBefore950() {
            return isHiSilicon()
                    && model < HISILICON_950_MODEL
                    && (model2 == -1 || model2 < HISILICON_950_MODEL);
        }

        private boolean supportsFeature(String feature) {
            return (", " + features + ",").contains(", " + feature + ",");
        }

    }
}
