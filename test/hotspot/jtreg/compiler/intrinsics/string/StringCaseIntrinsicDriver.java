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

package compiler.intrinsics.string;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class StringCaseIntrinsicDriver {
    private static final String HISILICON_IMPLEMENTER = "0x48:";
    private static final Map<String, String> WRAPPER_INTRINSICS =
            wrapperIntrinsics();
    private static final WhiteBox WHITE_BOX = WhiteBox.getWhiteBox();

    private record Configuration(String name, int useSVE, int requestedBackend,
                                 int expectedBackend) {}

    public static void main(String[] args) throws Exception {
        String cpuFeatures = WHITE_BOX.getCPUFeatures();
        boolean isHiSilicon = cpuFeatures.startsWith(HISILICON_IMPLEMENTER);
        boolean supportsSVE = hasCPUFeature(cpuFeatures, "sve");
        boolean supportsSVE2 = hasCPUFeature(cpuFeatures, "sve2");
        boolean supportsSVEBitPerm = hasCPUFeature(cpuFeatures, "svebitperm");
        List<Configuration> configurations = new ArrayList<>();

        int offUseSVE = supportsSVE2 ? 2 : supportsSVE ? 1 : 0;
        configurations.add(new Configuration("off", offUseSVE, 0, 0));
        if (isHiSilicon && supportsSVE) {
            configurations.add(new Configuration("sve", 1, 1, 1));
        }

        if (isHiSilicon && supportsSVE2 && supportsSVEBitPerm) {
            configurations.add(new Configuration("sve2", 2, 2, 2));
        }

        System.out.println("CPU features: " + cpuFeatures);
        for (Configuration configuration : configurations) {
            runConfiguration(configuration);
        }
    }

    private static boolean hasCPUFeature(String cpuFeatures, String feature) {
        return Arrays.asList(cpuFeatures.split("[, ]+")).contains(feature);
    }

    private static Map<String, String> wrapperIntrinsics() {
        Map<String, String> result = new LinkedHashMap<>();
        result.put("latin1LowerWrapper", "_stringLatin1ToLowerCase");
        result.put("latin1UpperWrapper", "_stringLatin1ToUpperCase");
        result.put("utf16LowerWrapper", "_stringUTF16ToLowerCase");
        result.put("utf16UpperWrapper", "_stringUTF16ToUpperCase");
        return Map.copyOf(result);
    }

    private static void runConfiguration(Configuration configuration)
            throws Exception {
        Path logFile = Path.of("string-case-" + configuration.name() + ".log")
                .toAbsolutePath();
        Files.deleteIfExists(logFile);

        List<String> options = new ArrayList<>();
        options.add("-Xbootclasspath/a:.");
        options.add("--add-opens=java.base/java.lang=ALL-UNNAMED");
        options.add("--add-modules=jdk.management");
        options.add("-Xbatch");
        options.add("-XX:-TieredCompilation");
        options.add("-XX:CompileThreshold=1000");
        options.add("-XX:+CompactStrings");
        options.add("-XX:+UnlockDiagnosticVMOptions");
        options.add("-XX:+WhiteBoxAPI");
        options.add("-XX:+LogCompilation");
        options.add("-XX:LogFile=" + logFile);
        options.add("-XX:UseSVE=" + configuration.useSVE());
        options.add("-XX:StringCaseIntrinsicBackend="
                + configuration.requestedBackend());
        options.add("-XX:StringCaseIntrinsicMinLength=8");
        options.add("-DexpectedUseSVE=" + configuration.useSVE());
        options.add("-DexpectedBackend=" + configuration.expectedBackend());
        options.add("-DexpectedMinLength=8");
        for (String wrapper : WRAPPER_INTRINSICS.keySet()) {
            options.add("-XX:CompileCommand=compileonly,"
                    + TestStringCaseIntrinsic.class.getName() + "::" + wrapper);
        }
        options.add(TestStringCaseIntrinsic.class.getName());

        System.out.println("Running configuration: " + configuration.name());
        ProcessBuilder processBuilder =
                ProcessTools.createLimitedTestJavaProcessBuilder(options);
        OutputAnalyzer output = new OutputAnalyzer(processBuilder.start());
        output.shouldHaveExitValue(0);
        StringCaseIntrinsicLogVerifier.verify(
                logFile, TestStringCaseIntrinsic.class.getName(),
                WRAPPER_INTRINSICS, configuration.expectedBackend() != 0);
        Files.delete(logFile);
    }
}
