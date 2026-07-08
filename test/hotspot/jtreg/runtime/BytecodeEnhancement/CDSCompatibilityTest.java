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
 *
 */

/*
 * @test
 * @summary Verify BytecodeEnhancement class-level fallback with classic AppCDS.
 * @requires os.arch == "aarch64"
 * @requires vm.cds
 * @library /test/lib /runtime/cds/appcds
 * @build jdk.test.whitebox.WhiteBox
 * @compile BytecodeEnhancementCDSApp.java
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver CDSCompatibilityTest
 */

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.CRC32;

import jdk.test.lib.process.OutputAnalyzer;

public class CDSCompatibilityTest {
    private static final Path TEST_CLASSES = Path.of(System.getProperty("test.classes", "."));

    private static final String[] APP_CLASSES = {
        "BytecodeEnhancementCDSApp",
        "BytecodeEnhancementCDSOuter",
        "BytecodeEnhancementCDSOuter$Inner",
        "BytecodeEnhancementCDSClient",
        "BytecodeEnhancementCDSSub",
        "BytecodeEnhancementCDSInterface",
        "BytecodeEnhancementCDSImpl"
    };

    public static void main(String[] args) throws Exception {
        String whiteBoxJar = JarBuilder.build(true, "WhiteBox", "jdk/test/whitebox/WhiteBox");
        String useWhiteBox = "-Xbootclasspath/a:" + whiteBoxJar;
        String appJar = JarBuilder.build("bytecode-enhancement-cds", APP_CLASSES);

        Path enhancementDir = Path.of("bytecode-enhancement-cds-enhancement").toAbsolutePath().normalize();
        Files.createDirectories(enhancementDir);

        byte[] outerOriginal = readClass("BytecodeEnhancementCDSOuter");
        byte[] interfaceOriginal = readClass("BytecodeEnhancementCDSInterface");
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSOuter.class"),
                    replaceOnce(outerOriginal, "OUTER-OLD", "OUTER-NEW"));
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSInterface.class"),
                    replaceOnce(interfaceOriginal, "IFACE-OLD", "IFACE-NEW"));
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSAdded.class"),
                    readClass("BytecodeEnhancementCDSAdded"));
        Files.writeString(enhancementDir.resolve("bytecode-enhancement.list"),
                "class-rep " + crc(outerOriginal) + " BytecodeEnhancementCDSOuter\n" +
                "class-rep " + crc(interfaceOriginal) + " BytecodeEnhancementCDSInterface\n" +
                "class-add BytecodeEnhancementCDSAdded owner-loader=app\n");

        TestCommon.dump(appJar, APP_CLASSES, useWhiteBox);

        OutputAnalyzer baseline = TestCommon.exec(appJar,
                useWhiteBox,
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                "-Xlog:class+load=info",
                "BytecodeEnhancementCDSApp",
                "baseline");
        TestCommon.checkExec(baseline);
        baseline.stdoutShouldContain("CDS-COMPAT:baseline:PASS");

        OutputAnalyzer enhanced = TestCommon.exec(appJar,
                useWhiteBox,
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                "-XX:BytecodeEnhancementPaths=" + enhancementDir,
                "-Xlog:class+load=info,class+load+enhancement=debug",
                "BytecodeEnhancementCDSApp",
                "enhanced");
        TestCommon.checkExec(enhanced);
        enhanced.stdoutShouldContain("CDS-COMPAT:enhanced:PASS")
               .stdoutShouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSOuter")
               .stdoutShouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSInterface")
               .stdoutShouldContain("Bytecode enhancement adding class BytecodeEnhancementCDSAdded");

        OutputAnalyzer sharingOff = TestCommon.exec(appJar,
                useWhiteBox,
                "-Xshare:off",
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                "-XX:BytecodeEnhancementPaths=" + enhancementDir,
                "BytecodeEnhancementCDSApp",
                "sharing-off");
        sharingOff.shouldHaveExitValue(0)
                  .stdoutShouldContain("CDS-COMPAT:sharing-off:PASS");
    }

    private static byte[] readClass(String binaryName) throws Exception {
        return Files.readAllBytes(TEST_CLASSES.resolve(binaryName + ".class"));
    }

    private static byte[] replaceOnce(byte[] original, String fromString, String toString) {
        byte[] from = fromString.getBytes(StandardCharsets.ISO_8859_1);
        byte[] to = toString.getBytes(StandardCharsets.ISO_8859_1);
        if (from.length != to.length) {
            throw new IllegalArgumentException("replacement strings must have equal length");
        }

        byte[] result = original.clone();
        int found = -1;
        for (int i = 0; i <= result.length - from.length; i++) {
            boolean match = true;
            for (int j = 0; j < from.length; j++) {
                if (result[i + j] != from[j]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                if (found != -1) {
                    throw new RuntimeException("class marker occurs more than once: " + fromString);
                }
                found = i;
            }
        }
        if (found == -1) {
            throw new RuntimeException("class marker not found: " + fromString);
        }
        System.arraycopy(to, 0, result, found, to.length);
        return result;
    }

    private static String crc(byte[] bytes) {
        CRC32 crc = new CRC32();
        crc.update(bytes);
        return Long.toUnsignedString(crc.getValue());
    }
}
