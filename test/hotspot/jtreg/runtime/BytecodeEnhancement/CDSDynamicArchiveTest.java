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
 * @summary Test BytecodeEnhancement with classic dynamic CDS archives.
 * @requires os.arch == "aarch64"
 * @requires vm.cds
 * @library /test/lib /runtime/cds/appcds
 * @build jdk.test.whitebox.WhiteBox
 * @compile BytecodeEnhancementCDSApp.java
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver CDSDynamicArchiveTest
 */

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.CRC32;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class CDSDynamicArchiveTest {
    private static final Path TEST_CLASSES = Path.of(System.getProperty("test.classes"));
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
        String appJar = JarBuilder.build("bytecode-enhancement-dynamic-cds", APP_CLASSES);
        Path enhancementDir = createEnhancement();

        String baseArchive = TestCommon.getNewArchiveName("bytecode-enhancement-base");
        String normalTop = TestCommon.getNewArchiveName("bytecode-enhancement-normal-top");
        String enhancedTop = TestCommon.getNewArchiveName("bytecode-enhancement-enhanced-top");
        TestCommon.dumpBaseArchive(baseArchive)
                  .shouldHaveExitValue(0);

        dumpTop(baseArchive, normalTop, whiteBoxJar, appJar, null,
                "dynamic-dump-baseline");
        dumpTop(baseArchive, enhancedTop, whiteBoxJar, appJar, enhancementDir,
                "dynamic-dump-enhanced");

        runTop(baseArchive, normalTop, whiteBoxJar, appJar, null, "baseline");
        runTop(baseArchive, normalTop, whiteBoxJar, appJar, enhancementDir, "enhanced");
        runTop(baseArchive, enhancedTop, whiteBoxJar, appJar, null, "partial-baseline");
        runTop(baseArchive, enhancedTop, whiteBoxJar, appJar, enhancementDir, "partial-enhanced");
    }

    private static void dumpTop(String baseArchive, String topArchive,
                                String whiteBoxJar, String appJar,
                                Path enhancementDir, String mode) throws Exception {
        OutputAnalyzer output = run(baseArchive, null, whiteBoxJar, appJar,
                                    enhancementDir,
                                    mode,
                                    "-XX:ArchiveClassesAtExit=" + topArchive,
                                    "-Xlog:cds+dynamic=info,class+load+enhancement=info");
        output.shouldHaveExitValue(0)
              .shouldContain("CDS-COMPAT:" + mode + ":PASS")
              .shouldContain("Written dynamic archive");
        if (enhancementDir != null) {
            output.shouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSOuter")
                  .shouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSInterface")
                  .shouldContain("Bytecode enhancement adding class BytecodeEnhancementCDSAdded");
        }
    }

    private static void runTop(String baseArchive, String topArchive,
                               String whiteBoxJar, String appJar,
                               Path enhancementDir, String mode) throws Exception {
        OutputAnalyzer output = run(baseArchive, topArchive, whiteBoxJar, appJar,
                                    enhancementDir,
                                    mode,
                                    "-Xlog:class+load+enhancement=info");
        output.shouldHaveExitValue(0)
              .shouldContain("CDS-COMPAT:" + mode + ":PASS");
        if (enhancementDir != null) {
            output.shouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSOuter")
                  .shouldContain("Bytecode enhancement replacing class BytecodeEnhancementCDSInterface")
                  .shouldContain("Bytecode enhancement adding class BytecodeEnhancementCDSAdded");
        }
    }

    private static OutputAnalyzer run(String baseArchive, String topArchive,
                                      String whiteBoxJar, String appJar,
                                      Path enhancementDir, String mode,
                                      String... extraOptions) throws Exception {
        String archive = topArchive == null
                ? baseArchive
                : baseArchive + File.pathSeparator + topArchive;
        var command = new java.util.ArrayList<String>();
        command.add("-Xshare:on");
        command.add("-XX:SharedArchiveFile=" + archive);
        command.add("-Xbootclasspath/a:" + whiteBoxJar);
        command.add("-XX:+UnlockDiagnosticVMOptions");
        command.add("-XX:+WhiteBoxAPI");
        if (enhancementDir != null) {
            command.add("-XX:BytecodeEnhancementPaths=" + enhancementDir);
        }
        command.addAll(java.util.List.of(extraOptions));
        command.add("-cp");
        command.add(appJar);
        command.add("BytecodeEnhancementCDSApp");
        command.add(mode);
        return new OutputAnalyzer(ProcessTools.createTestJavaProcessBuilder(command).start());
    }

    private static Path createEnhancement() throws Exception {
        Path enhancementDir = Path.of("bytecode-enhancement-dynamic-enhancement").toAbsolutePath().normalize();
        Files.createDirectories(enhancementDir);

        byte[] outer = readClass("BytecodeEnhancementCDSOuter");
        byte[] intf = readClass("BytecodeEnhancementCDSInterface");
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSOuter.class"),
                    replaceOnce(outer, "OUTER-OLD", "OUTER-NEW"));
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSInterface.class"),
                    replaceOnce(intf, "IFACE-OLD", "IFACE-NEW"));
        Files.write(enhancementDir.resolve("BytecodeEnhancementCDSAdded.class"),
                    readClass("BytecodeEnhancementCDSAdded"));
        Files.writeString(enhancementDir.resolve("bytecode-enhancement.list"),
                "class-rep " + crc(outer) + " BytecodeEnhancementCDSOuter\n" +
                "class-rep " + crc(intf) + " BytecodeEnhancementCDSInterface\n" +
                "class-add BytecodeEnhancementCDSAdded owner-loader=app\n");
        return enhancementDir;
    }

    private static byte[] readClass(String binaryName) throws Exception {
        return Files.readAllBytes(TEST_CLASSES.resolve(binaryName + ".class"));
    }

    private static byte[] replaceOnce(byte[] original, String fromString, String toString) {
        byte[] from = fromString.getBytes(StandardCharsets.ISO_8859_1);
        byte[] to = toString.getBytes(StandardCharsets.ISO_8859_1);
        if (from.length != to.length) {
            throw new IllegalArgumentException("replacement must preserve class-file length");
        }

        byte[] result = original.clone();
        int match = -1;
        for (int i = 0; i <= result.length - from.length; i++) {
            boolean equal = true;
            for (int j = 0; j < from.length; j++) {
                if (result[i + j] != from[j]) {
                    equal = false;
                    break;
                }
            }
            if (equal) {
                if (match != -1) {
                    throw new RuntimeException("multiple occurrences of " + fromString);
                }
                match = i;
            }
        }
        if (match == -1) {
            throw new RuntimeException("missing class-file string " + fromString);
        }
        System.arraycopy(to, 0, result, match, to.length);
        return result;
    }

    private static long crc(byte[] bytes) {
        CRC32 crc = new CRC32();
        crc.update(bytes);
        return crc.getValue();
    }
}
