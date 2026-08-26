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
 * @summary Test BytecodeEnhancement CDS dump and archived heap boundaries.
 * @requires os.arch == "aarch64"
 * @requires vm.cds
 * @requires vm.flagless
 * @library /test/lib
 * @modules jdk.zipfs
 * @run main CDSConfigurationTest
 */

import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.zip.CRC32;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class CDSConfigurationTest {
    private static final Path ROOT = Path.of(".").toAbsolutePath().normalize();

    public static void main(String[] args) throws Exception {
        if (args.length == 2 && args[0].equals("probe")) {
            String expected = args[1];
            try {
                new java.util.HashMap<>(-1);
                throw new RuntimeException("negative HashMap capacity unexpectedly accepted");
            } catch (IllegalArgumentException e) {
                if (!e.getMessage().startsWith(expected)) {
                    throw new RuntimeException("HashMap marker mismatch: " + e.getMessage());
                }
            }
            return;
        }
        Path hashMapEnhancement = createEnhancement("hashmap-enhancement", "java/util/HashMap.class");

        testStaticDump(hashMapEnhancement);
        testArchivedHeapIsSkipped(hashMapEnhancement);
        testVmClassStaticDumpIsRejected();
    }

    private static void testStaticDump(Path enhancementDir) throws Exception {
        Path archive = ROOT.resolve("bytecode-enhancement-static.jsa");
        OutputAnalyzer dump = java(
                "-Xshare:dump",
                "-XX:SharedArchiveFile=" + archive,
                "-XX:BytecodeEnhancementPaths=" + enhancementDir,
                "-Xlog:cds=info,class+load+enhancement=info");
        dump.shouldHaveExitValue(0)
            .shouldContain("Bytecode enhancement replacing class java/util/HashMap")
            .shouldContain("Skipping java/util/HashMap: Bytecode enhancement candidate")
            .shouldContain("Skipping java/util/LinkedHashMap: super class java/util/HashMap is excluded")
            .shouldNotContain("Shared file region (hp)");

        java("-Xshare:off",
             CDSConfigurationTest.class.getName(), "probe", "Illegal initial capacity:")
                .shouldHaveExitValue(0);
        java("-Xshare:on",
             "-XX:SharedArchiveFile=" + archive,
             CDSConfigurationTest.class.getName(), "probe", "Illegal initial capacity:")
                .shouldHaveExitValue(0);
        java("-Xshare:on",
             "-XX:SharedArchiveFile=" + archive,
             "-XX:BytecodeEnhancementPaths=" + enhancementDir,
             "-Xlog:class+load+enhancement=info",
             CDSConfigurationTest.class.getName(), "probe", "Updated initial capacity:")
                .shouldHaveExitValue(0)
                .shouldContain("Bytecode enhancement replacing class java/util/HashMap");
    }

    private static void testArchivedHeapIsSkipped(Path enhancementDir) throws Exception {
        Path archive = ROOT.resolve("bytecode-enhancement-with-heap.jsa");
        java("-Xshare:dump",
             "-XX:SharedArchiveFile=" + archive)
                .shouldHaveExitValue(0);

        java("-Xshare:on",
             "-XX:SharedArchiveFile=" + archive,
             "-XX:BytecodeEnhancementPaths=" + enhancementDir,
             "-Xlog:cds+heap=info,class+load+enhancement=info",
             CDSConfigurationTest.class.getName(), "probe", "Updated initial capacity:")
                .shouldHaveExitValue(0)
                .shouldContain("Archived heap is not used because BytecodeEnhancement is configured")
                .shouldContain("Bytecode enhancement replacing class java/util/HashMap");
    }

    private static void testVmClassStaticDumpIsRejected() throws Exception {
        Path enhancementDir = createEnhancement("string-enhancement", "java/lang/String.class");
        Path archive = ROOT.resolve("bytecode-enhancement-invalid-vm-class.jsa");
        java("-Xshare:dump",
             "-XX:SharedArchiveFile=" + archive,
             "-XX:BytecodeEnhancementPaths=" + enhancementDir,
             "-Xlog:class+load+enhancement=info")
                .shouldNotHaveExitValue(0)
                .shouldContain("Bytecode enhancement replacing class java/lang/String")
                .shouldContain("BytecodeEnhancement replacement of VM bootstrap class or supertype java/lang/String is not supported while using or creating a CDS archive");

        enhancementDir = createEnhancement("abstract-list-enhancement", "java/util/AbstractList.class");
        archive = ROOT.resolve("bytecode-enhancement-invalid-vm-supertype.jsa");
        java("-Xshare:dump",
             "-XX:SharedArchiveFile=" + archive,
             "-XX:BytecodeEnhancementPaths=" + enhancementDir,
             "-Xlog:class+load+enhancement=info")
                .shouldNotHaveExitValue(0)
                .shouldContain("Bytecode enhancement replacing class java/util/AbstractList")
                .shouldContain("BytecodeEnhancement replacement of VM bootstrap class or supertype java/util/AbstractList is not supported while using or creating a CDS archive");
    }

    private static Path createEnhancement(String directory, String classFile) throws IOException {
        byte[] original = readBaseModuleClass(classFile);
        byte[] enhanced = classFile.equals("java/util/HashMap.class")
                ? replaceOnce(original, "Illegal initial capacity:", "Updated initial capacity:")
                : original;
        Path enhancementDir = ROOT.resolve(directory);
        Path target = enhancementDir.resolve(classFile);
        Files.createDirectories(target.getParent());
        Files.write(target, enhanced);

        String binaryName = classFile.substring(0, classFile.length() - ".class".length());
        Files.writeString(enhancementDir.resolve("bytecode-enhancement.list"),
                "class-rep " + crc(original) + " " + binaryName + "\n");
        return enhancementDir;
    }

    private static byte[] replaceOnce(byte[] original, String fromString, String toString) {
        byte[] from = fromString.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1);
        byte[] to = toString.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1);
        if (from.length != to.length) {
            throw new IllegalArgumentException("replacement strings must have equal length");
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

    private static byte[] readBaseModuleClass(String name) throws IOException {
        Path jmod = Path.of(System.getProperty("java.home"), "jmods", "java.base.jmod");
        try (FileSystem fs = FileSystems.newFileSystem(jmod, Map.of())) {
            return Files.readAllBytes(fs.getPath("classes", name));
        }
    }

    private static long crc(byte[] bytes) {
        CRC32 crc = new CRC32();
        crc.update(bytes);
        return crc.getValue();
    }

    private static OutputAnalyzer java(String... args) throws Exception {
        return new OutputAnalyzer(ProcessTools.createTestJavaProcessBuilder(args).start());
    }
}
