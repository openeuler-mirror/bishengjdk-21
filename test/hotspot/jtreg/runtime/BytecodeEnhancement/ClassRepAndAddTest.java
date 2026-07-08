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
 * @summary Test BytecodeEnhancement class-rep and built-in loader class-add support.
 * @requires os.arch == "aarch64"
 * @requires vm.cds
 * @library /test/lib
 * @modules jdk.compiler jdk.zipfs java.sql
 * @run main ClassRepAndAddTest
 */

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;
import java.util.zip.CRC32;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class ClassRepAndAddTest {
    private static final JavaCompiler JAVAC = ToolProvider.getSystemJavaCompiler();
    private static final Path ROOT = Path.of(".").toAbsolutePath().normalize();

    public static void main(String[] args) throws Exception {
        if (JAVAC == null) {
            throw new RuntimeException("No system Java compiler available");
        }

        testClassRepWithOwnerLoaderAdd();
        testClassRepWithOwnerClassAdd();
        testCustomLoaderOwnerClassAddIsUnsupported();
        testClassRepAndAddFromZipPathList();
        testCrcMismatchSkipsReplacement();
        testExitOnCrcMismatch();
        testMissingReplacementSkipsReplacement();
        testExitOnMissingReplacement();
        testExitOnExistingClassAddConflict();
        testExistingClassAddWarns();
        testDuplicateActionsAreRejected();
        testPlatformLoaderClassRepAndAdd();
        testWellKnownClassReplacementWithAndWithoutCDS();
        testVmClassDependencyReplacementWithAndWithoutCDS();
        testClassRepAndAddMatrixAcrossLoadersAndPaths();
    }

    private static void testClassRepWithOwnerLoaderAdd() throws Exception {
        TestCase tc = new TestCase("owner_loader", "com.test.ownerloader", "old", "new");
        tc.compileOriginal();
        tc.compileEnhancement(true);
        tc.installEnhancementClass("Target");
        tc.installEnhancementClass("Added");
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n" +
                     "class-add " + tc.pkg + ".Added owner-loader=app\n");

        runMain(tc).shouldHaveExitValue(0).stdoutShouldContain("old");
        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("new")
                .stdoutShouldContain(tc.enhancementDirUrl())
                .shouldNotContain("Bytecode enhancement skipped adding class");
    }

    private static void testClassRepWithOwnerClassAdd() throws Exception {
        TestCase tc = new TestCase("owner_class", "com.test.ownerclass", "old", "owner-new");
        tc.compileOriginal();
        tc.compileEnhancement(true);
        tc.installEnhancementClass("Target");
        tc.installEnhancementClass("Added");
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n" +
                     "class-add " + tc.pkg + ".Added owner-class=" + tc.pkg + ".Target\n");

        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("owner-new")
                .stdoutShouldContain(tc.enhancementDirUrl())
                .shouldNotContain("Bytecode enhancement skipped adding class");
    }

    private static void testCustomLoaderOwnerClassAddIsUnsupported() throws Exception {
        TestCase tc = new TestCase("custom_owner", "com.test.customowner", "old", "custom-new");
        tc.compileOriginal();
        tc.compileEnhancement(false);
        tc.installEnhancementClass("Target");
        String addedName = tc.pkg + ".Added";
        writeClassFile(tc.enhancementDir, addedName.replace('.', '/'),
                       minimalPublicClass(addedName.replace('.', '/')));
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n" +
                     "class-add " + addedName + " owner-class=" + tc.pkg + ".Target\n");
        tc.writeSource(tc.originalSrc, "Main", """
                package %s;
                import java.nio.file.Files;
                import java.nio.file.Path;
                public class Main {
                    private static final String TARGET = "%s.Target";
                    private static final String ADDED = "%s";
                    private static final class TargetLoader extends ClassLoader {
                        TargetLoader() {
                            super(ClassLoader.getSystemClassLoader());
                        }
                        Class<?> defineTarget(byte[] bytes) {
                            return defineClass(TARGET, bytes, 0, bytes.length);
                        }
                    }
                    public static void main(String[] args) throws Exception {
                        TargetLoader loader = new TargetLoader();
                        Class<?> target = loader.defineTarget(
                                Files.readAllBytes(Path.of(System.getProperty("target.class"))));
                        if (target.getClassLoader() != loader) {
                            throw new RuntimeException("owner was not defined by the custom loader");
                        }
                        System.out.println(target.getMethod("message").invoke(null));
                        assertNotFound("app", ClassLoader.getSystemClassLoader());
                        assertNotFound("custom", loader);
                    }
                    private static void assertNotFound(String kind, ClassLoader loader) throws Exception {
                        try {
                            Class.forName(ADDED, true, loader);
                            throw new RuntimeException("class-add unexpectedly succeeded for " + kind + " loader");
                        } catch (ClassNotFoundException expected) {
                            System.out.println(kind + "-add-not-found");
                        }
                    }
                }
                """.formatted(tc.pkg, tc.pkg, addedName));
        compile(tc.originalClasses, tc.sourcePath(tc.originalSrc, "Main"));

        String ownerName = tc.pkg.replace('.', '/') + "/Target";
        runMain(tc,
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Dtarget.class=" + tc.classFile(tc.originalClasses, "Target"),
                "-Xlog:class+load+enhancement=info")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("custom-new")
                .stdoutShouldContain("app-add-not-found")
                .stdoutShouldContain("custom-add-not-found")
                .shouldContain("Bytecode enhancement class-add for owner class " + ownerName +
                               " is not supported with a custom class loader")
                .shouldContain("Bytecode enhancement replacing class " + ownerName)
                .shouldNotContain("Bytecode enhancement adding class " + addedName.replace('.', '/'));
    }

    private static void testClassRepAndAddFromZipPathList() throws Exception {
        TestCase tc = new TestCase("zip_path_list", "com.test.zippathlist", "old", "zip-new");
        tc.compileOriginal();
        tc.compileEnhancement(true);
        tc.installEnhancementClass("Target");
        tc.installEnhancementClass("Added");
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n" +
                     "class-add " + tc.pkg + ".Added owner-loader=app\n");
        Path enhancementZip = tc.createEnhancementZip();

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + tc.root.resolve("missing") + File.pathSeparator + enhancementZip)
                .shouldHaveExitValue(0)
                .stdoutShouldContain("zip-new")
                .stdoutShouldContain(enhancementZip.toUri().toURL().toString());
    }

    private static void testCrcMismatchSkipsReplacement() throws Exception {
        TestCase tc = new TestCase("crc_mismatch", "com.test.crcmismatch", "old", "new");
        tc.compileOriginal();
        tc.compileEnhancement(false);
        tc.installEnhancementClass("Target");
        tc.writeList("class-rep 0x12345678 " + tc.pkg + ".Target\n");

        runMain(tc,
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("old")
                .shouldContain("Bytecode enhancement skipped for " + tc.pkg.replace('.', '/') + "/Target: CRC32 mismatch")
                .shouldNotContain("Bytecode enhancement replacing class " + tc.pkg.replace('.', '/') + "/Target");
    }

    private static void testExitOnCrcMismatch() throws Exception {
        TestCase tc = new TestCase("exit_crc_mismatch", "com.test.exitcrcmismatch", "old", "new");
        tc.compileOriginal();
        tc.compileEnhancement(false);
        tc.installEnhancementClass("Target");
        tc.writeList("class-rep 0x12345678 " + tc.pkg + ".Target\n");

        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir)
                .shouldNotHaveExitValue(0)
                .shouldContain("CRC32 mismatch");
    }

    private static void testMissingReplacementSkipsReplacement() throws Exception {
        TestCase tc = new TestCase("missing_replacement", "com.test.missingreplacement", "old", "new");
        tc.compileOriginal();
        tc.compileEnhancement(false);
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n");

        runMain(tc,
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("old")
                .shouldContain("Bytecode enhancement class file not found for " + tc.pkg.replace('.', '/') + "/Target")
                .shouldNotContain("Bytecode enhancement replacing class " + tc.pkg.replace('.', '/') + "/Target");
    }

    private static void testExitOnMissingReplacement() throws Exception {
        TestCase tc = new TestCase("exit_missing_replacement", "com.test.exitmissingreplacement", "old", "new");
        tc.compileOriginal();
        tc.compileEnhancement(false);
        tc.writeList("class-rep " + tc.originalCrc("Target") + " " + tc.pkg + ".Target\n");

        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir)
                .shouldNotHaveExitValue(0)
                .shouldContain("Bytecode enhancement class file not found");
    }

    private static void testExitOnExistingClassAddConflict() throws Exception {
        TestCase tc = new TestCase("exit_existing_add", "com.test.exitexistingadd", "old", "unused");
        tc.compileOriginal();
        tc.writeList("class-add " + tc.pkg + ".Target owner-loader=app\n");

        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir)
                .shouldNotHaveExitValue(0)
                .shouldContain("Bytecode enhancement class-add ignored for " + tc.pkg.replace('.', '/') +
                               "/Target because the class already exists")
                .shouldNotContain("requested by boot loader");
    }

    private static void testExistingClassAddWarns() throws Exception {
        TestCase tc = new TestCase("existing_add", "com.test.existingadd", "old", "unused");
        tc.compileOriginal();
        tc.writeList("class-add " + tc.pkg + ".Target owner-loader=app\n");

        runMain(tc,
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("old")
                .shouldContain("Bytecode enhancement class-add ignored for " + tc.pkg.replace('.', '/') +
                               "/Target because the class already exists")
                .shouldNotContain("Bytecode enhancement adding class");
    }

    private static void testDuplicateActionsAreRejected() throws Exception {
        assertDuplicateActionsRejected("duplicate_rep", """
                class-rep 0 com.test.Duplicate
                class-rep 0 com.test.Duplicate
                """);
        assertDuplicateActionsRejected("duplicate_add", """
                class-add com.test.Duplicate owner-loader=app
                class-add com.test.Duplicate owner-loader=app
                """);
        assertDuplicateActionsRejected("duplicate_rep_add", """
                class-rep 0 com.test.Duplicate
                class-add com.test.Duplicate owner-loader=app
                """);
    }

    private static void assertDuplicateActionsRejected(String name, String list) throws Exception {
        TestCase tc = new TestCase(name, "com.test.duplicate", "unused", "unused");
        tc.writeList(list);
        new OutputAnalyzer(ProcessTools.createTestJavaProcessBuilder(
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-version").start())
                .shouldNotHaveExitValue(0)
                .shouldContain("Duplicate BytecodeEnhancement action for class com/test/Duplicate");
    }




    private static void testPlatformLoaderClassRepAndAdd() throws Exception {
        TestCase tc = new TestCase("platform_loader", "com.test.platformloader", "unused", "unused");
        byte[] original = readModuleClass("java.sql", "java/sql/SQLWarning.class");
        String owner = "java.sql.SQLWarning";
        String inner = owner + "$Inner";
        String innerInternal = inner.replace('.', '/');
        writeClassFile(tc.enhancementDir, "java/sql/SQLWarning",
                       addPublicNewInnerMethod(original, "bytecodeEnhancementNewInner", innerInternal));
        writeClassFile(tc.enhancementDir, innerInternal, minimalPublicClass(innerInternal));
        tc.writeList("class-rep " + crc(original) + " " + owner + "\n"
                   + "class-add " + inner + " owner-loader=platform\n");
        tc.writeSource(tc.originalSrc, "Main", """
                package %s;
                import java.sql.SQLWarning;
                public class Main {
                    public static void main(String[] args) throws Exception {
                        Class<?> owner = SQLWarning.class;
                        Object inner = owner.getMethod("bytecodeEnhancementNewInner")
                                .invoke(owner.getConstructor().newInstance());
                        ClassLoader platform = ClassLoader.getPlatformClassLoader();
                        if (owner.getClassLoader() != platform
                                || inner.getClass().getClassLoader() != platform) {
                            throw new RuntimeException("owner/add class was not defined by platform loader");
                        }
                        System.out.println("platform-loader:" + inner.getClass().getName());
                    }
                }
                """.formatted(tc.pkg));
        compile(tc.originalClasses, tc.sourcePath(tc.originalSrc, "Main"));
        runMain(tc,
                "-XX:+ExitOnBytecodeEnhancementFailure",
                "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir,
                "-Xlog:class+load+enhancement=warning")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("platform-loader:" + inner)
                .shouldNotContain("Bytecode enhancement skipped adding class");
    }

    private static void testWellKnownClassReplacementWithAndWithoutCDS() throws Exception {
        TestCase tc = new TestCase("well_known_string", "com.test.wellknown", "unused", "unused");
        byte[] original = readBaseModuleClass("java/lang/String.class");
        byte[] enhanced = addPublicStaticIntField(original, "bytecodeEnhancementField");
        Path target = tc.enhancementDir.resolve("java/lang/String.class");
        Files.createDirectories(target.getParent());
        Files.write(target, enhanced);
        writeFieldProbe(tc, "java.lang.String");
        tc.writeList("class-rep " + crc(original) + " java.lang.String\n");

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir, "-Xshare:off", "-Xlog:class+load+enhancement=debug")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("bytecodeEnhancementField")
                .stdoutShouldContain("int")
                .stdoutShouldContain("Bytecode enhancement replacing class java/lang/String")
                .shouldNotContain("no-bytecodeEnhancementField");

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir, "-Xshare:on", "-Xlog:class+load+enhancement=debug")
                .shouldNotHaveExitValue(0)
                .shouldContain("BytecodeEnhancement replacement of VM bootstrap class or supertype java/lang/String is not supported while using or creating a CDS archive")
                .shouldNotContain("Bytecode enhancement replacing class java/lang/String");
    }

    private static void testVmClassDependencyReplacementWithAndWithoutCDS() throws Exception {
        TestCase tc = new TestCase("vm_class_dependency", "com.test.vmclassdependency", "unused", "unused");
        byte[] original = readBaseModuleClass("java/util/AbstractList.class");
        byte[] enhanced = addPublicStaticIntField(original, "bytecodeEnhancementField");
        Path target = tc.enhancementDir.resolve("java/util/AbstractList.class");
        Files.createDirectories(target.getParent());
        Files.write(target, enhanced);
        writeFieldProbe(tc, "java.util.AbstractList");
        tc.writeList("class-rep " + crc(original) + " java.util.AbstractList\n");

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir, "-Xshare:off", "-Xlog:class+load+enhancement=debug")
                .shouldHaveExitValue(0)
                .stdoutShouldContain("bytecodeEnhancementField")
                .stdoutShouldContain("Bytecode enhancement replacing class java/util/AbstractList");

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + tc.enhancementDir, "-Xshare:on", "-Xlog:class+load+enhancement=debug")
                .shouldNotHaveExitValue(0)
                .shouldContain("BytecodeEnhancement replacement of VM bootstrap class or supertype java/util/AbstractList is not supported while using or creating a CDS archive")
                .shouldNotContain("Bytecode enhancement replacing class java/util/AbstractList");
    }

    private static void testClassRepAndAddMatrixAcrossLoadersAndPaths() throws Exception {
        TestCase tc = new TestCase("matrix", "com.test.matrix", "unused", "unused");
        writeMatrixAppSources(tc);
        Path dirPath = tc.root.resolve("matrix-dir");
        Path zipRoot = tc.root.resolve("matrix-zip-root");
        Path jarRoot = tc.root.resolve("matrix-jar-root");
        Files.createDirectories(dirPath);
        Files.createDirectories(zipRoot);
        Files.createDirectories(jarRoot);

        installMatrixEnhancement(dirPath,
                new EnhancementTarget("java.util.HashMap", readBaseModuleClass("java/util/HashMap.class"), true),
                new EnhancementTarget("com.test.path1.App", Files.readAllBytes(tc.originalClasses.resolve("com/test/path1/App.class")), false));
        installMatrixEnhancement(zipRoot,
                new EnhancementTarget("java.util.PriorityQueue", readBaseModuleClass("java/util/PriorityQueue.class"), true),
                new EnhancementTarget("com.test.path2.App", Files.readAllBytes(tc.originalClasses.resolve("com/test/path2/App.class")), false));
        installMatrixEnhancement(jarRoot,
                new EnhancementTarget("java.util.Random", readBaseModuleClass("java/util/Random.class"), true),
                new EnhancementTarget("com.test.path3.App", Files.readAllBytes(tc.originalClasses.resolve("com/test/path3/App.class")), false));
        Path zipPath = tc.root.resolve("matrix.zip");
        Path jarPath = tc.root.resolve("matrix.jar");
        createArchive(zipRoot, zipPath);
        createArchive(jarRoot, jarPath);

        runMain(tc, "-XX:BytecodeEnhancementPaths=" + dirPath + File.pathSeparator + zipPath + File.pathSeparator + jarPath)
                .shouldHaveExitValue(0)
                .stdoutShouldContain("java.util.HashMap$Inner:null")
                .stdoutShouldContain("java.util.PriorityQueue$Inner:null")
                .stdoutShouldContain("java.util.Random$Inner:null")
                .stdoutShouldContain("com.test.path1.App$Inner:app")
                .stdoutShouldContain("com.test.path2.App$Inner:app")
                .stdoutShouldContain("com.test.path3.App$Inner:app");
    }

    private static OutputAnalyzer runMain(TestCase tc, String... vmOptions) throws Exception {
        List<String> args = new ArrayList<>();
        args.addAll(List.of(vmOptions));
        args.add("-cp");
        args.add(tc.originalClasses.toString());
        args.add(tc.pkg + ".Main");
        ProcessBuilder pb = ProcessTools.createTestJavaProcessBuilder(args);
        return new OutputAnalyzer(pb.start());
    }

    private static final class TestCase {
        final String name;
        final String pkg;
        final String oldValue;
        final String newValue;
        final Path root;
        final Path originalSrc;
        final Path enhancementSrc;
        final Path originalClasses;
        final Path enhancementClasses;
        final Path enhancementDir;

        TestCase(String name, String pkg, String oldValue, String newValue) throws IOException {
            this.name = name;
            this.pkg = pkg;
            this.oldValue = oldValue;
            this.newValue = newValue;
            this.root = Files.createTempDirectory(ROOT, "bytecode-enhancement-" + name + "-");
            this.originalSrc = root.resolve("original-src");
            this.enhancementSrc = root.resolve("enhancement-src");
            this.originalClasses = root.resolve("original-classes");
            this.enhancementClasses = root.resolve("enhancement-classes");
            this.enhancementDir = root.resolve("enhancement-dir");
            Files.createDirectories(originalSrc);
            Files.createDirectories(enhancementSrc);
            Files.createDirectories(originalClasses);
            Files.createDirectories(enhancementClasses);
            Files.createDirectories(enhancementDir);
        }

        void compileOriginal() throws IOException {
            writeSource(originalSrc, "Main", """
                    package %s;
                    public class Main {
                        public static void main(String[] args) {
                            System.out.println(Target.message());
                        }
                    }
                    """.formatted(pkg));
            writeSource(originalSrc, "Target", """
                    package %s;
                    public class Target {
                        public static String message() {
                            return "%s";
                        }
                    }
                    """.formatted(pkg, oldValue));
            compile(originalClasses, sourcePath(originalSrc, "Main"), sourcePath(originalSrc, "Target"));
        }

        void compileEnhancement(boolean usesAdded) throws IOException {
            if (usesAdded) {
                writeSource(enhancementSrc, "Target", """
                        package %s;
                        public class Target {
                            public static String message() {
                                return Added.message() + "\\n" + Added.source();
                            }
                        }
                        """.formatted(pkg));
                writeSource(enhancementSrc, "Added", """
                        package %s;
                        class Added {
                            static String message() {
                                return "%s";
                            }
                            static String source() {
                                var codeSource = Added.class.getProtectionDomain().getCodeSource();
                                return codeSource == null ? "null" : codeSource.getLocation().toString();
                            }
                        }
                        """.formatted(pkg, newValue));
                compile(enhancementClasses, sourcePath(enhancementSrc, "Target"), sourcePath(enhancementSrc, "Added"));
            } else {
                writeSource(enhancementSrc, "Target", """
                        package %s;
                        public class Target {
                            public static String message() {
                                return "%s";
                            }
                        }
                        """.formatted(pkg, newValue));
                compile(enhancementClasses, sourcePath(enhancementSrc, "Target"));
            }
        }

        void installEnhancementClass(String simpleName) throws IOException {
            Path source = classFile(enhancementClasses, simpleName);
            Path target = classFile(enhancementDir, simpleName);
            Files.createDirectories(target.getParent());
            Files.copy(source, target);
        }

        void writeList(String content) throws IOException {
            Files.writeString(enhancementDir.resolve("bytecode-enhancement.list"), content);
        }

        String originalCrc(String simpleName) throws IOException {
            return crc(classFile(originalClasses, simpleName));
        }

        String enhancementDirUrl() throws IOException {
            return enhancementDir.toRealPath().toUri().toURL().toString();
        }

        Path createEnhancementZip() throws IOException {
            Path zip = root.resolve("enhancement.zip");
            try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(zip))) {
                try (var files = Files.walk(enhancementDir)) {
                    for (Path file : files.filter(Files::isRegularFile).toList()) {
                        String entryName = enhancementDir.relativize(file).toString().replace(File.separatorChar, '/');
                        out.putNextEntry(new ZipEntry(entryName));
                        out.write(Files.readAllBytes(file));
                        out.closeEntry();
                    }
                }
            }
            return zip;
        }

        private void writeSource(Path srcRoot, String simpleName, String source) throws IOException {
            Path path = sourcePath(srcRoot, simpleName);
            Files.createDirectories(path.getParent());
            Files.writeString(path, source);
        }

        private Path sourcePath(Path srcRoot, String simpleName) {
            return srcRoot.resolve(pkg.replace('.', '/')).resolve(simpleName + ".java");
        }

        private Path classFile(Path classesRoot, String simpleName) {
            return classesRoot.resolve(pkg.replace('.', '/')).resolve(simpleName + ".class");
        }
    }



    private record EnhancementTarget(String name, byte[] original, boolean bootLoader) {}

    private static void writeMatrixAppSources(TestCase tc) throws IOException {
        tc.writeSource(tc.originalSrc, "Main", """
                package %s;
                import java.util.HashMap;
                import java.util.PriorityQueue;
                import java.util.Random;
                public class Main {
                    public static void main(String[] args) throws Exception {
                        probe(HashMap.class, new HashMap<>(), true);
                        probe(PriorityQueue.class, new PriorityQueue<>(), true);
                        probe(Random.class, new Random(), true);
                        probe(com.test.path1.App.class, new com.test.path1.App(), false);
                        probe(com.test.path2.App.class, new com.test.path2.App(), false);
                        probe(com.test.path3.App.class, new com.test.path3.App(), false);
                    }
                    private static void probe(Class<?> ownerClass, Object owner, boolean bootLoader) throws Exception {
                        var method = ownerClass.getMethod("bytecodeEnhancementNewInner");
                        Object inner = method.invoke(owner);
                        ClassLoader ownerLoader = ownerClass.getClassLoader();
                        ClassLoader innerLoader = inner.getClass().getClassLoader();
                        if (bootLoader && (ownerLoader != null || innerLoader != null)) {
                            throw new RuntimeException("expected boot loader for " + ownerClass.getName());
                        }
                        if (!bootLoader && (ownerLoader == null || ownerLoader != innerLoader)) {
                            throw new RuntimeException("expected app loader for " + ownerClass.getName());
                        }
                        System.out.println(inner.getClass().getName() + ":" + (innerLoader == null ? "null" : "app"));
                    }
                }
                """.formatted(tc.pkg));
        writeSource(tc.originalSrc, "com.test.path1", "App", """
                package com.test.path1;
                public class App {}
                """);
        writeSource(tc.originalSrc, "com.test.path2", "App", """
                package com.test.path2;
                public class App {}
                """);
        writeSource(tc.originalSrc, "com.test.path3", "App", """
                package com.test.path3;
                public class App {}
                """);
        compile(tc.originalClasses,
                tc.sourcePath(tc.originalSrc, "Main"),
                sourcePath(tc.originalSrc, "com.test.path1", "App"),
                sourcePath(tc.originalSrc, "com.test.path2", "App"),
                sourcePath(tc.originalSrc, "com.test.path3", "App"));
    }

    private static void installMatrixEnhancement(Path enhancementRoot, EnhancementTarget... targets) throws IOException {
        StringBuilder list = new StringBuilder();
        for (EnhancementTarget target : targets) {
            String internalName = target.name.replace('.', '/');
            String innerName = target.name + "$Inner";
            String innerInternalName = internalName + "$Inner";
            writeClassFile(enhancementRoot, internalName, addPublicNewInnerMethod(target.original, "bytecodeEnhancementNewInner", innerInternalName));
            writeClassFile(enhancementRoot, innerInternalName, minimalPublicClass(innerInternalName));
            list.append("class-rep ").append(crc(target.original)).append(' ').append(target.name).append('\n');
            list.append("class-add ").append(innerName).append(" owner-class=").append(target.name).append('\n');
        }
        Files.writeString(enhancementRoot.resolve("bytecode-enhancement.list"), list.toString());
    }

    private static void writeClassFile(Path root, String internalName, byte[] bytes) throws IOException {
        Path file = root.resolve(internalName + ".class");
        Files.createDirectories(file.getParent());
        Files.write(file, bytes);
    }

    private static void createArchive(Path root, Path archive) throws IOException {
        try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(archive))) {
            try (var files = Files.walk(root)) {
                for (Path file : files.filter(Files::isRegularFile).toList()) {
                    String entryName = root.relativize(file).toString().replace(File.separatorChar, '/');
                    out.putNextEntry(new ZipEntry(entryName));
                    out.write(Files.readAllBytes(file));
                    out.closeEntry();
                }
            }
        }
    }


    private static void writeSource(Path srcRoot, String pkg, String simpleName, String source) throws IOException {
        Path path = sourcePath(srcRoot, pkg, simpleName);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
    }

    private static Path sourcePath(Path srcRoot, String pkg, String simpleName) {
        return srcRoot.resolve(pkg.replace('.', '/')).resolve(simpleName + ".java");
    }


    private static void writeFieldProbe(TestCase tc, String className) throws IOException {
        tc.writeSource(tc.originalSrc, "Main", """
                package %s;
                public class Main {
                    public static void main(String[] args) throws Exception {
                        Class<?> target = Class.forName("%s");
                        try {
                            var field = target.getField("bytecodeEnhancementField");
                            System.out.println(field.getName());
                            System.out.println(field.getType().getName());
                        } catch (NoSuchFieldException expected) {
                            System.out.println("no-bytecodeEnhancementField");
                        }
                    }
                }
                """.formatted(tc.pkg, className));
        compile(tc.originalClasses, tc.sourcePath(tc.originalSrc, "Main"));
    }

    private static byte[] readBaseModuleClass(String name) throws IOException {
        return readModuleClass("java.base", name);
    }

    private static byte[] readModuleClass(String module, String name) throws IOException {
        Path jmod = Path.of(System.getProperty("java.home"), "jmods", module + ".jmod");
        try (FileSystem fs = FileSystems.newFileSystem(jmod, Map.of())) {
            return Files.readAllBytes(fs.getPath("classes", name));
        }
    }


    private static byte[] addPublicNewInnerMethod(byte[] original, String methodName, String innerInternalName) throws IOException {
        ClassCursor cursor = new ClassCursor(original);
        cursor.u4(); // magic
        cursor.u2(); // minor_version
        cursor.u2(); // major_version
        int cpCountOffset = cursor.position();
        int cpCount = cursor.u2();
        for (int i = 1; i < cpCount; i++) {
            int tag = cursor.u1();
            switch (tag) {
                case 1 -> cursor.skip(cursor.u2());
                case 3, 4, 9, 10, 11, 12, 17, 18 -> cursor.skip(4);
                case 5, 6 -> { cursor.skip(8); i++; }
                case 7, 8, 16, 19, 20 -> cursor.skip(2);
                case 15 -> cursor.skip(3);
                default -> throw new IOException("unsupported constant pool tag " + tag);
            }
        }
        int cpEnd = cursor.position();

        cursor.skip(6); // access_flags, this_class, super_class
        int interfacesCount = cursor.u2();
        cursor.skip(2 * interfacesCount);
        int fieldsCount = cursor.u2();
        for (int i = 0; i < fieldsCount; i++) {
            skipMember(cursor);
        }
        int methodsCountOffset = cursor.position();
        int methodsCount = cursor.u2();
        for (int i = 0; i < methodsCount; i++) {
            skipMember(cursor);
        }
        int methodsEnd = cursor.position();

        ByteArrayOutputStream cpAdd = new ByteArrayOutputStream();
        writeUtf8(cpAdd, methodName);
        writeUtf8(cpAdd, "()Ljava/lang/Object;");
        writeUtf8(cpAdd, "Code");
        writeUtf8(cpAdd, innerInternalName);
        writeClass(cpAdd, cpCount + 3);
        writeUtf8(cpAdd, "<init>");
        writeUtf8(cpAdd, "()V");
        writeNameAndType(cpAdd, cpCount + 5, cpCount + 6);
        writeMethodRef(cpAdd, cpCount + 4, cpCount + 7);

        ByteArrayOutputStream code = new ByteArrayOutputStream();
        code.write(0xbb); // new
        writeU2(code, cpCount + 4);
        code.write(0x59); // dup
        code.write(0xb7); // invokespecial
        writeU2(code, cpCount + 8);
        code.write(0xb0); // areturn

        ByteArrayOutputStream codeAttribute = new ByteArrayOutputStream();
        writeU2(codeAttribute, 2); // max_stack
        writeU2(codeAttribute, 1); // max_locals
        writeU4(codeAttribute, code.size());
        codeAttribute.write(code.toByteArray());
        writeU2(codeAttribute, 0); // exception_table_length
        writeU2(codeAttribute, 0); // attributes_count

        ByteArrayOutputStream method = new ByteArrayOutputStream();
        writeU2(method, 0x0001); // ACC_PUBLIC
        writeU2(method, cpCount); // name_index
        writeU2(method, cpCount + 1); // descriptor_index
        writeU2(method, 1); // attributes_count
        writeU2(method, cpCount + 2); // Code
        writeU4(method, codeAttribute.size());
        method.write(codeAttribute.toByteArray());

        ByteArrayOutputStream out = new ByteArrayOutputStream(original.length + cpAdd.size() + method.size());
        out.write(original, 0, cpCountOffset);
        writeU2(out, cpCount + 9);
        out.write(original, cpCountOffset + 2, cpEnd - cpCountOffset - 2);
        out.write(cpAdd.toByteArray());
        out.write(original, cpEnd, methodsCountOffset - cpEnd);
        writeU2(out, methodsCount + 1);
        out.write(original, methodsCountOffset + 2, methodsEnd - methodsCountOffset - 2);
        out.write(method.toByteArray());
        out.write(original, methodsEnd, original.length - methodsEnd);
        return out.toByteArray();
    }

    private static byte[] minimalPublicClass(String internalName) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        writeU4(out, 0xcafebabe);
        writeU2(out, 0); // minor_version
        writeU2(out, 52); // major_version
        writeU2(out, 10); // constant_pool_count
        writeUtf8(out, internalName); // #1
        writeClass(out, 1); // #2
        writeUtf8(out, "java/lang/Object"); // #3
        writeClass(out, 3); // #4
        writeUtf8(out, "<init>"); // #5
        writeUtf8(out, "()V"); // #6
        writeUtf8(out, "Code"); // #7
        writeNameAndType(out, 5, 6); // #8
        writeMethodRef(out, 4, 8); // #9
        writeU2(out, 0x0021); // ACC_PUBLIC | ACC_SUPER
        writeU2(out, 2); // this_class
        writeU2(out, 4); // super_class
        writeU2(out, 0); // interfaces_count
        writeU2(out, 0); // fields_count
        writeU2(out, 1); // methods_count
        writeU2(out, 0x0001); // ACC_PUBLIC
        writeU2(out, 5); // <init>
        writeU2(out, 6); // ()V
        writeU2(out, 1); // attributes_count
        writeU2(out, 7); // Code
        writeU4(out, 17); // attribute_length
        writeU2(out, 1); // max_stack
        writeU2(out, 1); // max_locals
        writeU4(out, 5); // code_length
        out.write(0x2a); // aload_0
        out.write(0xb7); // invokespecial
        writeU2(out, 9);
        out.write(0xb1); // return
        writeU2(out, 0); // exception_table_length
        writeU2(out, 0); // attributes_count
        writeU2(out, 0); // class attributes_count
        return out.toByteArray();
    }

    private static void skipMember(ClassCursor cursor) {
        cursor.skip(6); // access_flags, name_index, descriptor_index
        int attributesCount = cursor.u2();
        for (int i = 0; i < attributesCount; i++) {
            cursor.skip(2);
            cursor.skip(cursor.u4());
        }
    }


    private static byte[] addPublicStaticIntField(byte[] original, String fieldName) throws IOException {
        ClassCursor cursor = new ClassCursor(original);
        cursor.u4(); // magic
        cursor.u2(); // minor_version
        cursor.u2(); // major_version
        int cpCountOffset = cursor.position();
        int cpCount = cursor.u2();
        for (int i = 1; i < cpCount; i++) {
            int tag = cursor.u1();
            switch (tag) {
                case 1 -> cursor.skip(cursor.u2());
                case 3, 4, 9, 10, 11, 12, 17, 18 -> cursor.skip(4);
                case 5, 6 -> { cursor.skip(8); i++; }
                case 7, 8, 16, 19, 20 -> cursor.skip(2);
                case 15 -> cursor.skip(3);
                default -> throw new IOException("unsupported constant pool tag " + tag);
            }
        }
        int cpEnd = cursor.position();

        cursor.skip(6); // access_flags, this_class, super_class
        int interfacesCount = cursor.u2();
        cursor.skip(2 * interfacesCount);
        int fieldsCountOffset = cursor.position();
        int fieldsCount = cursor.u2();
        for (int i = 0; i < fieldsCount; i++) {
            skipMember(cursor);
        }
        int fieldsEnd = cursor.position();

        ByteArrayOutputStream cpAdd = new ByteArrayOutputStream();
        writeUtf8(cpAdd, fieldName);
        writeUtf8(cpAdd, "I");
        int nameIndex = cpCount;
        int descriptorIndex = cpCount + 1;

        ByteArrayOutputStream field = new ByteArrayOutputStream();
        writeU2(field, 0x0009); // ACC_PUBLIC | ACC_STATIC
        writeU2(field, nameIndex);
        writeU2(field, descriptorIndex);
        writeU2(field, 0); // attributes_count

        ByteArrayOutputStream out = new ByteArrayOutputStream(original.length + cpAdd.size() + field.size());
        out.write(original, 0, cpCountOffset);
        writeU2(out, cpCount + 2);
        out.write(original, cpCountOffset + 2, cpEnd - cpCountOffset - 2);
        out.write(cpAdd.toByteArray());
        out.write(original, cpEnd, fieldsCountOffset - cpEnd);
        writeU2(out, fieldsCount + 1);
        out.write(original, fieldsCountOffset + 2, fieldsEnd - fieldsCountOffset - 2);
        out.write(field.toByteArray());
        out.write(original, fieldsEnd, original.length - fieldsEnd);
        return out.toByteArray();
    }

    private static void writeUtf8(ByteArrayOutputStream out, String value) throws IOException {
        byte[] bytes = value.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        out.write(1);
        writeU2(out, bytes.length);
        out.write(bytes);
    }

    private static void writeClass(ByteArrayOutputStream out, int nameIndex) {
        out.write(7);
        writeU2(out, nameIndex);
    }

    private static void writeNameAndType(ByteArrayOutputStream out, int nameIndex, int descriptorIndex) {
        out.write(12);
        writeU2(out, nameIndex);
        writeU2(out, descriptorIndex);
    }

    private static void writeMethodRef(ByteArrayOutputStream out, int classIndex, int nameAndTypeIndex) {
        out.write(10);
        writeU2(out, classIndex);
        writeU2(out, nameAndTypeIndex);
    }

    private static void writeU2(ByteArrayOutputStream out, int value) {
        out.write((value >>> 8) & 0xff);
        out.write(value & 0xff);
    }

    private static void writeU4(ByteArrayOutputStream out, int value) {
        out.write((value >>> 24) & 0xff);
        out.write((value >>> 16) & 0xff);
        out.write((value >>> 8) & 0xff);
        out.write(value & 0xff);
    }

    private static String crc(byte[] bytes) {
        CRC32 crc = new CRC32();
        crc.update(bytes);
        return "0x%08x".formatted(crc.getValue());
    }

    private static final class ClassCursor {
        private final byte[] bytes;
        private int pos;

        ClassCursor(byte[] bytes) {
            this.bytes = bytes;
        }

        int position() {
            return pos;
        }

        int u1() {
            return bytes[pos++] & 0xff;
        }

        int u2() {
            int value = ((bytes[pos] & 0xff) << 8) | (bytes[pos + 1] & 0xff);
            pos += 2;
            return value;
        }

        int u4() {
            int value = ((bytes[pos] & 0xff) << 24) | ((bytes[pos + 1] & 0xff) << 16) |
                        ((bytes[pos + 2] & 0xff) << 8) | (bytes[pos + 3] & 0xff);
            pos += 4;
            return value;
        }

        void skip(int n) {
            pos += n;
            if (pos < 0 || pos > bytes.length) {
                throw new IllegalArgumentException("bad classfile offset");
            }
        }
    }

    private static void compile(Path outputDir, Path... sources) throws IOException {
        List<String> args = new ArrayList<>();
        args.add("-d");
        args.add(outputDir.toString());
        for (Path source : sources) {
            args.add(source.toString());
        }
        int rc = JAVAC.run(null, null, null, args.toArray(String[]::new));
        if (rc != 0) {
            throw new RuntimeException("javac failed: " + args);
        }
    }

    private static String crc(Path path) throws IOException {
        CRC32 crc = new CRC32();
        crc.update(Files.readAllBytes(path));
        return "0x%08x".formatted(crc.getValue());
    }
}
