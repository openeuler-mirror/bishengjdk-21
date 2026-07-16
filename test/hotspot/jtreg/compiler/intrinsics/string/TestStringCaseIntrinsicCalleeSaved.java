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
 * @summary Smoke test UTF16 String case stub calls with live floating-point values
 * @requires os.arch=="aarch64"
 * @requires vm.compiler2.enabled
 * @requires vm.flagless
 * @library /test/lib
 * @modules java.base/java.lang:open
 *          java.base/jdk.internal.misc
 * @build jdk.test.whitebox.WhiteBox
 *        compiler.intrinsics.string.StringCaseIntrinsicLogVerifier
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions
 *      -XX:+WhiteBoxAPI compiler.intrinsics.string.TestStringCaseIntrinsicCalleeSaved
 */

package compiler.intrinsics.string;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;
import jtreg.SkippedException;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TestStringCaseIntrinsicCalleeSaved {
    private static final int COMP_LEVEL_FULL_OPTIMIZATION = 4;
    private static final String HISILICON_IMPLEMENTER = "0x48:";
    private static final String CLASS_NAME =
            TestStringCaseIntrinsicCalleeSaved.class.getName();
    private static final Map<String, String> WRAPPER_INTRINSICS =
            wrapperIntrinsics();
    private static final WhiteBox WHITE_BOX = WhiteBox.getWhiteBox();
    private static final int LENGTH = 64;
    private static final double[] DATA = {
            1.125, 2.25, 3.5, 4.75, 5.875, 6.0625,
            7.125, 8.25, 9.5, 10.75, 11.875, 12.0625
    };
    private static final byte[] LOWER_SOURCE = utf16('A', '\u0100');
    private static final byte[] UPPER_SOURCE = utf16('a', '\u0101');
    private static final byte[] LOWER_RESULT = new byte[LENGTH * 2];
    private static final byte[] UPPER_RESULT = new byte[LENGTH * 2];
    private static final MethodHandle LOWER;
    private static final MethodHandle UPPER;
    private static volatile int markerSink;

    // This creates cross-call floating-point liveness, but C2 remains free to
    // allocate, spill, or recompute the values. Stub-generation ASSERTs provide
    // the deterministic guard against using v8-v15 as scratch registers.

    static {
        try {
            Class<?> utf16 = Class.forName("java.lang.StringUTF16");
            MethodHandles.Lookup lookup = MethodHandles.privateLookupIn(
                    utf16, MethodHandles.lookup());
            MethodType type = MethodType.methodType(int.class,
                    byte[].class, byte[].class, int.class, int.class);
            LOWER = lookup.findStatic(utf16, "toLowerCaseSimple", type);
            UPPER = lookup.findStatic(utf16, "toUpperCaseSimple", type);
        } catch (ReflectiveOperationException exception) {
            throw new ExceptionInInitializerError(exception);
        }
    }

    private static byte[] utf16(char first, char rest) {
        ByteBuffer buffer = ByteBuffer.allocate(LENGTH * 2).order(ByteOrder.nativeOrder());
        buffer.putChar(first);
        for (int i = 1; i < LENGTH; i++) {
            buffer.putChar(rest);
        }
        return buffer.array();
    }

    private static double sumA() {
        return DATA[0] * 1.25 + DATA[1] * 2.5 + DATA[2] * 3.75
             + DATA[3] * 4.125 + DATA[4] * 5.25 + DATA[5] * 6.5;
    }

    private static double sumB() {
        return DATA[6] * 7.25 + DATA[7] * 8.5 + DATA[8] * 9.75
             + DATA[9] * 10.125 + DATA[10] * 11.25 + DATA[11] * 12.5;
    }

    private static double lower() throws Throwable {
        double a = sumA();
        double b = sumB();
        int marker = (int) LOWER.invokeExact(LOWER_SOURCE, LOWER_RESULT, 0, LENGTH);
        markerSink = marker;
        return a * (marker + 1.0) + b * (getChar(LOWER_RESULT, 0) + 1.0);
    }

    private static double upper() throws Throwable {
        double a = sumA();
        double b = sumB();
        int marker = (int) UPPER.invokeExact(UPPER_SOURCE, UPPER_RESULT, 0, LENGTH);
        markerSink = marker;
        return a * (marker + 1.0) + b * (getChar(UPPER_RESULT, 0) + 1.0);
    }

    private static int getChar(byte[] value, int index) {
        return ByteBuffer.wrap(value).order(ByteOrder.nativeOrder()).getChar(index * 2);
    }

    public static void main(String[] args) throws Throwable {
        if (args.length == 0) {
            runDriver();
        } else if (args.length == 1 && args[0].equals("worker")) {
            runWorker();
        } else {
            throw new IllegalArgumentException(
                    "Unexpected arguments: " + Arrays.toString(args));
        }
    }

    private static void runDriver() throws Exception {
        String cpuFeatures = WHITE_BOX.getCPUFeatures();
        if (!cpuFeatures.startsWith(HISILICON_IMPLEMENTER)
                || !hasCPUFeature(cpuFeatures, "sve")) {
            throw new SkippedException("HiSilicon SVE is not available: "
                    + cpuFeatures);
        }

        runBackend(1);
        if (hasCPUFeature(cpuFeatures, "sve2")
                && hasCPUFeature(cpuFeatures, "svebitperm")) {
            runBackend(2);
        }
    }

    private static boolean hasCPUFeature(String cpuFeatures, String feature) {
        return Arrays.asList(cpuFeatures.split("[, ]+")).contains(feature);
    }

    private static Map<String, String> wrapperIntrinsics() {
        Map<String, String> result = new LinkedHashMap<>();
        result.put("lower", "_stringUTF16ToLowerCase");
        result.put("upper", "_stringUTF16ToUpperCase");
        return Map.copyOf(result);
    }

    private static void runBackend(int backend) throws Exception {
        Path logFile = Path.of("string-case-callee-saved-" + backend + ".log")
                .toAbsolutePath();
        Files.deleteIfExists(logFile);

        List<String> options = new ArrayList<>();
        options.add("-Xbootclasspath/a:.");
        options.add("--add-opens=java.base/java.lang=ALL-UNNAMED");
        options.add("-Xbatch");
        options.add("-XX:-TieredCompilation");
        options.add("-XX:CompileThreshold=100");
        options.add("-XX:+UnlockDiagnosticVMOptions");
        options.add("-XX:+WhiteBoxAPI");
        options.add("-XX:+LogCompilation");
        options.add("-XX:LogFile=" + logFile);
        options.add("-XX:UseSVE=" + backend);
        options.add("-XX:StringCaseIntrinsicBackend=" + backend);
        options.add("-XX:CompileCommand=compileonly," + CLASS_NAME + "::lower");
        options.add("-XX:CompileCommand=compileonly," + CLASS_NAME + "::upper");
        options.add("-DexpectedBackend=" + backend);
        options.add(CLASS_NAME);
        options.add("worker");

        OutputAnalyzer output = new OutputAnalyzer(
                ProcessTools.createLimitedTestJavaProcessBuilder(options).start());
        output.shouldHaveExitValue(0);
        StringCaseIntrinsicLogVerifier.verify(
                logFile, CLASS_NAME, WRAPPER_INTRINSICS, true);
        Files.delete(logFile);
    }

    private static void runWorker() throws Throwable {
        int expectedBackend = Integer.getInteger("expectedBackend", -1);
        int actualBackend = WHITE_BOX.getUintVMFlag(
                "StringCaseIntrinsicBackend").intValue();
        int actualUseSVE = WHITE_BOX.getUintVMFlag("UseSVE").intValue();
        if (actualBackend != expectedBackend || actualUseSVE != expectedBackend) {
            throw new AssertionError("backend=" + actualBackend
                    + ", UseSVE=" + actualUseSVE
                    + ", expected " + expectedBackend);
        }

        assertIntrinsicAvailability();
        for (int i = 0; i < 20_000; i++) {
            lower();
            upper();
        }
        assertCompiledByC2("lower");
        assertCompiledByC2("upper");

        for (int i = 0; i < 200_000; i++) {
            double lower = lower();
            int lowerMarker = markerSink;
            double expectedLower = sumA() * (lowerMarker + 1.0)
                    + sumB() * ('a' + 1.0);
            double upper = upper();
            int upperMarker = markerSink;
            double expectedUpper = sumA() * (upperMarker + 1.0)
                    + sumB() * ('A' + 1.0);
            if (Double.doubleToRawLongBits(lower) != Double.doubleToRawLongBits(expectedLower)
                    || Double.doubleToRawLongBits(upper) != Double.doubleToRawLongBits(expectedUpper)) {
                throw new AssertionError("iteration=" + i
                        + " lower=" + lower + " expectedLower=" + expectedLower
                        + " lowerMarker=" + lowerMarker
                        + " upper=" + upper + " expectedUpper=" + expectedUpper
                        + " upperMarker=" + upperMarker);
            }
        }
    }

    private static void assertIntrinsicAvailability()
            throws ReflectiveOperationException {
        Class<?> utf16 = Class.forName("java.lang.StringUTF16");
        Class<?>[] parameters = {
                byte[].class, byte[].class, int.class, int.class
        };
        Method[] helpers = {
                utf16.getDeclaredMethod("toLowerCaseSimple", parameters),
                utf16.getDeclaredMethod("toUpperCaseSimple", parameters)
        };
        for (Method helper : helpers) {
            if (!WHITE_BOX.isIntrinsicAvailable(
                    helper, COMP_LEVEL_FULL_OPTIMIZATION)) {
                throw new AssertionError(helper + " intrinsic is unavailable");
            }
        }
    }

    private static void assertCompiledByC2(String methodName)
            throws ReflectiveOperationException {
        Method method = TestStringCaseIntrinsicCalleeSaved.class
                .getDeclaredMethod(methodName);
        if (WHITE_BOX.getMethodCompilationLevel(method)
                != COMP_LEVEL_FULL_OPTIMIZATION) {
            WHITE_BOX.enqueueMethodForCompilation(
                    method, COMP_LEVEL_FULL_OPTIMIZATION);
        }
        int level = WHITE_BOX.getMethodCompilationLevel(method);
        if (level != COMP_LEVEL_FULL_OPTIMIZATION) {
            throw new AssertionError(methodName + " compilation level=" + level
                    + ", expected " + COMP_LEVEL_FULL_OPTIMIZATION);
        }
    }
}
