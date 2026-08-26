/*
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
 * @summary Test AArch64 String.equalsIgnoreCase C2 intrinsic end to end.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.flagless
 * @requires vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @modules java.base/java.lang:open
 * @build jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver compiler.intrinsics.string.TestStringEqualsIgnoreCaseIntrinsic
 */

package compiler.intrinsics.string;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jtreg.SkippedException;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

public class TestStringEqualsIgnoreCaseIntrinsic {
    private static final String CLASS_NAME =
            TestStringEqualsIgnoreCaseIntrinsic.class.getName();
    private static final String CHILD = "child";
    private static final String PREFLIGHT = "preflight";
    private static final int COMP_LEVEL_FULL_OPTIMIZATION = 4;
    private static final int MIN_LENGTH = 16;
    private static final int REQUIRED_VECTOR_LENGTH = 32;
    private static final int WARMUP_ROUNDS = 2_000;
    private static final int PHASE_LENGTH = 258;
    private static final int PHASE_ANCHOR = 127;
    private static final Pattern PREFLIGHT_MARKER = Pattern.compile(
            "(?m)^EIIC_PREFLIGHT\\|use_sve=(\\d+)\\|vl=(\\d+)\\r?$");

    private static final String LL_LEFT = "aBcDeFgH".repeat(4);
    private static final String LL_RIGHT = "AbCdEfGh".repeat(4);
    private static final String LU_LEFT = "k".repeat(32);
    private static final String LU_RIGHT = "\u212A".repeat(32);
    private static final String UL_LEFT = "\u212A".repeat(32);
    private static final String UL_RIGHT = "k".repeat(32);
    private static final String UU_LEFT =
            "\u03b1\u03b2\u03b3\u03b4\u03b5\u03b6\u03b7\u03b8".repeat(4);
    private static final String UU_RIGHT =
            "\u0391\u0392\u0393\u0394\u0395\u0396\u0397\u0398".repeat(4);

    private static final String LOWER_PREFIX = "a".repeat(PHASE_ANCHOR);
    private static final String UPPER_PREFIX = "A".repeat(PHASE_ANCHOR);
    private static final String LOWER_SUFFIX = "b".repeat(129);
    private static final String UPPER_SUFFIX = "B".repeat(129);
    private static final String SUPPLEMENTARY_LEFT =
            LOWER_PREFIX + "\uD801\uDC00" + LOWER_SUFFIX;
    private static final String SUPPLEMENTARY_MATCH =
            UPPER_PREFIX + "\uD801\uDC28" + UPPER_SUFFIX;
    private static final String MALFORMED_LEFT =
            LOWER_PREFIX + "\uD800X" + LOWER_SUFFIX;
    private static final String MALFORMED_MATCH =
            UPPER_PREFIX + "\uD800x" + UPPER_SUFFIX;
    private static final String EARLIER_MISMATCH =
            "?" + "A".repeat(PHASE_ANCHOR - 1)
                    + "\uD800x" + UPPER_SUFFIX;

    private static final Case[] CASES = {
            new Case("LL match", LL_LEFT, LL_RIGHT, true),
            new Case("LL mismatch", LL_LEFT, replaceLast(LL_RIGHT, '!'), false),
            new Case("LU match", LU_LEFT, LU_RIGHT, true),
            new Case("LU mismatch", LU_LEFT, replaceLast(LU_RIGHT, '!'), false),
            new Case("UL match", UL_LEFT, UL_RIGHT, true),
            new Case("UL mismatch", UL_LEFT, replaceLast(UL_RIGHT, '!'), false),
            new Case("UU match", UU_LEFT, UU_RIGHT, true),
            new Case("UU mismatch", UU_LEFT, replaceLast(UU_RIGHT, '!'), false),
            new Case("legal supplementary", SUPPLEMENTARY_LEFT,
                    SUPPLEMENTARY_MATCH, true),
            new Case("malformed checkpoint", MALFORMED_LEFT,
                    MALFORMED_MATCH, true),
            new Case("mismatch before malformed checkpoint", MALFORMED_LEFT,
                    EARLIER_MISMATCH, false)
    };

    private static volatile boolean sink;

    static {
        for (int i = 8; i < CASES.length; i++) {
            if (CASES[i].left().length() != PHASE_LENGTH
                    || CASES[i].right().length() != PHASE_LENGTH) {
                throw new ExceptionInInitializerError(
                        CASES[i].label() + " has wrong phase length");
            }
        }
    }

    public static void main(String[] args) throws Exception {
        if (args.length > 0 && CHILD.equals(args[0])) {
            runChild(args);
            return;
        }
        if (args.length > 0 && PREFLIGHT.equals(args[0])) {
            runPreflightChild();
            return;
        }
        requireSupportedVectorLength();
        runCase(true);
        runCase(false);
    }

    public static boolean equalsWrapper(String left, String right) {
        return left.equalsIgnoreCase(right);
    }

    public static boolean regionWrapper(String left, int leftOffset,
                                        String right, int rightOffset,
                                        int length) {
        return left.regionMatches(true, leftOffset,
                right, rightOffset, length);
    }

    private static void requireSupportedVectorLength() throws Exception {
        OutputAnalyzer output = ProcessTools.executeTestJava(
                "-Xbootclasspath/a:.",
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                "-XX:UseSVE=1",
                "-XX:MaxVectorSize=" + REQUIRED_VECTOR_LENGTH,
                "-XX:-UseStringEqualsIgnoreCaseIntrinsic",
                CLASS_NAME,
                PREFLIGHT).shouldHaveExitValue(0);
        Matcher marker = PREFLIGHT_MARKER.matcher(output.getStdout());
        if (!marker.find()) {
            throw new AssertionError("missing equalsIgnoreCase preflight marker");
        }
        int useSVE = Integer.parseInt(marker.group(1));
        int vectorLength = Integer.parseInt(marker.group(2));
        if (useSVE != 1 || vectorLength != REQUIRED_VECTOR_LENGTH) {
            throw new SkippedException(
                    "requires effective UseSVE=1 and MaxVectorSize="
                            + REQUIRED_VECTOR_LENGTH + "; got UseSVE="
                            + useSVE + ", MaxVectorSize=" + vectorLength);
        }
    }

    private static void runPreflightChild() {
        WhiteBox wb = WhiteBox.getWhiteBox();
        System.out.println("EIIC_PREFLIGHT|use_sve="
                + uintFlag(wb, "UseSVE")
                + "|vl=" + intxFlag(wb, "MaxVectorSize"));
    }

    private static void runCase(boolean enabled) throws Exception {
        String mode = enabled ? "enabled" : "flag-off";
        List<String> command = new ArrayList<>();
        command.add("-Xbootclasspath/a:.");
        command.add("--add-opens=java.base/java.lang=ALL-UNNAMED");
        command.add("-Xbatch");
        command.add("-XX:-TieredCompilation");
        command.add("-XX:CompileThreshold=100000");
        command.add("-XX:+CompactStrings");
        command.add("-XX:+UnlockDiagnosticVMOptions");
        command.add("-XX:+WhiteBoxAPI");
        command.add("-XX:UseSVE=1");
        command.add("-XX:MaxVectorSize=" + REQUIRED_VECTOR_LENGTH);
        command.add(enabled
                ? "-XX:+UseStringEqualsIgnoreCaseIntrinsic"
                : "-XX:-UseStringEqualsIgnoreCaseIntrinsic");
        command.add("-XX:StringEqualsIgnoreCaseIntrinsicMinLength="
                + MIN_LENGTH);
        command.add("-XX:CompileCommand=quiet");
        command.add("-XX:CompileCommand=inline,java.lang.String::equalsIgnoreCase");
        command.add("-XX:CompileCommand=inline,java.lang.String::regionMatches");
        command.add("-XX:CompileCommand=inline,java.lang.StringLatin1::regionMatchesCI");
        command.add("-XX:CompileCommand=inline,java.lang.StringLatin1::regionMatchesCI_UTF16");
        command.add("-XX:CompileCommand=inline,java.lang.StringUTF16::regionMatchesCI");
        command.add("-XX:CompileCommand=inline,java.lang.StringUTF16::regionMatchesCI_Latin1");
        command.add("-XX:CompileCommand=compileonly," + CLASS_NAME
                + "::equalsWrapper");
        command.add("-XX:CompileCommand=compileonly," + CLASS_NAME
                + "::regionWrapper");
        command.add(CLASS_NAME);
        command.add(CHILD);
        command.add(Boolean.toString(enabled));

        OutputAnalyzer output = ProcessTools.executeProcess(
                ProcessTools.createTestJavaProcessBuilder(
                        command.toArray(new String[0])));
        output.shouldHaveExitValue(0);
        output.shouldContain("EIIC_PASS|" + mode);
        System.out.print(output.getStdout());
    }

    private static void runChild(String[] args) throws Exception {
        if (args.length != 2) {
            throw new AssertionError("expected: child <enabled>");
        }
        boolean expectedEnabled = Boolean.parseBoolean(args[1]);
        WhiteBox wb = WhiteBox.getWhiteBox();
        checkEquals("UseStringEqualsIgnoreCaseIntrinsic",
                expectedEnabled,
                booleanFlag(wb, "UseStringEqualsIgnoreCaseIntrinsic"));
        checkEquals("UseSVE", 1, uintFlag(wb, "UseSVE"));
        checkEquals("MaxVectorSize", REQUIRED_VECTOR_LENGTH,
                intxFlag(wb, "MaxVectorSize"));
        checkEquals("minimum length", MIN_LENGTH,
                intFlag(wb, "StringEqualsIgnoreCaseIntrinsicMinLength"));

        for (Method helper : intrinsicHelpers()) {
            boolean available = wb.isIntrinsicAvailable(
                    helper, COMP_LEVEL_FULL_OPTIMIZATION);
            checkEquals(helper + " availability",
                    expectedEnabled, available);
        }

        Method[] wrappers = wrapperMethods();
        for (Method wrapper : wrappers) {
            wb.deoptimizeMethod(wrapper);
            wb.testSetDontInlineMethod(wrapper, true);
        }

        checkCases();
        warmup();
        for (Method wrapper : wrappers) {
            compileAndCheck(wb, wrapper);
        }
        checkCases();
        for (Method wrapper : wrappers) {
            checkCompiledAtLevel(wb, wrapper);
        }

        System.out.println("EIIC_PASS|"
                + (expectedEnabled ? "enabled" : "flag-off")
                + "|sink=" + sink);
    }

    private static void warmup() {
        for (int round = 0; round < WARMUP_ROUNDS; round++) {
            for (Case testCase : CASES) {
                sink ^= equalsWrapper(testCase.left(), testCase.right());
                String left = "p" + testCase.left() + "!";
                String right = "qq" + testCase.right() + "?";
                sink ^= regionWrapper(left, 1, right, 2,
                        testCase.left().length());
            }
        }
    }

    private static void checkCases() {
        for (Case testCase : CASES) {
            checkEquals(testCase.label() + " equalsIgnoreCase",
                    testCase.expected(),
                    equalsWrapper(testCase.left(), testCase.right()));
            String left = "p" + testCase.left() + "!";
            String right = "qq" + testCase.right() + "?";
            checkEquals(testCase.label() + " regionMatches",
                    testCase.expected(),
                    regionWrapper(left, 1, right, 2,
                            testCase.left().length()));
        }
    }

    private static Method[] intrinsicHelpers() throws Exception {
        Class<?> latin1 = Class.forName("java.lang.StringLatin1");
        Class<?> utf16 = Class.forName("java.lang.StringUTF16");
        Class<?>[] signature = {
                byte[].class, int.class, byte[].class, int.class, int.class
        };
        Method ll = latin1.getDeclaredMethod("regionMatchesCIResult", signature);
        Method lu = latin1.getDeclaredMethod(
                "regionMatchesCI_UTF16Result", signature);
        Method uu = utf16.getDeclaredMethod("regionMatchesCIResult", signature);
        ll.setAccessible(true);
        lu.setAccessible(true);
        uu.setAccessible(true);
        return new Method[] {ll, lu, uu};
    }

    private static Method[] wrapperMethods() throws Exception {
        return new Method[] {
                TestStringEqualsIgnoreCaseIntrinsic.class.getDeclaredMethod(
                        "equalsWrapper", String.class, String.class),
                TestStringEqualsIgnoreCaseIntrinsic.class.getDeclaredMethod(
                        "regionWrapper", String.class, int.class,
                        String.class, int.class, int.class)
        };
    }

    private static void compileAndCheck(WhiteBox wb, Method method)
            throws InterruptedException {
        if (!wb.enqueueMethodForCompilation(
                method, COMP_LEVEL_FULL_OPTIMIZATION)) {
            throw new AssertionError("failed to enqueue " + method);
        }
        for (int i = 0; i < 1_000 && !wb.isMethodCompiled(method); i++) {
            Thread.sleep(10);
        }
        checkCompiledAtLevel(wb, method);
    }

    private static void checkCompiledAtLevel(WhiteBox wb, Method method) {
        if (!wb.isMethodCompiled(method)) {
            throw new AssertionError(method + " was not compiled");
        }
        checkEquals(method + " compilation level",
                COMP_LEVEL_FULL_OPTIMIZATION,
                wb.getMethodCompilationLevel(method));
    }

    private static String replaceLast(String value, char replacement) {
        return value.substring(0, value.length() - 1) + replacement;
    }

    private static long uintFlag(WhiteBox wb, String name) {
        Long value = wb.getUintVMFlag(name);
        if (value == null) {
            throw new AssertionError("missing uint VM flag: " + name);
        }
        return value;
    }

    private static long intxFlag(WhiteBox wb, String name) {
        Long value = wb.getIntxVMFlag(name);
        if (value == null) {
            throw new AssertionError("missing intx VM flag: " + name);
        }
        return value;
    }

    private static long intFlag(WhiteBox wb, String name) {
        Long value = wb.getIntVMFlag(name);
        if (value == null) {
            throw new AssertionError("missing int VM flag: " + name);
        }
        return value;
    }

    private static boolean booleanFlag(WhiteBox wb, String name) {
        Boolean value = wb.getBooleanVMFlag(name);
        if (value == null) {
            throw new AssertionError("missing boolean VM flag: " + name);
        }
        return value;
    }

    private static void checkEquals(String label, boolean expected,
                                    boolean actual) {
        if (expected != actual) {
            throw new AssertionError(label + ": expected=" + expected
                    + ", actual=" + actual);
        }
    }

    private static void checkEquals(String label, long expected, long actual) {
        if (expected != actual) {
            throw new AssertionError(label + ": expected=" + expected
                    + ", actual=" + actual);
        }
    }

    private record Case(String label, String left, String right,
                        boolean expected) { }
}
