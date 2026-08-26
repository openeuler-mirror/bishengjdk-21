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
 * @summary Verify AArch64 C2 IR for the String.equalsIgnoreCase helper intrinsics.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.debug == true & vm.flagless
 * @requires vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @build jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver compiler.intrinsics.string.TestStringEqualsIgnoreCaseIntrinsicIR
 */

package compiler.intrinsics.string;

import java.lang.reflect.Method;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import compiler.lib.ir_framework.IR;
import compiler.lib.ir_framework.IRNode;
import compiler.lib.ir_framework.Run;
import compiler.lib.ir_framework.Test;
import compiler.lib.ir_framework.TestFramework;
import jtreg.SkippedException;
import jdk.test.lib.Asserts;
import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

public class TestStringEqualsIgnoreCaseIntrinsicIR {
    private static final String CLASS_NAME =
            TestStringEqualsIgnoreCaseIntrinsicIR.class.getName();
    private static final String PREFLIGHT = "preflight";
    private static final int REQUIRED_VECTOR_LENGTH = 32;
    static final int TEST_MIN_LENGTH = 16;
    private static final Pattern PREFLIGHT_MARKER = Pattern.compile(
            "(?m)^EIIC_PREFLIGHT\\|enabled=(true|false)"
                    + "\\|use_sve=(\\d+)\\|vl=(\\d+)\\r?$");
    private static final String[] COMMON_FLAGS = {
            "-XX:+UnlockDiagnosticVMOptions",
            "-XX:UseSVE=1",
            "-XX:MaxVectorSize=" + REQUIRED_VECTOR_LENGTH,
            "-XX:StringEqualsIgnoreCaseIntrinsicMinLength=" + TEST_MIN_LENGTH
    };

    public static void main(String[] args) throws Exception {
        if (args.length > 0 && PREFLIGHT.equals(args[0])) {
            runPreflightChild();
            return;
        }
        requireSupportedConfiguration();
        // Keep each positive coder family in its own VM.  The branch profile
        // of the inlined String.regionMatches method is otherwise shared by
        // all callers and could retain unrelated coder branches in the IR.
        run(EqualsIgnoreCaseIRPositiveLL.class,
                "-XX:+UseStringEqualsIgnoreCaseIntrinsic");
        run(EqualsIgnoreCaseIRPositiveLU.class,
                "-XX:+UseStringEqualsIgnoreCaseIntrinsic");
        run(EqualsIgnoreCaseIRPositiveUL.class,
                "-XX:+UseStringEqualsIgnoreCaseIntrinsic");
        run(EqualsIgnoreCaseIRPositiveUU.class,
                "-XX:+UseStringEqualsIgnoreCaseIntrinsic");
        run(EqualsIgnoreCaseIRFlagOff.class,
                "-XX:-UseStringEqualsIgnoreCaseIntrinsic");
    }

    private static void requireSupportedConfiguration() throws Exception {
        OutputAnalyzer output = ProcessTools.executeTestJava(
                "-Xbootclasspath/a:.",
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:+WhiteBoxAPI",
                "-XX:UseSVE=1",
                "-XX:MaxVectorSize=" + REQUIRED_VECTOR_LENGTH,
                "-XX:+UseStringEqualsIgnoreCaseIntrinsic",
                CLASS_NAME,
                PREFLIGHT).shouldHaveExitValue(0);
        System.out.print(output.getStdout());

        Matcher marker = PREFLIGHT_MARKER.matcher(output.getStdout());
        if (!marker.find()) {
            throw new AssertionError("Missing equalsIgnoreCase preflight marker");
        }
        boolean enabled = Boolean.parseBoolean(marker.group(1));
        int useSVE = Integer.parseInt(marker.group(2));
        int vectorLength = Integer.parseInt(marker.group(3));
        if (!enabled || useSVE != 1
                || vectorLength != REQUIRED_VECTOR_LENGTH) {
            throw new SkippedException(
                    "String.equalsIgnoreCase SVE intrinsic requires effective"
                            + " UseStringEqualsIgnoreCaseIntrinsic=true,"
                            + " UseSVE=1 and MaxVectorSize="
                            + REQUIRED_VECTOR_LENGTH + "; preflight reported"
                            + " UseStringEqualsIgnoreCaseIntrinsic=" + enabled
                            + ", UseSVE=" + useSVE
                            + ", MaxVectorSize=" + vectorLength);
        }
    }

    private static void runPreflightChild() {
        WhiteBox wb = WhiteBox.getWhiteBox();
        Boolean enabled =
                wb.getBooleanVMFlag("UseStringEqualsIgnoreCaseIntrinsic");
        Long useSVE = wb.getUintVMFlag("UseSVE");
        Long vectorLength = wb.getIntxVMFlag("MaxVectorSize");
        if (enabled == null || useSVE == null || vectorLength == null) {
            throw new AssertionError(
                    "Missing String.equalsIgnoreCase preflight VM flag");
        }
        System.out.println("EIIC_PREFLIGHT|enabled=" + enabled
                + "|use_sve=" + useSVE
                + "|vl=" + vectorLength);
    }

    private static void run(Class<?> testClass, String... extraFlags) {
        String[] flags = new String[COMMON_FLAGS.length + extraFlags.length];
        System.arraycopy(COMMON_FLAGS, 0, flags, 0, COMMON_FLAGS.length);
        System.arraycopy(extraFlags, 0, flags, COMMON_FLAGS.length,
                extraFlags.length);
        new TestFramework(testClass).addFlags(flags).start();
        System.out.println("EIIC_IR|class=" + testClass.getSimpleName()
                + "|verified=true");
    }
}

class EqualsIgnoreCaseIRData {
    static final int BELOW_THRESHOLD =
            TestStringEqualsIgnoreCaseIntrinsicIR.TEST_MIN_LENGTH - 1;
    static final int AT_THRESHOLD =
            TestStringEqualsIgnoreCaseIntrinsicIR.TEST_MIN_LENGTH;
    static final int ABOVE_THRESHOLD =
            TestStringEqualsIgnoreCaseIntrinsicIR.TEST_MIN_LENGTH + 1;

    private static final WhiteBox WB = WhiteBox.getWhiteBox();
    private static final Method REGION_MATCHES = regionMatchesMethod();

    // These are deliberately non-final and are passed to every @Test method.
    // The compiler therefore sees dynamic String arguments rather than
    // constant String objects whose comparison could be folded away.
    String llLeft = new String("abcdefghijklmnopq");
    String llRight = new String("ABCDEFGHIJKLMNOPQ");

    String luLeft = new String("abcdefghijklmnopq");
    String luRight = new String("ABCDEFGHIJKLMNOPQ\u0100");

    String ulLeft = new String("abcdefghijklmnopq\u0100");
    String ulRight = new String("ABCDEFGHIJKLMNOPQ");

    String uuLeft = new String("\u0100bcdefghijklmnopq");
    String uuRight = new String("\u0101BCDEFGHIJKLMNOPQ");

    EqualsIgnoreCaseIRData() {
        // Startup code can use equalsIgnoreCase before this test begins.  Drop
        // any such compiled code and profile so this family-specific VM builds
        // a clean, one-coder branch profile during the framework warmup.
        WB.deoptimizeMethod(REGION_MATCHES);
        WB.clearMethodState(REGION_MATCHES);
    }

    private static Method regionMatchesMethod() {
        try {
            return String.class.getMethod("regionMatches", boolean.class,
                    int.class, String.class, int.class, int.class);
        } catch (ReflectiveOperationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    static void assertMatch(boolean result, String description) {
        Asserts.assertTrue(result, description);
    }
}

class EqualsIgnoreCaseIRPositiveLL extends EqualsIgnoreCaseIRData {
    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantBelow(String left, String right) {
        return left.regionMatches(true, 0, right, 0, BELOW_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantAt(String left, String right) {
        return left.regionMatches(true, 0, right, 0, AT_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testDynamic(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Run(test = {"testConstantBelow", "testConstantAt", "testDynamic"})
    void runTests() {
        assertMatch(testConstantBelow(llLeft, llRight),
                "LL length " + BELOW_THRESHOLD);
        assertMatch(testConstantAt(llLeft, llRight),
                "LL length " + AT_THRESHOLD);
        for (int len = BELOW_THRESHOLD; len <= ABOVE_THRESHOLD; len++) {
            assertMatch(testDynamic(llLeft, llRight, len),
                    "LL dynamic length " + len);
        }
    }
}

class EqualsIgnoreCaseIRPositiveLU extends EqualsIgnoreCaseIRData {
    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantBelow(String left, String right) {
        return left.regionMatches(true, 0, right, 0, BELOW_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantAt(String left, String right) {
        return left.regionMatches(true, 0, right, 0, AT_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testDynamic(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Run(test = {"testConstantBelow", "testConstantAt", "testDynamic"})
    void runTests() {
        assertMatch(testConstantBelow(luLeft, luRight),
                "LU length " + BELOW_THRESHOLD);
        assertMatch(testConstantAt(luLeft, luRight),
                "LU length " + AT_THRESHOLD);
        for (int len = BELOW_THRESHOLD; len <= ABOVE_THRESHOLD; len++) {
            assertMatch(testDynamic(luLeft, luRight, len),
                    "LU dynamic length " + len);
        }
    }
}

class EqualsIgnoreCaseIRPositiveUL extends EqualsIgnoreCaseIRData {
    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantBelow(String left, String right) {
        return left.regionMatches(true, 0, right, 0, BELOW_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantAt(String left, String right) {
        return left.regionMatches(true, 0, right, 0, AT_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testDynamic(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Run(test = {"testConstantBelow", "testConstantAt", "testDynamic"})
    void runTests() {
        assertMatch(testConstantBelow(ulLeft, ulRight),
                "UL length " + BELOW_THRESHOLD);
        assertMatch(testConstantAt(ulLeft, ulRight),
                "UL length " + AT_THRESHOLD);
        for (int len = BELOW_THRESHOLD; len <= ABOVE_THRESHOLD; len++) {
            assertMatch(testDynamic(ulLeft, ulRight, len),
                    "UL dynamic length " + len);
        }
    }
}

class EqualsIgnoreCaseIRPositiveUU extends EqualsIgnoreCaseIRData {
    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testConstantBelow(String left, String right) {
        return left.regionMatches(true, 0, right, 0, BELOW_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseUU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU"})
    static boolean testConstantAt(String left, String right) {
        return left.regionMatches(true, 0, right, 0, AT_THRESHOLD);
    }

    @Test
    @IR(counts = {IRNode.CALL_OF, "stringEqualsIgnoreCaseUU", "1"},
        failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU"})
    static boolean testDynamic(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Run(test = {"testConstantBelow", "testConstantAt", "testDynamic"})
    void runTests() {
        assertMatch(testConstantBelow(uuLeft, uuRight),
                "UU length " + BELOW_THRESHOLD);
        assertMatch(testConstantAt(uuLeft, uuRight),
                "UU length " + AT_THRESHOLD);
        for (int len = BELOW_THRESHOLD; len <= ABOVE_THRESHOLD; len++) {
            assertMatch(testDynamic(uuLeft, uuRight, len),
                    "UU dynamic length " + len);
        }
    }
}

class EqualsIgnoreCaseIRFlagOff extends EqualsIgnoreCaseIRData {
    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testLL(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testLU(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testUL(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Test
    @IR(failOn = {IRNode.CALL_OF, "stringEqualsIgnoreCaseLL",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseLU",
                  IRNode.CALL_OF, "stringEqualsIgnoreCaseUU"})
    static boolean testUU(String left, String right, int len) {
        return left.regionMatches(true, 0, right, 0, len);
    }

    @Run(test = {"testLL", "testLU", "testUL", "testUU"})
    void runTests() {
        runDynamicLengths();
    }

    private void runDynamicLengths() {
        for (int len = BELOW_THRESHOLD; len <= ABOVE_THRESHOLD; len++) {
            assertMatch(testLL(llLeft, llRight, len),
                    "flag off LL length " + len);
            assertMatch(testLU(luLeft, luRight, len),
                    "flag off LU length " + len);
            assertMatch(testUL(ulLeft, ulRight, len),
                    "flag off UL length " + len);
            assertMatch(testUU(uuLeft, uuRight, len),
                    "flag off UU length " + len);
        }
    }
}
