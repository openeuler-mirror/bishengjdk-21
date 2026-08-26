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
 * @summary Test AArch64 String.equalsIgnoreCase capability selection and flags.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.flagless
 * @requires vm.cpu.features ~= ".*sve.*"
 * @library /test/lib /
 * @modules java.base/java.lang:open
 * @build jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run driver compiler.intrinsics.string.TestStringEqualsIgnoreCaseOptions
 */

package compiler.intrinsics.string;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import jdk.test.whitebox.WhiteBox;

public class TestStringEqualsIgnoreCaseOptions {
    private static final String CLASS_NAME =
            TestStringEqualsIgnoreCaseOptions.class.getName();
    private static final String CHILD = "child";
    private static final String OPEN_JAVA_LANG =
            "--add-opens=java.base/java.lang=ALL-UNNAMED";
    private static final String ENABLE_FLAG = "UseStringEqualsIgnoreCaseIntrinsic";
    private static final String MIN_LENGTH_FLAG = "StringEqualsIgnoreCaseIntrinsicMinLength";
    private static final int DEFAULT_MIN_LENGTH = 16;
    private static final Pattern HISILICON_CPU = Pattern.compile(
            "(?i)^0x48:");
    private static final Pattern STATE_PATTERN = Pattern.compile(
            "(?m)^EIIC_STATE"
                    + "\\|case=([^|]+)"
                    + "\\|enabled=(true|false)"
                    + "\\|use_sve=(\\d+)"
                    + "\\|max_vector_size=(\\d+)"
                    + "\\|min_length=(\\d+)"
                    + "\\|sve=(true|false)"
                    + "\\|hisilicon=(true|false)\\r?$");

    public static void main(String[] args) throws Exception {
        if (args.length > 0 && CHILD.equals(args[0])) {
            runChild(args);
            return;
        }

        State defaults = runSuccess("default", "-XX:UseSVE=1");
        expectState(defaults, false, defaults.useSVE(), DEFAULT_MIN_LENGTH);

        State optimized = runSuccess("hisi-optimizations",
                "-XX:UseSVE=1", "-XX:+UseHisiOptimizations");
        expectState(optimized,
                optimized.hisilicon() && optimized.sve(),
                optimized.useSVE(), DEFAULT_MIN_LENGTH);

        State enabled = runSuccess("explicit-on",
                "-XX:UseSVE=1", enableFlag(true));
        expectState(enabled, enabled.hisilicon() && enabled.sve(),
                enabled.useSVE(), DEFAULT_MIN_LENGTH);

        State disabled = runSuccess("explicit-off-min-zero",
                "-XX:UseSVE=0", enableFlag(false), minLengthFlag(0));
        expectState(disabled, false, 0, 0);

        State noSVE = runSuccess("explicit-on-no-sve",
                "-XX:UseSVE=0", enableFlag(true));
        expectState(noSVE, false, 0, DEFAULT_MIN_LENGTH);

        State sve128 = runSuccess("explicit-on-sve128",
                "-XX:UseSVE=1", "-XX:MaxVectorSize=16",
                enableFlag(true));
        expectState(sve128, sve128.hisilicon() && sve128.sve(),
                sve128.useSVE(), DEFAULT_MIN_LENGTH);
        checkEquals("SVE 128-bit MaxVectorSize", 16,
                sve128.maxVectorSize());

        runRangeFailure(MIN_LENGTH_FLAG, "-1", minLengthFlag(-1));
    }

    private static State runSuccess(String testCase, String... flags)
            throws Exception {
        List<String> command = childCommand(flags);
        command.add(CLASS_NAME);
        command.add(CHILD);
        command.add(testCase);
        OutputAnalyzer output = ProcessTools.executeProcess(
                ProcessTools.createTestJavaProcessBuilder(
                        command.toArray(new String[0])));
        output.shouldHaveExitValue(0);
        return parseState(output.getOutput(), testCase);
    }

    private static void runRangeFailure(String flagName, String value,
                                        String option) throws Exception {
        OutputAnalyzer output = runFailingVM(option);
        output.shouldContain(flagName);
        output.shouldContain(value);
        String text = output.getOutput();
        if (!text.contains("outside the allowed range")
                && !text.contains("Improperly specified VM option")) {
            throw new AssertionError("missing range failure for " + option
                    + "\nOutput:\n" + text);
        }
    }

    private static OutputAnalyzer runFailingVM(String... flags)
            throws Exception {
        List<String> command = childCommand(flags);
        command.add("-version");
        OutputAnalyzer output = ProcessTools.executeProcess(
                ProcessTools.createTestJavaProcessBuilder(
                        command.toArray(new String[0])));
        output.shouldNotHaveExitValue(0);
        return output;
    }

    private static List<String> childCommand(String... flags) {
        List<String> command = new ArrayList<>();
        command.add("-Xbootclasspath/a:.");
        command.add(OPEN_JAVA_LANG);
        command.add("-XX:+UnlockDiagnosticVMOptions");
        command.add("-XX:+WhiteBoxAPI");
        command.addAll(Arrays.asList(flags));
        return command;
    }

    private static void runChild(String[] args) {
        if (args.length != 2) {
            throw new AssertionError("expected: child <case>");
        }
        WhiteBox wb = WhiteBox.getWhiteBox();
        boolean enabled = booleanFlag(wb, ENABLE_FLAG);
        long useSVE = uintFlag(wb, "UseSVE");
        long maxVectorSize = intxFlag(wb, "MaxVectorSize");
        long minLength = intFlag(wb, MIN_LENGTH_FLAG);
        checkInjectedConfiguration(enabled);
        String cpuInfo = wb.getCPUFeatures();
        Set<String> features = featureSet(cpuInfo);
        System.out.println("EIIC_STATE"
                + "|case=" + args[1]
                + "|enabled=" + enabled
                + "|use_sve=" + useSVE
                + "|max_vector_size=" + maxVectorSize
                + "|min_length=" + minLength
                + "|sve=" + features.contains("sve")
                + "|hisilicon=" + isHiSilicon(cpuInfo));
    }

    private static void checkInjectedConfiguration(boolean enabled) {
        try {
            Field enabledField = String.class.getDeclaredField(
                    "STRING_EQUALS_IGNORE_CASE_INTRINSICS");
            enabledField.setAccessible(true);
            checkEquals("injected intrinsic state", enabled,
                    enabledField.getBoolean(null));
        } catch (ReflectiveOperationException e) {
            throw new AssertionError("missing injected equalsIgnoreCase"
                    + " configuration", e);
        }
    }

    private static State parseState(String output, String expectedCase) {
        Matcher matcher = STATE_PATTERN.matcher(output);
        if (!matcher.find()) {
            throw new AssertionError("missing child state for " + expectedCase
                    + "\nOutput:\n" + output);
        }
        State state = new State(
                matcher.group(1),
                Boolean.parseBoolean(matcher.group(2)),
                Long.parseLong(matcher.group(3)),
                Long.parseLong(matcher.group(4)),
                Long.parseLong(matcher.group(5)),
                Boolean.parseBoolean(matcher.group(6)),
                Boolean.parseBoolean(matcher.group(7)));
        if (matcher.find()) {
            throw new AssertionError("multiple child states for "
                    + expectedCase + "\nOutput:\n" + output);
        }
        if (!expectedCase.equals(state.testCase())) {
            throw new AssertionError("child case: expected=" + expectedCase
                    + ", actual=" + state.testCase());
        }
        return state;
    }

    private static Set<String> featureSet(String features) {
        if (features == null || features.isEmpty()) {
            return Set.of();
        }
        return new HashSet<>(Arrays.asList(features.split(", ")));
    }

    private static boolean isHiSilicon(String cpuInfo) {
        return cpuInfo != null && HISILICON_CPU.matcher(cpuInfo).find();
    }

    private static String enableFlag(boolean enabled) {
        return "-XX:" + (enabled ? "+" : "-") + ENABLE_FLAG;
    }

    private static String minLengthFlag(long value) {
        return "-XX:" + MIN_LENGTH_FLAG + "=" + value;
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

    private static void expectState(State state, boolean enabled,
                                    long useSVE, long minLength) {
        checkEquals("enabled", enabled, state.enabled());
        checkEquals("effective UseSVE", useSVE, state.useSVE());
        checkEquals("minimum length", minLength, state.minLength());
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

    private record State(String testCase, boolean enabled, long useSVE,
            long maxVectorSize, long minLength, boolean sve,
            boolean hisilicon) { }
}
