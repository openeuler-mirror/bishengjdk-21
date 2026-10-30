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
 */

/*
 * @test
 * @summary Check AArch64 Integer.toString cache flags, thresholds, identity, and other-platform fallback
 * @requires vm.flagless & (vm.bits == 64)
 * @library /test/lib
 * @run driver IntegerToStringCache driver
 */

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jdk.test.lib.Platform;
import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class IntegerToStringCache {
    private static final String CACHE_THRESHOLD_PROPERTY = "java.lang.Integer.IntegerStringCache.threshold";

    public static void main(String[] args) throws Exception {
        // The no-argument entry also permits a minimal default-off RED run.
        if (args.length == 0) {
            checkValues(false, Platform.isAArch64() ? 128 : 0, true);
        } else if (args.length == 1 && args[0].equals("driver")) {
            runDriver();
        } else if (args.length == 4 && args[0].equals("probe")) {
            checkValues(Boolean.parseBoolean(args[1]), Integer.parseInt(args[2]),
                        Boolean.parseBoolean(args[3]));
        } else {
            throw new IllegalArgumentException("Expected driver or probe <enabled> <threshold> <compact>");
        }
    }

    private static void runDriver() throws Exception {
        // Use separate target-JDK VMs: Integer reads the effective threshold
        // once, and default-off must not inherit test.vm.opts or test.java.opts.
        System.out.println("Case default-off");
        new OutputAnalyzer(ProcessTools.createLimitedTestJavaProcessBuilder(
                IntegerToStringCache.class.getName()).start()).shouldHaveExitValue(0);
        if (!Platform.isAArch64()) {
            runCase("other-platform-utf16", false, 0, false);
            checkUnavailableFlag("UseIntegerToStringCache", "-XX:+UseIntegerToStringCache");
            checkUnavailableFlag("IntegerToStringCacheThreshold", "-XX:IntegerToStringCacheThreshold=128");
            return;
        }
        runCase("private-property-off", false, 128, true,
                "-XX:-UseIntegerToStringCache", "-D" + CACHE_THRESHOLD_PROPERTY + "=2000");
        runCase("private-property-on", true, 128, true,
                "-XX:+UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=128",
                "-D" + CACHE_THRESHOLD_PROPERTY + "=2000");
        runCase("explicit-off", false, 128, true,
                "-XX:-UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=128");
        runCase("on-default-threshold", true, 128, true, "-XX:+UseIntegerToStringCache");
        for (int threshold : new int[] { 0, 1, 127, 128, 129, 150, 2_000 }) {
            runCase("on-" + threshold, true, threshold, true,
                    "-XX:+UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=" + threshold);
        }
        runCase("on-five-digits", true, 10_001, true,
                "-XX:+UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=10001");
        runCase("on-utf16", true, 2_000, false,
                "-XX:+UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=2000");
        checkInvalidThreshold("-1");
        checkInvalidThreshold("2147483648");
    }

    private static void runCase(String name, boolean enabled, int threshold, boolean compact,
                                String... flags) throws Exception {
        List<String> command = new ArrayList<>(Arrays.asList(flags));
        command.add(compact ? "-XX:+CompactStrings" : "-XX:-CompactStrings");
        command.add(IntegerToStringCache.class.getName());
        command.add("probe");
        command.add(Boolean.toString(enabled));
        command.add(Integer.toString(threshold));
        command.add(Boolean.toString(compact));
        System.out.println("Case " + name);
        new OutputAnalyzer(ProcessTools.createLimitedTestJavaProcessBuilder(command).start())
                .shouldHaveExitValue(0);
    }

    private static void checkInvalidThreshold(String value) throws Exception {
        System.out.println("Case invalid-threshold-" + value);
        new OutputAnalyzer(ProcessTools.createLimitedTestJavaProcessBuilder(
                "-XX:IntegerToStringCacheThreshold=" + value, "-version").start())
                .shouldHaveExitValue(1)
                .shouldContain("IntegerToStringCacheThreshold=" + value)
                .shouldContain("outside the allowed range")
                .shouldNotContain("Unrecognized VM option");
    }

    private static void checkUnavailableFlag(String name, String flag) throws Exception {
        System.out.println("Case unavailable-flag-" + name);
        new OutputAnalyzer(ProcessTools.createLimitedTestJavaProcessBuilder(flag, "-version").start())
                .shouldHaveExitValue(1)
                .shouldContain("Unrecognized VM option")
                .shouldContain(name);
    }

    private static void checkValues(boolean enabled, int threshold, boolean compact) {
        if (System.getProperty(CACHE_THRESHOLD_PROPERTY) != null ||
                System.getProperties().containsKey(CACHE_THRESHOLD_PROPERTY)) {
            throw new AssertionError("Private Integer.toString cache property must not be public: " +
                                     CACHE_THRESHOLD_PROPERTY);
        }
        // Changing the public property before the first value check must not
        // alter the private configuration saved during VM initialization.
        System.setProperty(CACHE_THRESHOLD_PROPERTY, "2000");
        try {
            // This includes both sides of every tested exclusive threshold, 10000
            // with five cached digits, and the first non-cached value 10001.
            for (int value = 0; value <= Math.max(2_000, threshold); value++) {
                checkValue(value, enabled, threshold, compact);
            }
            for (int value : new int[] { -1, -9, -10, -99, -100, -127, -128, -129, -999, -1_000, -10_000,
                                         Integer.MIN_VALUE, Integer.MAX_VALUE }) {
                checkValue(value, enabled, threshold, compact);
            }
        } finally {
            System.clearProperty(CACHE_THRESHOLD_PROPERTY);
        }
    }

    private static void checkValue(int value, boolean enabled, int threshold, boolean compact) {
        String expected = Long.toString((long) value);
        String first = Integer.toString(value);
        String second = Integer.toString(value);
        if (!expected.equals(first) || !expected.equals(second)) {
            throw new AssertionError("Incorrect Integer.toString content for value " + value);
        }
        boolean shared = enabled && compact && value >= 0 && value < threshold;
        if (shared != (first == second)) {
            String identity = shared ? "the same String instance" : "distinct String instances";
            throw new AssertionError("Integer.toString(" + value + ") must return " + identity +
                                     " (enabled=" + enabled + ", threshold=" + threshold +
                                     ", compact=" + compact + ")");
        }
        byte[] copy = first.getBytes(StandardCharsets.ISO_8859_1);
        Arrays.fill(copy, (byte) 'x');
        if (!expected.equals(first) || !expected.equals(second) ||
                !expected.equals(Integer.toString(value))) {
            throw new AssertionError("Mutating getBytes copy changed Integer.toString value " + value);
        }
    }
}
