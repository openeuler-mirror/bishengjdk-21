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
 * @summary Check AArch64 Long decimal String cache flags, thresholds, identity, and other-platform fallback
 * @requires vm.flagless & (vm.bits == 64)
 * @library /test/lib
 * @modules jdk.management
 * @run driver LongToStringCache driver
 */

import com.sun.management.HotSpotDiagnosticMXBean;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import jdk.test.lib.Platform;
import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class LongToStringCache {
    private static final String CACHE_THRESHOLD_PROPERTY = "java.lang.Long.LongStringCache.threshold";

    public static void main(String[] args) throws Exception {
        // The no-argument probe also detects the absent product flags for RED.
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
        System.out.println("Case default-off");
        new OutputAnalyzer(ProcessTools.createLimitedTestJavaProcessBuilder(
                LongToStringCache.class.getName()).start()).shouldHaveExitValue(0);
        if (!Platform.isAArch64()) {
            runCase("other-platform-utf16", false, 0, false);
            checkUnavailableFlag("UseLongToStringCache", "-XX:+UseLongToStringCache");
            checkUnavailableFlag("LongToStringCacheThreshold", "-XX:LongToStringCacheThreshold=128");
            return;
        }
        runCase("private-property-off", false, 128, true,
                "-XX:-UseLongToStringCache", "-D" + CACHE_THRESHOLD_PROPERTY + "=2000");
        runCase("private-property-on", true, 128, true,
                "-XX:+UseLongToStringCache", "-XX:LongToStringCacheThreshold=128",
                "-D" + CACHE_THRESHOLD_PROPERTY + "=2000");
        runCase("explicit-off", false, 128, true,
                "-XX:-UseLongToStringCache", "-XX:LongToStringCacheThreshold=128");
        runCase("on-default-threshold", true, 128, true, "-XX:+UseLongToStringCache");
        for (int threshold : new int[] { 0, 1, 127, 128, 129, 150, 65_536 }) {
            runCase("on-" + threshold, true, threshold, true,
                    "-XX:+UseLongToStringCache", "-XX:LongToStringCacheThreshold=" + threshold);
        }
        runCase("on-six-digits", true, 100_001, true,
                "-XX:+UseLongToStringCache", "-XX:LongToStringCacheThreshold=100001");
        runCase("on-utf16", true, 65_536, false,
                "-XX:+UseLongToStringCache", "-XX:LongToStringCacheThreshold=65536");
        runCase("both-caches-default", true, 128, true,
                "-XX:+UseLongToStringCache", "-XX:+UseIntegerToStringCache");
        // Also cover the larger cache sizes used by the application.
        runCase("both-caches-on", true, 65_536, true,
                "-XX:+UseLongToStringCache", "-XX:LongToStringCacheThreshold=65536",
                "-XX:+UseIntegerToStringCache", "-XX:IntegerToStringCacheThreshold=150");
        checkInvalidThreshold("-1");
        checkInvalidThreshold("2147483648");
    }

    private static void runCase(String name, boolean enabled, int threshold, boolean compact,
                                String... flags) throws Exception {
        List<String> command = new ArrayList<>(Arrays.asList(flags));
        command.add(compact ? "-XX:+CompactStrings" : "-XX:-CompactStrings");
        command.add(LongToStringCache.class.getName());
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
                "-XX:LongToStringCacheThreshold=" + value, "-version").start())
                .shouldHaveExitValue(1)
                .shouldContain("LongToStringCacheThreshold=" + value)
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
            throw new AssertionError("Private Long.toString cache property must not be public: " +
                                     CACHE_THRESHOLD_PROPERTY);
        }
        // The public property must not replace the private VM configuration,
        // including when changed before the first value assertion.
        System.setProperty(CACHE_THRESHOLD_PROPERTY, "2000");
        try {
            HotSpotDiagnosticMXBean bean = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean.class);
            try {
                if (Platform.isAArch64() &&
                        (Boolean.parseBoolean(bean.getVMOption("UseLongToStringCache").getValue()) != enabled ||
                         Integer.parseInt(bean.getVMOption("LongToStringCacheThreshold").getValue()) != threshold)) {
                    throw new AssertionError("Unexpected Long String cache flags");
                }
                if (Boolean.parseBoolean(bean.getVMOption("CompactStrings").getValue()) != compact) {
                    throw new AssertionError("Unexpected CompactStrings flag");
                }
            } catch (IllegalArgumentException e) {
                throw new AssertionError("Missing Long String cache product flag", e);
            }
            // Exhaust the application mask range and every configured threshold boundary.
            for (long value = 0; value <= Math.max(65_536, threshold); value++) {
                checkValue(value, enabled, threshold, compact);
            }
            for (long value : new long[] { -1, -9, -10, -99, -100, -999, -1_000, -10_000,
                    -65_535, -65_536, 99_999, 100_000, 100_001, 1_000_000, 99_999_999,
                    100_000_000, 4_294_967_296L, 4_294_967_423L,
                    1_700_000_000_000L, Long.MIN_VALUE, Long.MAX_VALUE }) {
                checkValue(value, enabled, threshold, compact);
            }
            Random random = new Random(0x4c4f4e4743414348L);
            for (int i = 0; i < 1_000; i++) {
                checkValue(random.nextLong(), enabled, threshold, compact);
            }
        } finally {
            System.clearProperty(CACHE_THRESHOLD_PROPERTY);
        }
    }

    private static void checkValue(long value, boolean enabled, int threshold, boolean compact) {
        String expected = decimalOracle(value);
        String first = Long.toString(value);
        String second = Long.toString(value);
        boolean shared = enabled && compact && value >= 0 && value < threshold;
        checkResult(value, expected, first, second, shared, "Long.toString");
        String[] routed = { Long.toString(value, 10), Long.toString(value, 1),
                Long.valueOf(value).toString(), String.valueOf(value) };
        String[] repeated = { Long.toString(value, 10), Long.toString(value, 1),
                Long.valueOf(value).toString(), String.valueOf(value) };
        for (int i = 0; i < routed.length; i++) {
            checkResult(value, expected, routed[i], repeated[i], shared, "decimal route " + i);
            if (shared && first != routed[i]) {
                throw new AssertionError("Decimal route does not share Long cached object for value " + value);
            }
        }
        byte[] copy = first.getBytes(StandardCharsets.ISO_8859_1);
        Arrays.fill(copy, (byte) 'x');
        if (!expected.equals(first) || !expected.equals(second) ||
                !expected.equals(Long.toString(value))) {
            throw new AssertionError("Mutating getBytes copy changed Long cached content for value " + value);
        }
    }

    private static void checkResult(long value, String expected, String first, String second,
                                    boolean shared, String route) {
        if (!expected.equals(first) || !expected.equals(second)) {
            throw new AssertionError("Incorrect " + route + " content for value " + expected);
        }
        if (shared != (first == second)) {
            throw new AssertionError(route + " identity differs for value " + expected +
                    "; expected shared=" + shared);
        }
    }

    // No Integer/Long formatting helper is used by the independent content oracle.
    private static String decimalOracle(long value) {
        char[] digits = new char[20];
        boolean negative = value < 0;
        long remaining = negative ? value : -value;
        int pos = digits.length;
        do {
            digits[--pos] = (char) ('0' - remaining % 10);
            remaining /= 10;
        } while (remaining != 0);
        if (negative) {
            digits[--pos] = '-';
        }
        return new String(digits, pos, digits.length - pos);
    }
}
