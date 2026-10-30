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
 * @summary Integer and Long own their String cache gates; disabled caches do not initialize their holders
 * @requires vm.flagless & (vm.bits == 64) & (os.arch == "aarch64")
 * @library /test/lib
 * @run driver IntegerLongStringCacheInitialization
 */

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class IntegerLongStringCacheInitialization {
    private static final String INTEGER_INITIALIZATION =
            "Initializing 'java/lang/Integer$IntegerStringCache'";
    private static final String LONG_INITIALIZATION =
            "Initializing 'java/lang/Long$LongStringCache'";

    public static void main(String[] args) throws Exception {
        if (args.length == 3 && args[0].equals("probe")) {
            probe(Boolean.parseBoolean(args[1]), Boolean.parseBoolean(args[2]));
            return;
        }
        if (args.length != 0) {
            throw new IllegalArgumentException("Expected no arguments or probe <integer> <long>");
        }

        checkFieldOwnership();

        // Also exercise the default cache flags and default CDS mode.
        checkChild("defaults", List.of(), false, false);
        for (boolean interpreted : new boolean[] { false, true }) {
            String mode = interpreted ? "interpreted" : "mixed";
            for (boolean integer : new boolean[] { false, true }) {
                for (boolean longCache : new boolean[] { false, true }) {
                    runCase(mode + "-integer-" + integer + "-long-" + longCache,
                            interpreted, integer, longCache, 128, true);
                }
            }
            runCase(mode + "-zero-threshold", interpreted, true, true, 0, true);
            runCase(mode + "-utf16", interpreted, true, true, 128, false);
        }
    }

    private static void checkFieldOwnership() {
        // Inspect only the outer classes' field metadata, never the cache holders.
        checkOwnedFlag(Integer.class);
        checkOwnedFlag(Long.class);
        for (String name : new String[] { "INTEGER_TO_STRING_CACHE", "LONG_TO_STRING_CACHE" }) {
            try {
                String.class.getDeclaredField(name);
            } catch (NoSuchFieldException expected) {
                continue;
            }
            throw new AssertionError("java.lang.String must not own the numeric cache flag " + name);
        }
    }

    private static void checkOwnedFlag(Class<?> owner) {
        Field flag;
        try {
            flag = owner.getDeclaredField("USE_TO_STRING_CACHE");
        } catch (NoSuchFieldException e) {
            throw new AssertionError(owner.getName() +
                    " must declare private static final boolean USE_TO_STRING_CACHE", e);
        }
        int modifiers = flag.getModifiers();
        if (flag.getType() != boolean.class || !Modifier.isPrivate(modifiers) ||
                !Modifier.isStatic(modifiers) || !Modifier.isFinal(modifiers)) {
            throw new AssertionError(owner.getName() +
                    ".USE_TO_STRING_CACHE must be private static final boolean");
        }
    }

    private static void runCase(String name, boolean interpreted, boolean integer,
                                boolean longCache, int threshold, boolean compact) throws Exception {
        List<String> flags = new ArrayList<>();
        flags.add("-Xshare:off");
        if (interpreted) {
            flags.add("-Xint");
        }
        flags.add(integer ? "-XX:+UseIntegerToStringCache" : "-XX:-UseIntegerToStringCache");
        flags.add(longCache ? "-XX:+UseLongToStringCache" : "-XX:-UseLongToStringCache");
        flags.add("-XX:IntegerToStringCacheThreshold=" + threshold);
        flags.add("-XX:LongToStringCacheThreshold=" + threshold);
        flags.add(compact ? "-XX:+CompactStrings" : "-XX:-CompactStrings");
        checkChild(name, flags, integer && compact && threshold > 0,
                   longCache && compact && threshold > 0);
    }

    private static void checkChild(String name, List<String> flags,
                                   boolean integer, boolean longCache) throws Exception {
        List<String> command = new ArrayList<>(flags);
        command.add("-Xlog:class+init=info");
        command.add(IntegerLongStringCacheInitialization.class.getName());
        command.add("probe");
        command.add(Boolean.toString(integer));
        command.add(Boolean.toString(longCache));
        System.out.println("Case " + name);
        OutputAnalyzer out = new OutputAnalyzer(
                ProcessTools.createLimitedTestJavaProcessBuilder(command).start());
        out.shouldHaveExitValue(0).shouldContain("PROBE PASSED").shouldContain("[class,init]");
        checkInitialization(out, INTEGER_INITIALIZATION, integer);
        checkInitialization(out, LONG_INITIALIZATION, longCache);
    }

    private static void checkInitialization(OutputAnalyzer out, String initialization, boolean expected) {
        if (expected) {
            out.shouldContain(initialization);
        } else {
            out.shouldNotContain(initialization);
        }
    }

    private static void probe(boolean integer, boolean longCache) {
        // No reflective lookup or class literal may initialize a cache holder.
        // The repeated public conversions must trigger every enabled cache.
        int[] values = { 42, 127, 128 };
        String[] expected = { "42", "127", "128" };
        for (int index = 0; index < values.length; index++) {
            int value = values[index];
            String integerFirst = Integer.toString(value);
            String integerSecond = Integer.toString(value);
            checkResult(expected[index], integerFirst, integerSecond,
                        integer && value < 128, "Integer");
            String longFirst = Long.toString((long) value);
            String longSecond = Long.toString((long) value);
            checkResult(expected[index], longFirst, longSecond,
                        longCache && value < 128, "Long");
        }
        System.out.println("PROBE PASSED");
    }

    private static void checkResult(String expected, String first, String second,
                                    boolean shared, String type) {
        if (!expected.equals(first) || !expected.equals(second)) {
            throw new AssertionError(type + ".toString content differs for " + expected);
        }
        if ((first == second) != shared) {
            throw new AssertionError(type + ".toString identity differs for " + expected +
                                     "; expected shared=" + shared);
        }
    }
}
