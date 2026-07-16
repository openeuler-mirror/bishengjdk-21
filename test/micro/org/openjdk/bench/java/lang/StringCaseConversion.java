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

package org.openjdk.bench.java.lang;

import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

import com.sun.management.HotSpotDiagnosticMXBean;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
public abstract class StringCaseConversion {
    private static final int EXPECTED_MIN_LENGTH = 8;
    private static final byte LATIN1 = 0;
    private static final byte UTF16 = 1;
    private static final String EXPECTED_BACKEND_PROPERTY =
            "stringCaseExpectedBackend";
    private static final String EXPECTED_USE_SVE_PROPERTY =
            "stringCaseExpectedUseSVE";
    private static final String UNLOCK_DIAGNOSTIC =
            "-XX:+UnlockDiagnosticVMOptions";
    private static final String OPEN_JAVA_LANG =
            "--add-opens=java.base/java.lang=ALL-UNNAMED";
    private static final String COMPACT_STRINGS = "-XX:+CompactStrings";
    private static final String MIN_LENGTH = "-XX:StringCaseIntrinsicMinLength=8";
    private static final String BACKEND_OFF =
            "-XX:StringCaseIntrinsicBackend=0";
    private static final String BACKEND_SVE =
            "-XX:StringCaseIntrinsicBackend=1";
    private static final String BACKEND_SVE2 =
            "-XX:StringCaseIntrinsicBackend=2";
    private static final String USE_SVE = "-XX:UseSVE=1";
    private static final String USE_SVE2 = "-XX:UseSVE=2";
    private static final String EXPECT_OFF =
            "-D" + EXPECTED_BACKEND_PROPERTY + "=0";
    private static final String EXPECT_SVE =
            "-D" + EXPECTED_BACKEND_PROPERTY + "=1";
    private static final String EXPECT_SVE2 =
            "-D" + EXPECTED_BACKEND_PROPERTY + "=2";
    private static final String EXPECT_USE_SVE =
            "-D" + EXPECTED_USE_SVE_PROPERTY + "=1";
    private static final String EXPECT_USE_SVE2 =
            "-D" + EXPECTED_USE_SVE_PROPERTY + "=2";

    @Param({"8", "16", "64", "256", "1024", "4096"})
    private int convertedLength;

    private String latin1Upper;
    private String latin1Lower;
    private String latin1ExtendedUpper;
    private String latin1ExtendedLower;
    private String latin1UpperSpecialFallback;
    private String utf16UpperSuffix;
    private String utf16LowerSuffix;
    private String utf16UpperSuffixFallback;
    private String utf16BmpUpper;
    private String utf16BmpLower;
    private String utf16BmpLowerFallback;
    private String utf16BmpUpperFallback;

    @Setup
    public void setup() throws ReflectiveOperationException {
        int expectedBackend = Integer.getInteger(EXPECTED_BACKEND_PROPERTY, -1);
        HotSpotDiagnosticMXBean bean = ManagementFactory.getPlatformMXBean(
                HotSpotDiagnosticMXBean.class);
        int actualBackend = Integer.parseInt(
                bean.getVMOption("StringCaseIntrinsicBackend").getValue());
        int expectedUseSVE = Integer.getInteger(EXPECTED_USE_SVE_PROPERTY, -1);
        int actualUseSVE = Integer.parseInt(
                bean.getVMOption("UseSVE").getValue());
        boolean actualCompactStrings = Boolean.parseBoolean(
                bean.getVMOption("CompactStrings").getValue());
        int actualMinLength = Integer.parseInt(
                bean.getVMOption("StringCaseIntrinsicMinLength").getValue());
        if (actualBackend != expectedBackend) {
            throw new IllegalStateException("StringCaseIntrinsicBackend=" + actualBackend
                    + ", expected " + expectedBackend
                    + "; selected benchmark is unsupported on this CPU");
        }
        if (actualUseSVE != expectedUseSVE) {
            throw new IllegalStateException("UseSVE=" + actualUseSVE
                    + ", expected " + expectedUseSVE
                    + "; selected benchmark is unsupported on this CPU");
        }
        if (!actualCompactStrings) {
            throw new IllegalStateException("CompactStrings must be enabled");
        }
        if (actualMinLength != EXPECTED_MIN_LENGTH) {
            throw new IllegalStateException("StringCaseIntrinsicMinLength="
                    + actualMinLength + ", expected " + EXPECTED_MIN_LENGTH);
        }
        if (convertedLength < EXPECTED_MIN_LENGTH) {
            throw new IllegalArgumentException("convertedLength=" + convertedLength
                    + " is below StringCaseIntrinsicMinLength="
                    + EXPECTED_MIN_LENGTH);
        }

        Method stringCoder = String.class.getDeclaredMethod("coder");
        stringCoder.setAccessible(true);

        latin1Upper = repeatAscii('A', 'Z', convertedLength);
        latin1Lower = repeatAscii('a', 'z', convertedLength);
        latin1ExtendedUpper = repeatLatin1Case(true, convertedLength);
        latin1ExtendedLower = repeatLatin1Case(false, convertedLength);
        latin1UpperSpecialFallback = withLastChar(latin1ExtendedLower, '\u00df');
        utf16UpperSuffix = '\u4e00' + repeatAscii('A', 'Z', convertedLength);
        utf16LowerSuffix = '\u4e00' + repeatAscii('a', 'z', convertedLength);
        utf16UpperSuffixFallback = withLastChar(utf16UpperSuffix, '\u03a3');
        utf16BmpUpper = repeatBmpCase(true, convertedLength);
        utf16BmpLower = repeatBmpCase(false, convertedLength);
        utf16BmpLowerFallback = withLastChar(utf16BmpUpper, '\u03a3');
        utf16BmpUpperFallback = withLastChar(utf16BmpLower, '\u1f80');

        assertWorkload(stringCoder, "latin1Upper", latin1Upper, LATIN1,
                true, latin1Lower, 0);
        assertWorkload(stringCoder, "latin1Lower", latin1Lower, LATIN1,
                false, latin1Upper, 0);
        assertWorkload(stringCoder, "latin1ExtendedUpper", latin1ExtendedUpper,
                LATIN1, true, latin1ExtendedLower, 0);
        assertWorkload(stringCoder, "latin1ExtendedLower", latin1ExtendedLower,
                LATIN1, false, latin1ExtendedUpper, 0);
        assertWorkload(stringCoder, "latin1UpperSpecialFallback",
                latin1UpperSpecialFallback, LATIN1, false,
                withLastString(latin1ExtendedUpper, "SS"), 0);
        assertFallbackChar("latin1UpperSpecialFallback",
                latin1UpperSpecialFallback, convertedLength - 1, '\u00df');

        assertWorkload(stringCoder, "utf16UpperSuffix", utf16UpperSuffix,
                UTF16, true, utf16LowerSuffix, 1);
        assertWorkload(stringCoder, "utf16LowerSuffix", utf16LowerSuffix,
                UTF16, false, utf16UpperSuffix, 1);
        assertWorkload(stringCoder, "utf16UpperSuffixFallback",
                utf16UpperSuffixFallback, UTF16, true,
                withLastChar(utf16LowerSuffix, '\u03c2'), 1);
        assertFallbackChar("utf16UpperSuffixFallback",
                utf16UpperSuffixFallback, convertedLength, '\u03a3');

        assertWorkload(stringCoder, "utf16BmpUpper", utf16BmpUpper,
                UTF16, true, utf16BmpLower, 0);
        assertWorkload(stringCoder, "utf16BmpLower", utf16BmpLower,
                UTF16, false, utf16BmpUpper, 0);
        assertWorkload(stringCoder, "utf16BmpLowerFallback",
                utf16BmpLowerFallback, UTF16, true,
                withLastChar(utf16BmpLower, '\u03c2'), 0);
        assertFallbackChar("utf16BmpLowerFallback",
                utf16BmpLowerFallback, convertedLength - 1, '\u03a3');
        assertWorkload(stringCoder, "utf16BmpUpperFallback",
                utf16BmpUpperFallback, UTF16, false,
                withLastString(utf16BmpUpper, "\u1f08\u0399"), 0);
        assertFallbackChar("utf16BmpUpperFallback",
                utf16BmpUpperFallback, convertedLength - 1, '\u1f80');
    }

    @Fork(value = 3, jvmArgsAppend = {
            UNLOCK_DIAGNOSTIC, OPEN_JAVA_LANG, COMPACT_STRINGS, MIN_LENGTH,
            USE_SVE, BACKEND_OFF,
            EXPECT_OFF, EXPECT_USE_SVE
    })
    public static class OffSVE1 extends StringCaseConversion {}

    @Fork(value = 3, jvmArgsAppend = {
            UNLOCK_DIAGNOSTIC, OPEN_JAVA_LANG, COMPACT_STRINGS, MIN_LENGTH,
            USE_SVE2, BACKEND_OFF,
            EXPECT_OFF, EXPECT_USE_SVE2
    })
    public static class OffSVE2 extends StringCaseConversion {}

    @Fork(value = 3, jvmArgsAppend = {
            UNLOCK_DIAGNOSTIC, OPEN_JAVA_LANG, COMPACT_STRINGS, MIN_LENGTH,
            USE_SVE, BACKEND_SVE,
            EXPECT_SVE, EXPECT_USE_SVE
    })
    public static class SVE extends StringCaseConversion {}

    @Fork(value = 3, jvmArgsAppend = {
            UNLOCK_DIAGNOSTIC, OPEN_JAVA_LANG, COMPACT_STRINGS, MIN_LENGTH,
            USE_SVE2, BACKEND_SVE2,
            EXPECT_SVE2, EXPECT_USE_SVE2
    })
    public static class SVE2 extends StringCaseConversion {}

    @Benchmark
    public String latin1ToLower() {
        return latin1Upper.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String latin1ToUpper() {
        return latin1Lower.toUpperCase(Locale.ROOT);
    }

    @Benchmark
    public String latin1ExtendedToLower() {
        return latin1ExtendedUpper.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String latin1ExtendedToUpper() {
        return latin1ExtendedLower.toUpperCase(Locale.ROOT);
    }

    @Benchmark
    public String latin1ExtendedToUpperFallback() {
        return latin1UpperSpecialFallback.toUpperCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16AsciiSuffixToLower() {
        return utf16UpperSuffix.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16AsciiSuffixToUpper() {
        return utf16LowerSuffix.toUpperCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16AsciiSuffixToLowerFallback() {
        return utf16UpperSuffixFallback.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16BmpToLower() {
        return utf16BmpUpper.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16BmpToUpper() {
        return utf16BmpLower.toUpperCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16BmpToLowerFallback() {
        return utf16BmpLowerFallback.toLowerCase(Locale.ROOT);
    }

    @Benchmark
    public String utf16BmpToUpperFallback() {
        return utf16BmpUpperFallback.toUpperCase(Locale.ROOT);
    }

    private static String repeatAscii(char first, char last, int length) {
        char[] chars = new char[length];
        int range = last - first + 1;
        for (int i = 0; i < length; i++) {
            chars[i] = (char)(first + (i % range));
        }
        return new String(chars);
    }

    private static String repeatLatin1Case(boolean upper, int length) {
        char[] chars = new char[length];
        int base = upper ? 0xc0 : 0xe0;
        int excluded = upper ? 0xd7 : 0xf7;
        for (int i = 0; i < length; i++) {
            int cp = base + i % 30;
            if (cp >= excluded) {
                cp++;
            }
            chars[i] = (char)cp;
        }
        return new String(chars);
    }

    private static String repeatBmpCase(boolean upper, int length) {
        char[] upperPattern = {
                '\u0100', '\u0179', '\u0391', '\u0410', '\u0531', '\u10a0',
                '\u13a0', '\u1e00', '\u2160', '\u24b6', '\u2c00', '\ua640',
                '\uff21', '\u4e00'
        };
        char[] lowerPattern = {
                '\u0101', '\u017a', '\u03b1', '\u0430', '\u0561', '\u2d00',
                '\uab70', '\u1e01', '\u2170', '\u24d0', '\u2c30', '\ua641',
                '\uff41', '\u4e00'
        };
        char[] pattern = upper ? upperPattern : lowerPattern;
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = pattern[i % pattern.length];
        }
        return new String(chars);
    }

    private static String withLastChar(String value, char last) {
        char[] chars = value.toCharArray();
        chars[chars.length - 1] = last;
        return new String(chars);
    }

    private static String withLastString(String value, String last) {
        return value.substring(0, value.length() - 1) + last;
    }

    private void assertWorkload(Method stringCoder, String name, String source,
                                byte expectedCoder, boolean lower,
                                String expected, int expectedFirst)
            throws ReflectiveOperationException {
        byte actualCoder = (Byte)stringCoder.invoke(source);
        if (actualCoder != expectedCoder) {
            throw new IllegalStateException(name + " coder=" + actualCoder
                    + ", expected " + expectedCoder);
        }

        String actual = lower
                ? source.toLowerCase(Locale.ROOT)
                : source.toUpperCase(Locale.ROOT);
        if (!actual.equals(expected)) {
            throw new IllegalStateException(name + " result mismatch: expected="
                    + printable(expected) + ", actual=" + printable(actual));
        }

        int actualFirst = firstDifference(source, expected);
        if (actualFirst != expectedFirst) {
            throw new IllegalStateException(name + " first change=" + actualFirst
                    + ", expected " + expectedFirst);
        }
        int effectiveCount = source.length() - actualFirst;
        if (effectiveCount != convertedLength) {
            throw new IllegalStateException(name + " effective count=" + effectiveCount
                    + ", expected convertedLength=" + convertedLength);
        }
    }

    private static void assertFallbackChar(String name, String source,
                                           int fallbackIndex,
                                           char fallbackChar) {
        if (source.charAt(fallbackIndex) != fallbackChar) {
            throw new IllegalStateException(name + " fallback character at "
                    + fallbackIndex + " is U+"
                    + Integer.toHexString(source.charAt(fallbackIndex))
                    + ", expected U+" + Integer.toHexString(fallbackChar));
        }
    }

    private static int firstDifference(String source, String result) {
        int limit = Math.min(source.length(), result.length());
        for (int i = 0; i < limit; i++) {
            if (source.charAt(i) != result.charAt(i)) {
                return i;
            }
        }
        return source.length() == result.length() ? -1 : limit;
    }

    private static String printable(String value) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch < 0x20 || ch > 0x7e) {
                result.append(String.format("\\u%04x", (int)ch));
            } else {
                result.append(ch);
            }
        }
        return result.toString();
    }
}
