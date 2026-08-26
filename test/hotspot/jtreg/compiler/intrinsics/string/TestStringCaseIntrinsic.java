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
 * @summary String case conversion intrinsic correctness, BMP mappings, fallback, and vector boundaries
 * @requires os.arch=="aarch64"
 * @requires vm.compiler2.enabled
 * @requires vm.flagless
 * @library /test/lib
 * @modules java.base/java.lang:open
 *          java.base/jdk.internal.misc
 *          jdk.management
 * @build jdk.test.whitebox.WhiteBox
 *        compiler.intrinsics.string.StringCaseIntrinsicLogVerifier
 *        compiler.intrinsics.string.StringCaseIntrinsicDriver
 *        compiler.intrinsics.string.TestStringCaseIntrinsic
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run main/othervm/timeout=600 -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions
 *      -XX:+WhiteBoxAPI compiler.intrinsics.string.StringCaseIntrinsicDriver
 */

package compiler.intrinsics.string;

import com.sun.management.HotSpotDiagnosticMXBean;
import jdk.test.whitebox.WhiteBox;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public class TestStringCaseIntrinsic {
    private static final int COMP_LEVEL_FULL_OPTIMIZATION = 4;
    private static final String[] WRAPPER_NAMES = {
            "latin1LowerWrapper",
            "latin1UpperWrapper",
            "utf16LowerWrapper",
            "utf16UpperWrapper"
    };
    private static final WhiteBox WHITE_BOX = WhiteBox.getWhiteBox();
    private static final Locale TR = Locale.forLanguageTag("tr");
    private static final Locale AZ = Locale.forLanguageTag("az");
    private static final Locale LT = Locale.forLanguageTag("lt");

    private record Case(String input, Locale locale, String expected) {}

    private static final Case[] LOWER_CASES = {
            new Case("ABCXYZ", Locale.ROOT, "abcxyz"),
            new Case("hello ABC xyz", Locale.ENGLISH, "hello abc xyz"),
            new Case("\u4e00ABCXYZ", Locale.ROOT, "\u4e00abcxyz"),
            new Case("ABC\u4e00XYZ", Locale.ROOT, "abc\u4e00xyz"),
            new Case("\u00c0ABC", Locale.ROOT, "\u00e0abc"),
            new Case("\u0410\u03a3", Locale.ROOT, "\u0430\u03c2"),
            new Case("\u0410\u0130", Locale.ROOT, "\u0430i\u0307"),
            new Case("\u0130ABC", Locale.ROOT, "i\u0307abc"),
            new Case("\u03a3ABC", Locale.ROOT, "\u03c3abc"),
            new Case("\uD801\uDC00ABC", Locale.ROOT, "\uD801\uDC28abc"),
            new Case("A\uD83D\uDE00Z", Locale.ROOT, "a\uD83D\uDE00z"),
            new Case("TITLE", TR, "t\u0131tle"),
            new Case("\u0130ABC", TR, "iabc"),
            new Case("TITLE", AZ, "t\u0131tle"),
            new Case("ABC", LT, "abc"),
            new Case("\u00ccAAAAAAA", LT, "i\u0307\u0300aaaaaaa"),
            new Case("I\u0301AAAAAA", LT, "i\u0307\u0301aaaaaa")
    };

    private static final Case[] UPPER_CASES = {
            new Case("abcxyz", Locale.ROOT, "ABCXYZ"),
            new Case("HELLO abc xyz", Locale.ENGLISH, "HELLO ABC XYZ"),
            new Case("\u4e00abcxyz", Locale.ROOT, "\u4e00ABCXYZ"),
            new Case("abc\u4e00xyz", Locale.ROOT, "ABC\u4e00XYZ"),
            new Case("\u0430\u00df", Locale.ROOT, "\u0410SS"),
            new Case("\u00dfabc", Locale.ROOT, "SSABC"),
            new Case("\u00b5abc", Locale.ROOT, "\u039cABC"),
            new Case("\uD801\uDC28abc", Locale.ROOT, "\uD801\uDC00ABC"),
            new Case("a\uD83D\uDE00z", Locale.ROOT, "A\uD83D\uDE00Z"),
            new Case("title", TR, "T\u0130TLE"),
            new Case("title", AZ, "T\u0130TLE"),
            new Case("abc", LT, "ABC"),
            new Case("i\u0307aaaaaa", LT, "IAAAAAA")
    };

    private static final int[] VECTOR_LENGTHS = {
            7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65
    };
    private static final int[] PREFIX_LENGTHS = {0, 1, 7, 15};
    private static final int[] FALLBACK_INDEXES = {0, 7, 8, 15, 16, 31, 32, 63};
    private static final char[] LOWER_CLASSIFIER_PATTERN = {
            '\u003f', '\u0040', '\u0041', '\u005a', '\u005b', '\\',
            '\u005d', '\u005e', '\u005f', '\u0060', '\u00bf', '\u00c0',
            '\u00d6', '\u00d7', '\u00d8', '\u00de', '\u00df', '\u00e0'
    };
    private static final char[] UPPER_CLASSIFIER_PATTERN = {
            '\u005f', '\u0060', '\u0061', '\u007a', '\u007b', '\u007c',
            '\u007d', '\u007e', '\u007f', '\u0080', '\u00b5', '\u00df',
            '\u00e0', '\u00f6', '\u00f7', '\u00f8', '\u00fe', '\u00ff'
    };
    private static final char[] UTF16_BMP_LOWER_SOURCE_PATTERN = {
            '\u0100', '\u0179', '\u0391', '\u0410', '\u0531', '\u10a0',
            '\u13a0', '\u1e00', '\u2160', '\u24b6', '\u2c00', '\ua640',
            '\uff21', '\u4e00'
    };
    private static final char[] UTF16_BMP_UPPER_SOURCE_PATTERN = {
            '\u0101', '\u017a', '\u03b1', '\u0430', '\u0561', '\u2d00',
            '\uab70', '\u1e01', '\u2170', '\u24d0', '\u2c30', '\ua641',
            '\uff41', '\u4e00'
    };
    private static final Case[] VECTOR_LOWER_CASES = createVectorCases(true);
    private static final Case[] VECTOR_UPPER_CASES = createVectorCases(false);
    private static final Case[] LATIN1_LOWER_CASES = createLatin1Cases(true);
    private static final Case[] LATIN1_UPPER_CASES = createLatin1Cases(false);
    private static final Case[] LATIN1_UPPER_FALLBACK_CASES = createLatin1UpperFallbackCases();
    private static final Case[] UTF16_BMP_LOWER_CASES = createUTF16BmpCases(true);
    private static final Case[] UTF16_BMP_UPPER_CASES = createUTF16BmpCases(false);
    private static final String WARM_LATIN1_UPPER =
            asciiPattern('A', 'Z', 32) + latin1Pattern(true, 33);
    private static final String WARM_LATIN1_LOWER =
            asciiPattern('a', 'z', 32) + latin1Pattern(false, 33);
    private static final String WARM_UTF16_BMP_UPPER = "\u0100"
            + asciiPattern('A', 'Z', 32) + utf16BmpPattern(true, 32);
    private static final String WARM_UTF16_BMP_LOWER = "\u0101"
            + asciiPattern('a', 'z', 32) + utf16BmpPattern(false, 32);
    private static final byte[] WRAPPER_LATIN1_UPPER =
            WARM_LATIN1_UPPER.getBytes(StandardCharsets.ISO_8859_1);
    private static final byte[] WRAPPER_LATIN1_LOWER =
            WARM_LATIN1_LOWER.getBytes(StandardCharsets.ISO_8859_1);
    private static final byte[] WRAPPER_UTF16_UPPER = utf16Bytes(WARM_UTF16_BMP_UPPER);
    private static final byte[] WRAPPER_UTF16_LOWER = utf16Bytes(WARM_UTF16_BMP_LOWER);
    private static final byte[] WRAPPER_LATIN1_LOWER_RESULT = new byte[65];
    private static final byte[] WRAPPER_LATIN1_UPPER_RESULT = new byte[65];
    private static final byte[] WRAPPER_UTF16_LOWER_RESULT = new byte[130];
    private static final byte[] WRAPPER_UTF16_UPPER_RESULT = new byte[130];
    private static final MethodHandle LATIN1_LOWER_HELPER;
    private static final MethodHandle LATIN1_UPPER_HELPER;
    private static final MethodHandle UTF16_LOWER_HELPER;
    private static final MethodHandle UTF16_UPPER_HELPER;
    private static volatile int sink;

    static {
        try {
            MethodType type = MethodType.methodType(int.class,
                    byte[].class, byte[].class, int.class, int.class);
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            Class<?> latin1 = Class.forName("java.lang.StringLatin1");
            Class<?> utf16 = Class.forName("java.lang.StringUTF16");
            MethodHandles.Lookup latin1Lookup = MethodHandles.privateLookupIn(latin1, lookup);
            MethodHandles.Lookup utf16Lookup = MethodHandles.privateLookupIn(utf16, lookup);
            LATIN1_LOWER_HELPER = latin1Lookup.findStatic(
                    latin1, "toLowerCaseSimple", type);
            LATIN1_UPPER_HELPER = latin1Lookup.findStatic(
                    latin1, "toUpperCaseSimple", type);
            UTF16_LOWER_HELPER = utf16Lookup.findStatic(
                    utf16, "toLowerCaseSimple", type);
            UTF16_UPPER_HELPER = utf16Lookup.findStatic(
                    utf16, "toUpperCaseSimple", type);
        } catch (ReflectiveOperationException exception) {
            throw new ExceptionInInitializerError(exception);
        }
    }

    public static void main(String[] args) throws Throwable {
        checkVMConfiguration();
        assertStringCaseCapability();
        assertIntrinsicAvailability();
        for (int warm = 0; warm < 20_000; warm++) {
            warmup();
        }
        assertWrappersCompiledByC2();
        exerciseCompiledWrappers();
        exerciseOnce();
        testLatin1ClassifierBlocks();
    }

    private static void assertStringCaseCapability()
            throws ReflectiveOperationException {
        boolean expected = Integer.getInteger("expectedBackend", 0) != 0;
        Field capability = String.class.getDeclaredField(
                "STRING_CASE_INTRINSICS");
        capability.setAccessible(true);
        boolean actual = capability.getBoolean(null);
        if (actual != expected) {
            throw new AssertionError("STRING_CASE_INTRINSICS=" + actual
                    + ", expected " + expected);
        }
    }

    private static void assertIntrinsicAvailability() throws ReflectiveOperationException {
        boolean expected = Integer.getInteger("expectedBackend", 0) != 0;
        Class<?> stringLatin1 = Class.forName("java.lang.StringLatin1");
        Class<?> stringUTF16 = Class.forName("java.lang.StringUTF16");
        Class<?>[] parameters = {byte[].class, byte[].class, int.class, int.class};
        Method[] helpers = {
                stringLatin1.getDeclaredMethod("toLowerCaseSimple", parameters),
                stringLatin1.getDeclaredMethod("toUpperCaseSimple", parameters),
                stringUTF16.getDeclaredMethod("toLowerCaseSimple", parameters),
                stringUTF16.getDeclaredMethod("toUpperCaseSimple", parameters)
        };
        for (Method helper : helpers) {
            boolean available = WHITE_BOX.isIntrinsicAvailable(
                    helper, COMP_LEVEL_FULL_OPTIMIZATION);
            if (available != expected) {
                throw new AssertionError(helper + " intrinsic availability=" + available
                        + ", expected " + expected);
            }
        }
    }

    private static void assertWrappersCompiledByC2() throws ReflectiveOperationException {
        for (String wrapperName : WRAPPER_NAMES) {
            Method wrapper = TestStringCaseIntrinsic.class.getDeclaredMethod(wrapperName);
            if (WHITE_BOX.getMethodCompilationLevel(wrapper) != COMP_LEVEL_FULL_OPTIMIZATION) {
                WHITE_BOX.enqueueMethodForCompilation(wrapper, COMP_LEVEL_FULL_OPTIMIZATION);
            }
            int level = WHITE_BOX.getMethodCompilationLevel(wrapper);
            if (level != COMP_LEVEL_FULL_OPTIMIZATION) {
                throw new AssertionError(wrapperName + " compilation level=" + level
                        + ", expected " + COMP_LEVEL_FULL_OPTIMIZATION);
            }
        }
    }

    private static void warmup() throws Throwable {
        sink += latin1LowerWrapper();
        sink += latin1UpperWrapper();
        sink += utf16LowerWrapper();
        sink += utf16UpperWrapper();
    }

    private static int latin1LowerWrapper() throws Throwable {
        return (int)LATIN1_LOWER_HELPER.invokeExact(
                WRAPPER_LATIN1_UPPER, WRAPPER_LATIN1_LOWER_RESULT, 0, 65);
    }

    private static int latin1UpperWrapper() throws Throwable {
        return (int)LATIN1_UPPER_HELPER.invokeExact(
                WRAPPER_LATIN1_LOWER, WRAPPER_LATIN1_UPPER_RESULT, 0, 65);
    }

    private static int utf16LowerWrapper() throws Throwable {
        return (int)UTF16_LOWER_HELPER.invokeExact(
                WRAPPER_UTF16_UPPER, WRAPPER_UTF16_LOWER_RESULT, 0, 65);
    }

    private static int utf16UpperWrapper() throws Throwable {
        return (int)UTF16_UPPER_HELPER.invokeExact(
                WRAPPER_UTF16_LOWER, WRAPPER_UTF16_UPPER_RESULT, 0, 65);
    }

    private static void exerciseCompiledWrappers() throws Throwable {
        assertWrapperResult("compiled Latin1 lower wrapper", true,
                latin1LowerWrapper(), WRAPPER_LATIN1_LOWER,
                WRAPPER_LATIN1_LOWER_RESULT);
        assertWrapperResult("compiled Latin1 upper wrapper", true,
                latin1UpperWrapper(), WRAPPER_LATIN1_UPPER,
                WRAPPER_LATIN1_UPPER_RESULT);
        assertWrapperResult("compiled UTF16 lower wrapper", false,
                utf16LowerWrapper(), WRAPPER_UTF16_LOWER,
                WRAPPER_UTF16_LOWER_RESULT);
        assertWrapperResult("compiled UTF16 upper wrapper", false,
                utf16UpperWrapper(), WRAPPER_UTF16_UPPER,
                WRAPPER_UTF16_UPPER_RESULT);
    }

    private static void exerciseOnce() {
        for (Case c : LOWER_CASES) {
            assertEquals(c.expected(), c.input().toLowerCase(c.locale()),
                    "lower", c.input(), c.locale());
        }
        for (Case c : UPPER_CASES) {
            assertEquals(c.expected(), c.input().toUpperCase(c.locale()),
                    "upper", c.input(), c.locale());
        }
        for (Case c : VECTOR_LOWER_CASES) {
            assertEquals(c.expected(), c.input().toLowerCase(c.locale()),
                    "vector lower", c.input(), c.locale());
        }
        for (Case c : VECTOR_UPPER_CASES) {
            assertEquals(c.expected(), c.input().toUpperCase(c.locale()),
                    "vector upper", c.input(), c.locale());
        }
        for (Case c : LATIN1_LOWER_CASES) {
            assertEquals(c.expected(), c.input().toLowerCase(c.locale()),
                    "Latin1 lower", c.input(), c.locale());
        }
        for (Case c : LATIN1_UPPER_CASES) {
            assertEquals(c.expected(), c.input().toUpperCase(c.locale()),
                    "Latin1 upper", c.input(), c.locale());
        }
        for (Case c : LATIN1_UPPER_FALLBACK_CASES) {
            assertEquals(c.expected(), c.input().toUpperCase(c.locale()),
                    "Latin1 upper fallback", c.input(), c.locale());
        }
        for (Case c : UTF16_BMP_LOWER_CASES) {
            assertEquals(c.expected(), c.input().toLowerCase(c.locale()),
                    "UTF16 BMP lower", c.input(), c.locale());
        }
        for (Case c : UTF16_BMP_UPPER_CASES) {
            assertEquals(c.expected(), c.input().toUpperCase(c.locale()),
                    "UTF16 BMP upper", c.input(), c.locale());
        }

        String unchanged = "already lower";
        if (unchanged.toLowerCase(Locale.ROOT) != unchanged) {
            throw new AssertionError("lower unchanged string did not return original instance");
        }

        String unchangedUpper = "ALREADY UPPER";
        if (unchangedUpper.toUpperCase(Locale.ROOT) != unchangedUpper) {
            throw new AssertionError("upper unchanged string did not return original instance");
        }
    }

    private static void checkVMConfiguration() {
        int expectedBackend = Integer.getInteger("expectedBackend", -1);
        int expectedMinLength = Integer.getInteger("expectedMinLength", -1);
        int expectedUseSVE = Integer.getInteger("expectedUseSVE", -1);
        HotSpotDiagnosticMXBean bean = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean.class);
        int actualBackend = Integer.parseInt(bean.getVMOption("StringCaseIntrinsicBackend").getValue());
        int actualMinLength = Integer.parseInt(bean.getVMOption("StringCaseIntrinsicMinLength").getValue());
        int useSVE = Integer.parseInt(bean.getVMOption("UseSVE").getValue());

        if (useSVE != expectedUseSVE) {
            throw new AssertionError("UseSVE=" + useSVE
                    + ", expected " + expectedUseSVE);
        }

        if (actualMinLength != expectedMinLength) {
            throw new AssertionError("StringCaseIntrinsicMinLength=" + actualMinLength
                    + ", expected " + expectedMinLength);
        }

        if (actualBackend != expectedBackend) {
            throw new AssertionError("StringCaseIntrinsicBackend=" + actualBackend
                    + ", expected " + expectedBackend + ", UseSVE=" + useSVE);
        }
    }

    private static Case[] createVectorCases(boolean lower) {
        List<Case> cases = new ArrayList<>();
        for (int length : VECTOR_LENGTHS) {
            for (int prefixLength : PREFIX_LENGTHS) {
                cases.add(createVectorCase(lower, false, prefixLength, length, -1));
                cases.add(createVectorCase(lower, true, prefixLength, length, -1));
            }
        }
        for (int fallbackIndex : FALLBACK_INDEXES) {
            cases.add(createVectorCase(lower, false, 1, 65, fallbackIndex));
            cases.add(createVectorCase(lower, true, 1, 65, fallbackIndex));
        }
        return cases.toArray(Case[]::new);
    }

    private static Case createVectorCase(boolean lower, boolean utf16,
                                         int prefixLength, int segmentLength,
                                         int fallbackIndex) {
        int leading = utf16 ? 1 : 0;
        int segmentOffset = leading + prefixLength;
        char[] input = new char[segmentOffset + segmentLength];
        char[] expected = new char[input.length];

        if (utf16) {
            input[0] = '\u4e00';
            expected[0] = '\u4e00';
        }

        char unchanged = lower ? 'x' : 'X';
        for (int i = leading; i < segmentOffset; i++) {
            input[i] = unchanged;
            expected[i] = unchanged;
        }

        char sourceFirst = lower ? 'A' : 'a';
        char sourceLast = lower ? 'Z' : 'z';
        int range = sourceLast - sourceFirst + 1;
        for (int i = 0; i < segmentLength; i++) {
            char source = (char)(sourceFirst + i % range);
            input[segmentOffset + i] = source;
            expected[segmentOffset + i] = (char)(lower ? source + 0x20 : source - 0x20);
        }

        if (fallbackIndex >= 0) {
            int index = segmentOffset + fallbackIndex;
            if (utf16) {
                input[index] = lower ? '\u0100' : '\u0101';
                expected[index] = lower ? '\u0101' : '\u0100';
            } else {
                input[index] = lower ? '\u00c0' : '\u00e0';
                expected[index] = lower ? '\u00e0' : '\u00c0';
            }
        }
        return new Case(new String(input), Locale.ROOT, new String(expected));
    }

    private static Case[] createLatin1Cases(boolean lower) {
        List<Case> cases = new ArrayList<>();
        for (int length : VECTOR_LENGTHS) {
            for (int prefixLength : PREFIX_LENGTHS) {
                char[] input = new char[prefixLength + length];
                char[] expected = new char[input.length];
                char unchanged = lower ? 'x' : 'X';
                for (int i = 0; i < prefixLength; i++) {
                    input[i] = unchanged;
                    expected[i] = unchanged;
                }
                for (int i = 0; i < length; i++) {
                    char source = latin1CaseChar(lower, i);
                    input[prefixLength + i] = source;
                    expected[prefixLength + i] = (char)(lower ? source + 0x20 : source - 0x20);
                }
                cases.add(new Case(new String(input), Locale.ROOT, new String(expected)));
            }
        }
        return cases.toArray(Case[]::new);
    }

    private static Case[] createLatin1UpperFallbackCases() {
        List<Case> cases = new ArrayList<>();
        char[] specials = {'\u00b5', '\u00df', '\u00ff'};
        for (int fallbackIndex : FALLBACK_INDEXES) {
            int prefixLength = 1;
            char[] input = new char[prefixLength + 65];
            input[0] = 'X';
            for (int i = 0; i < 65; i++) {
                input[prefixLength + i] = latin1CaseChar(false, i);
            }
            input[prefixLength + fallbackIndex] = specials[fallbackIndex % specials.length];

            StringBuilder expected = new StringBuilder(input.length + 1);
            expected.append('X');
            for (int i = 0; i < 65; i++) {
                char source = input[prefixLength + i];
                if (source == '\u00b5') {
                    expected.append('\u039c');
                } else if (source == '\u00df') {
                    expected.append("SS");
                } else if (source == '\u00ff') {
                    expected.append('\u0178');
                } else {
                    expected.append((char)(source - 0x20));
                }
            }
            cases.add(new Case(new String(input), Locale.ROOT, expected.toString()));
        }
        return cases.toArray(Case[]::new);
    }

    private static String asciiPattern(char first, char last, int length) {
        char[] chars = new char[length];
        int range = last - first + 1;
        for (int i = 0; i < length; i++) {
            chars[i] = (char)(first + i % range);
        }
        return new String(chars);
    }

    private static String latin1Pattern(boolean upper, int length) {
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = latin1CaseChar(upper, i);
        }
        return new String(chars);
    }

    private static String utf16BmpPattern(boolean upper, int length) {
        char[] pattern = upper
                ? UTF16_BMP_LOWER_SOURCE_PATTERN
                : UTF16_BMP_UPPER_SOURCE_PATTERN;
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = pattern[i % pattern.length];
        }
        return new String(chars);
    }

    private static Case[] createUTF16BmpCases(boolean lower) {
        List<Case> cases = new ArrayList<>();
        char[] pattern = lower
                ? UTF16_BMP_LOWER_SOURCE_PATTERN
                : UTF16_BMP_UPPER_SOURCE_PATTERN;
        for (int length : VECTOR_LENGTHS) {
            for (int prefixLength : PREFIX_LENGTHS) {
                char[] input = new char[prefixLength + length];
                char[] expected = new char[input.length];
                char unchanged = lower ? 'x' : 'X';
                for (int i = 0; i < prefixLength; i++) {
                    input[i] = unchanged;
                    expected[i] = unchanged;
                }
                for (int i = 0; i < length; i++) {
                    char source = pattern[i % pattern.length];
                    input[prefixLength + i] = source;
                    expected[prefixLength + i] = (char)(lower
                            ? Character.toLowerCase(source)
                            : Character.toUpperCase(source));
                }
                cases.add(new Case(new String(input), Locale.ROOT, new String(expected)));
            }
        }
        return cases.toArray(Case[]::new);
    }

    private static void testLatin1ClassifierBlocks() {
        for (int length : VECTOR_LENGTHS) {
            String lowerInput = repeatPattern(LOWER_CLASSIFIER_PATTERN, length);
            assertEquals(expectedLatin1Lower(lowerInput), lowerInput.toLowerCase(Locale.ROOT),
                    "Latin1 classifier lower", lowerInput, Locale.ROOT);

            String upperInput = repeatPattern(UPPER_CLASSIFIER_PATTERN, length);
            assertEquals(expectedLatin1Upper(upperInput), upperInput.toUpperCase(Locale.ROOT),
                    "Latin1 classifier upper", upperInput, Locale.ROOT);
        }
    }

    private static String repeatPattern(char[] pattern, int length) {
        char[] value = new char[length];
        for (int i = 0; i < length; i++) {
            value[i] = pattern[i % pattern.length];
        }
        return new String(value);
    }

    private static String expectedLatin1Lower(String input) {
        char[] result = input.toCharArray();
        for (int i = 0; i < result.length; i++) {
            result[i] = Character.toLowerCase(result[i]);
        }
        return new String(result);
    }

    private static String expectedLatin1Upper(String input) {
        StringBuilder result = new StringBuilder(input.length() + 4);
        for (int i = 0; i < input.length(); i++) {
            int cp = input.charAt(i);
            if (cp == 0xdf) {
                result.append("SS");
            } else {
                result.appendCodePoint(Character.toUpperCase(cp));
            }
        }
        return result.toString();
    }

    private static char latin1CaseChar(boolean upper, int index) {
        int base = upper ? 0xc0 : 0xe0;
        int excluded = upper ? 0xd7 : 0xf7;
        int cp = base + index % 30;
        if (cp >= excluded) {
            cp++;
        }
        return (char)cp;
    }

    private static byte[] utf16Bytes(String value) {
        ByteBuffer buffer = ByteBuffer.allocate(value.length() * 2)
                .order(ByteOrder.nativeOrder());
        for (int i = 0; i < value.length(); i++) {
            buffer.putChar(value.charAt(i));
        }
        return buffer.array();
    }

    private static void assertWrapperResult(String name, boolean latin1,
                                            int marker, byte[] expected,
                                            byte[] actual) {
        if ((latin1 && marker != expected.length) || (!latin1 && marker < 0)) {
            throw new AssertionError(name + " marker=" + marker);
        }
        if (!Arrays.equals(expected, actual)) {
            throw new AssertionError(name + " result bytes mismatch");
        }
    }

    private static void assertEquals(String expected, String actual,
                                     String mode, String input, Locale locale) {
        if (!expected.equals(actual)) {
            throw new AssertionError(mode + " mismatch for locale=" + locale
                    + " input=" + printable(input)
                    + " expected=" + printable(expected)
                    + " actual=" + printable(actual));
        }
    }

    private static String printable(String value) {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c < 0x20 || c > 0x7e) {
                out.append(String.format("\\u%04x", (int)c));
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }
}
