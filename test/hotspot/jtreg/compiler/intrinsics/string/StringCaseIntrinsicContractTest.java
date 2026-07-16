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
 * @summary Exhaustively validate the Java contract used by String case intrinsics
 * @modules java.base/java.lang:open
 * @run main/othervm/timeout=1200 -XX:+UnlockDiagnosticVMOptions
 *      -XX:DisableIntrinsic=_stringLatin1ToLowerCase,_stringLatin1ToUpperCase,_stringUTF16ToLowerCase,_stringUTF16ToUpperCase
 *      compiler.intrinsics.string.StringCaseIntrinsicContractTest
 */

package compiler.intrinsics.string;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Locale;

public final class StringCaseIntrinsicContractTest {
    private static final int[] FALLBACK_INDEXES = {
            0, 7, 8, 15, 16, 31, 32, 63
    };

    private StringCaseIntrinsicContractTest() {}

    public static void main(String[] args) {
        testLatin1TopLevelExhaustive();
        testLatin1HelperContract();
        testUTF16BmpHelperContract();
    }

    private static void testLatin1TopLevelExhaustive() {
        for (int cp = 0; cp <= 0xff; cp++) {
            String input = String.valueOf((char)cp);
            String expectedLower = String.valueOf((char)Character.toLowerCase(cp));
            String expectedUpper = cp == 0xdf
                    ? "SS"
                    : String.valueOf((char)Character.toUpperCase(cp));
            assertEquals(expectedLower, input.toLowerCase(Locale.ROOT),
                    "exhaustive Latin1 lower", input, Locale.ROOT);
            assertEquals(expectedUpper, input.toUpperCase(Locale.ROOT),
                    "exhaustive Latin1 upper", input, Locale.ROOT);
        }
    }

    private static void testLatin1HelperContract() {
        try {
            Class<?> stringLatin1 = Class.forName("java.lang.StringLatin1");
            Method toLowerCaseSimple = stringLatin1.getDeclaredMethod(
                    "toLowerCaseSimple", byte[].class, byte[].class,
                    int.class, int.class);
            Method toUpperCaseSimple = stringLatin1.getDeclaredMethod(
                    "toUpperCaseSimple", byte[].class, byte[].class,
                    int.class, int.class);
            toLowerCaseSimple.setAccessible(true);
            toUpperCaseSimple.setAccessible(true);

            byte[] allLatin1 = new byte[256];
            for (int cp = 0; cp <= 0xff; cp++) {
                allLatin1[cp] = (byte)cp;
            }
            assertLatin1HelperSuccess(toLowerCaseSimple, allLatin1, true);

            byte[] upperSupported = allLatin1.clone();
            upperSupported[0xb5] = 0;
            upperSupported[0xdf] = 0;
            upperSupported[0xff] = 0;
            assertLatin1HelperSuccess(toUpperCaseSimple, upperSupported, false);

            int[] specials = {0xb5, 0xdf, 0xff};
            for (int fallbackIndex : FALLBACK_INDEXES) {
                int first = 1;
                byte[] value = new byte[first + 65];
                for (int i = first; i < value.length; i++) {
                    value[i] = (byte)latin1CaseChar(false, i - first);
                }
                int expectedFallback = first + fallbackIndex;
                value[expectedFallback] =
                        (byte)specials[fallbackIndex % specials.length];
                byte[] result = new byte[value.length];
                int fallback = (Integer)toUpperCaseSimple.invoke(
                        null, value, result, first, value.length);
                if (fallback != expectedFallback) {
                    throw new AssertionError("Latin1 upper fallback=" + fallback
                            + ", expected " + expectedFallback);
                }
                for (int i = first; i < expectedFallback; i++) {
                    int expected = Character.toUpperCase(value[i] & 0xff);
                    if ((result[i] & 0xff) != expected) {
                        throw new AssertionError(
                                "Latin1 upper prefix mismatch at " + i
                                + ": expected=" + expected
                                + " actual=" + (result[i] & 0xff));
                    }
                }
            }
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }

    private static void assertLatin1HelperSuccess(Method helper, byte[] value,
                                                   boolean lower)
            throws ReflectiveOperationException {
        byte[] result = new byte[value.length];
        int processed = (Integer)helper.invoke(
                null, value, result, 0, value.length);
        if (processed != value.length) {
            throw new AssertionError(helper.getName() + " processed=" + processed
                    + ", expected " + value.length);
        }
        for (int i = 0; i < value.length; i++) {
            int cp = value[i] & 0xff;
            int expected = lower
                    ? Character.toLowerCase(cp)
                    : Character.toUpperCase(cp);
            if ((result[i] & 0xff) != expected) {
                throw new AssertionError(helper.getName()
                        + " result mismatch at " + i
                        + ": source=" + cp + " expected=" + expected
                        + " actual=" + (result[i] & 0xff));
            }
        }
    }

    private static void testUTF16BmpHelperContract() {
        try {
            Class<?> stringUTF16 = Class.forName("java.lang.StringUTF16");
            Method toBytes = stringUTF16.getDeclaredMethod(
                    "toBytes", char[].class, int.class, int.class);
            Method newBytesFor = stringUTF16.getDeclaredMethod(
                    "newBytesFor", int.class);
            Method getChar = stringUTF16.getDeclaredMethod(
                    "getChar", byte[].class, int.class);
            Method toLowerCaseSimple = stringUTF16.getDeclaredMethod(
                    "toLowerCaseSimple", byte[].class, byte[].class,
                    int.class, int.class);
            Method toUpperCaseSimple = stringUTF16.getDeclaredMethod(
                    "toUpperCaseSimple", byte[].class, byte[].class,
                    int.class, int.class);
            Method toLowerCaseEx = stringUTF16.getDeclaredMethod(
                    "toLowerCaseEx", String.class, byte[].class, byte[].class,
                    int.class, Locale.class, boolean.class);
            Method toUpperCaseStringEx = stringUTF16.getDeclaredMethod(
                    "toUpperCaseEx", String.class, byte[].class, byte[].class,
                    int.class, Locale.class, boolean.class);
            Method toUpperCaseEx = Character.class.getDeclaredMethod(
                    "toUpperCaseEx", int.class);
            Method stringCoder = String.class.getDeclaredMethod("coder");

            toBytes.setAccessible(true);
            newBytesFor.setAccessible(true);
            getChar.setAccessible(true);
            toLowerCaseSimple.setAccessible(true);
            toUpperCaseSimple.setAccessible(true);
            toLowerCaseEx.setAccessible(true);
            toUpperCaseStringEx.setAccessible(true);
            toUpperCaseEx.setAccessible(true);
            stringCoder.setAccessible(true);

            char[] lowerInput = new char[Character.MAX_VALUE + 1];
            char[] lowerExpected = new char[lowerInput.length];
            char[] upperInput = new char[Character.MAX_VALUE + 1];
            char[] upperExpected = new char[upperInput.length];
            int lowerCount = 0;
            int upperCount = 0;

            for (int cp = Character.MIN_VALUE;
                 cp <= Character.MAX_VALUE; cp++) {
                if (!Character.isSurrogate((char)cp)
                        && cp != '\u0130' && cp != '\u03a3') {
                    int lower = Character.toLowerCase(cp);
                    if (Character.isBmpCodePoint(lower)) {
                        lowerInput[lowerCount] = (char)cp;
                        lowerExpected[lowerCount++] = (char)lower;
                    }
                }

                if (!Character.isSurrogate((char)cp)) {
                    int upper = (Integer)toUpperCaseEx.invoke(null, cp);
                    if (Character.isBmpCodePoint(upper)) {
                        upperInput[upperCount] = (char)cp;
                        upperExpected[upperCount++] = (char)upper;
                    }
                }
            }

            assertUTF16BmpSuccess(toBytes, newBytesFor, getChar,
                    toLowerCaseSimple,
                    Arrays.copyOf(lowerInput, lowerCount),
                    Arrays.copyOf(lowerExpected, lowerCount));
            assertUTF16BmpSuccess(toBytes, newBytesFor, getChar,
                    toUpperCaseSimple,
                    Arrays.copyOf(upperInput, upperCount),
                    Arrays.copyOf(upperExpected, upperCount));

            assertUTF16TopLevelSuccess(
                    Arrays.copyOf(lowerInput, lowerCount),
                    Arrays.copyOf(lowerExpected, lowerCount), true);
            assertUTF16TopLevelSuccess(
                    Arrays.copyOf(upperInput, upperCount),
                    Arrays.copyOf(upperExpected, upperCount), false);

            assertUTF16NarrowSuccess(toBytes, newBytesFor, getChar,
                    stringCoder, toLowerCaseSimple, '\u212a', 'k', true);
            assertUTF16NarrowSuccess(toBytes, newBytesFor, getChar,
                    stringCoder, toUpperCaseSimple, '\u017f', 'S', false);

            for (int cp = Character.MIN_VALUE;
                 cp <= Character.MAX_VALUE; cp++) {
                int lower = Character.toLowerCase(cp);
                if (Character.isSurrogate((char)cp)
                        || cp == '\u0130' || cp == '\u03a3'
                        || !Character.isBmpCodePoint(lower)
                        || Character.isSurrogate((char)lower)) {
                    assertUTF16BmpFallback(toBytes, newBytesFor,
                            toLowerCaseSimple, true, (char)cp, 8);
                    assertUTF16TopLevelFallback(toBytes, newBytesFor,
                            toLowerCaseEx, true, (char)cp);
                }

                int upper = (Integer)toUpperCaseEx.invoke(null, cp);
                if (Character.isSurrogate((char)cp)
                        || !Character.isBmpCodePoint(upper)
                        || Character.isSurrogate((char)upper)) {
                    assertUTF16BmpFallback(toBytes, newBytesFor,
                            toUpperCaseSimple, false, (char)cp, 8);
                    assertUTF16TopLevelFallback(toBytes, newBytesFor,
                            toUpperCaseStringEx, false, (char)cp);
                }
            }

            for (int fallbackIndex : FALLBACK_INDEXES) {
                assertUTF16BmpFallback(toBytes, newBytesFor,
                        toLowerCaseSimple, true,
                        fallbackIndex % 2 == 0 ? '\u0130' : '\u03a3',
                        fallbackIndex);
                assertUTF16BmpFallback(toBytes, newBytesFor,
                        toUpperCaseSimple, false, '\u00df', fallbackIndex);
            }
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }

    private static void assertUTF16BmpSuccess(Method toBytes,
                                              Method newBytesFor,
                                              Method getChar, Method helper,
                                              char[] input, char[] expected)
            throws ReflectiveOperationException {
        byte[] value = (byte[])toBytes.invoke(null, input, 0, input.length);
        byte[] result = (byte[])newBytesFor.invoke(null, input.length);
        int marker = (Integer)helper.invoke(
                null, value, result, 0, input.length);
        if (marker < 0) {
            throw new AssertionError(helper.getName()
                    + " unexpected fallback = " + marker);
        }
        int expectedBits = 0;
        for (int i = 0; i < expected.length; i++) {
            int actual = (Character)getChar.invoke(null, result, i);
            expectedBits |= expected[i];
            if (actual != expected[i]) {
                throw new AssertionError(helper.getName()
                        + " result mismatch at " + i
                        + ": source=" + (int)input[i]
                        + " expected=" + (int)expected[i]
                        + " actual=" + actual);
            }
        }
        boolean expectedWide = expectedBits > 0xff;
        boolean actualWide = marker > 0xff;
        if (actualWide != expectedWide) {
            throw new AssertionError(helper.getName()
                    + " coder marker = " + marker
                    + (expectedWide
                            ? " for non-Latin1 result"
                            : " for Latin1 result"));
        }
    }

    private static void assertUTF16NarrowSuccess(Method toBytes,
                                                 Method newBytesFor,
                                                 Method getChar,
                                                 Method stringCoder,
                                                 Method helper,
                                                 char sourceChar,
                                                 char expectedChar,
                                                 boolean lower)
            throws ReflectiveOperationException {
        char[] input = new char[65];
        char[] expected = new char[input.length];
        Arrays.fill(input, sourceChar);
        Arrays.fill(expected, expectedChar);
        assertUTF16BmpSuccess(
                toBytes, newBytesFor, getChar, helper, input, expected);

        String source = new String(input);
        if ((Byte)stringCoder.invoke(source) != 1) {
            throw new AssertionError("Expected UTF16 source for U+"
                    + String.format("%04x", (int)sourceChar));
        }
        String result = lower
                ? source.toLowerCase(Locale.ROOT)
                : source.toUpperCase(Locale.ROOT);
        assertEquals(new String(expected), result,
                lower ? "UTF16 narrow lower" : "UTF16 narrow upper",
                source, Locale.ROOT);
        if ((Byte)stringCoder.invoke(result) != 0) {
            throw new AssertionError("Expected Latin1 result coder for U+"
                    + String.format("%04x", (int)sourceChar));
        }
    }

    private static void assertUTF16TopLevelSuccess(char[] input,
                                                   char[] expected,
                                                   boolean lower) {
        char sourcePrefix = lower ? '\u0100' : '\u0101';
        char targetPrefix = lower ? '\u0101' : '\u0100';
        char[] prefixedInput = new char[input.length + 1];
        char[] prefixedExpected = new char[expected.length + 1];
        prefixedInput[0] = sourcePrefix;
        prefixedExpected[0] = targetPrefix;
        System.arraycopy(input, 0, prefixedInput, 1, input.length);
        System.arraycopy(expected, 0, prefixedExpected, 1, expected.length);

        String source = new String(prefixedInput);
        String actual = lower
                ? source.toLowerCase(Locale.ROOT)
                : source.toUpperCase(Locale.ROOT);
        assertEquals(new String(prefixedExpected), actual,
                lower
                        ? "top-level exhaustive UTF16 lower"
                        : "top-level exhaustive UTF16 upper",
                source, Locale.ROOT);
    }

    private static void assertUTF16TopLevelFallback(Method toBytes,
                                                    Method newBytesFor,
                                                    Method fallbackHelper,
                                                    boolean lower,
                                                    char fallbackChar)
            throws ReflectiveOperationException {
        char[] chars = new char[65];
        Arrays.fill(chars, lower ? '\u0410' : '\u0430');
        chars[8] = fallbackChar;
        String source = new String(chars);
        byte[] value = (byte[])toBytes.invoke(null, chars, 0, chars.length);
        byte[] result = (byte[])newBytesFor.invoke(null, chars.length);
        String expected = (String)fallbackHelper.invoke(
                null, source, value, result, 0, Locale.ROOT, false);
        String actual = lower
                ? source.toLowerCase(Locale.ROOT)
                : source.toUpperCase(Locale.ROOT);
        assertEquals(expected, actual,
                (lower
                        ? "top-level UTF16 lower fallback U+"
                        : "top-level UTF16 upper fallback U+")
                        + String.format("%04x", (int)fallbackChar),
                source, Locale.ROOT);
    }

    private static void assertUTF16BmpFallback(Method toBytes,
                                               Method newBytesFor,
                                               Method helper, boolean lower,
                                               char fallbackChar,
                                               int fallbackIndex)
            throws ReflectiveOperationException {
        int first = 1;
        char[] input = new char[first + 65];
        input[0] = lower ? 'x' : 'X';
        Arrays.fill(input, first, input.length,
                lower ? '\u0410' : '\u0430');
        int index = first + fallbackIndex;
        input[index] = fallbackChar;
        byte[] value = (byte[])toBytes.invoke(null, input, 0, input.length);
        byte[] result = (byte[])newBytesFor.invoke(null, input.length);
        int fallback = (Integer)helper.invoke(
                null, value, result, first, input.length);
        int expectedFallback = -(index + 1);
        if (fallback != expectedFallback) {
            throw new AssertionError(helper.getName()
                    + " fallback = " + fallback
                    + ", expected " + expectedFallback
                    + " for U+"
                    + String.format("%04x", (int)fallbackChar));
        }
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

    private static void assertEquals(String expected, String actual,
                                     String mode, String input,
                                     Locale locale) {
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
