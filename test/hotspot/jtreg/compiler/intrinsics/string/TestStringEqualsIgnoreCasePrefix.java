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
 * @summary Verify case-insensitive prefix checks for Latin1, UTF16 and mixed encodings.
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.flagless
 * @requires vm.cpu.features ~= ".*sve.*"
 * @modules java.base/java.lang:open
 * @run main/othervm -XX:+UnlockDiagnosticVMOptions -XX:+UseStringEqualsIgnoreCaseIntrinsic -XX:StringEqualsIgnoreCaseIntrinsicMinLength=0 compiler.intrinsics.string.TestStringEqualsIgnoreCasePrefix
 * @run main/othervm -XX:+UnlockDiagnosticVMOptions -XX:+UseStringEqualsIgnoreCaseIntrinsic -XX:StringEqualsIgnoreCaseIntrinsicMinLength=16 compiler.intrinsics.string.TestStringEqualsIgnoreCasePrefix
 * @run main/othervm -XX:+UnlockDiagnosticVMOptions -XX:-UseStringEqualsIgnoreCaseIntrinsic compiler.intrinsics.string.TestStringEqualsIgnoreCasePrefix
 */

package compiler.intrinsics.string;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TestStringEqualsIgnoreCasePrefix {
    private static final int LATIN1_VALUES = 256;
    private static final Field CODER;

    static {
        try {
            CODER = String.class.getDeclaredField("coder");
            CODER.setAccessible(true);
        } catch (ReflectiveOperationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static void main(String[] args) {
        boolean[][] expected = new boolean[LATIN1_VALUES][LATIN1_VALUES];
        for (int a = 0; a < LATIN1_VALUES; a++) {
            for (int b = 0; b < LATIN1_VALUES; b++) {
                expected[a][b] = equivalent((char) a, (char) b);
            }
        }

        int comparisons = 0;
        for (int length : new int[] {1, 3, 7, 8, 16, 17}) {
            for (int position = 0; position < Math.min(length, Long.BYTES); position++) {
                String[] left = strings(length, position, 'a');
                String[] right = strings(length, position, 'A');
                String[] offsetLeft = new String[LATIN1_VALUES];
                String[] offsetRight = new String[LATIN1_VALUES];
                for (int value = 0; value < LATIN1_VALUES; value++) {
                    offsetLeft[value] = ">" + left[value] + "!";
                    offsetRight[value] = "##" + right[value] + "?";
                }
                for (int a = 0; a < LATIN1_VALUES; a++) {
                    for (int b = 0; b < LATIN1_VALUES; b++) {
                        if (left[a].equalsIgnoreCase(right[b]) != expected[a][b]
                                || offsetLeft[a].regionMatches(true, 1, offsetRight[b], 2, length)
                                   != expected[a][b]) {
                            throw new AssertionError("length=" + length + ", position=" + position
                                    + ", left=" + a + ", right=" + b);
                        }
                        comparisons += 2;
                    }
                }
            }
        }
        checkShortRanges();
        System.out.println("Verified Latin1 comparisons: " + comparisons);
        checkMixedEncodings();
    }

    private static String[] strings(int length, int position, char filler) {
        String[] result = new String[LATIN1_VALUES];
        char[] chars = new char[length];
        Arrays.fill(chars, filler);
        for (int value = 0; value < LATIN1_VALUES; value++) {
            chars[position] = (char) value;
            result[value] = new String(chars);
        }
        return result;
    }

    private static boolean equivalent(int a, int b) {
        int upperA = Character.toUpperCase(a);
        int upperB = Character.toUpperCase(b);
        return a == b || upperA == upperB
                || Character.toLowerCase(upperA) == Character.toLowerCase(upperB);
    }

    private static void checkMixedEncodings() {
        List<Comparison> cases = new ArrayList<>();
        for (int length : new int[] {7, 8, 15, 16, 17}) {
            for (int position = -1; position < Math.min(length, Long.BYTES); position++) {
                char[] left = ascii(length, false);
                char[] right = ascii(length, true);
                if (position >= 0) {
                    right[position] = 'Z';
                }
                String label = "ASCII length=" + length + ", position=" + position;
                addRegions(cases, label, new String(left), new String(right));

                // Keep the requested length while forcing the whole-string coders.
                int anchor = position == length - 1 ? 0 : length - 1;
                left[anchor] = 'k';
                right[anchor] = '\u212a';
                addWhole(cases, label, "LU", new String(left), new String(right));
                addWhole(cases, label, "UL", new String(right), new String(left));
                left[anchor] = right[anchor] = '\u0100';
                addWhole(cases, label, "UU", new String(left), new String(right));
            }

            String[][] pairs = {
                    {"k", "\u212a"}, {"s", "\u017f"}, {"i", "\u0131"},
                    {"\u00b5", "\u03bc"}, {"\u00ff", "\u0178"},
                    {"\u00e0", "\u00c0"}, {"k", "\u212b"},
                    {"\u00d7", "\u00f7"},
                    {"\ud801\udc00", "\ud801\udc28"},
                    {"\ud801\udc00", "\ud801\udc01"},
                    {"\ud801x", "\ud801X"}, {"\udc00", "\udc01"},
                    {"\ud801\ud801\udc00", "\ud801\udc00\ud801"}
            };
            for (int position = 0; position < Math.min(length, Long.BYTES); position++) {
                for (int pair = 0; pair < pairs.length; pair++) {
                    String a = pairs[pair][0];
                    String b = pairs[pair][1];
                    if (position + a.length() > length || position + b.length() > length) {
                        continue;
                    }
                    char[] left = ascii(length, false);
                    char[] right = ascii(length, true);
                    a.getChars(0, a.length(), left, position);
                    b.getChars(0, b.length(), right, position);
                    String label = "Unicode length=" + length + ", position=" + position + ", pair=" + pair;
                    addAvailableEncodings(cases, label, new String(left), new String(right));
                    if (position + a.length() < length) {
                        right[length - 1] = '!';
                        addAvailableEncodings(cases, label + ", later mismatch", new String(left), new String(right));
                    }
                }
            }
        }

        // The sentinel after the range must not be read as part of a short prefix.
        for (int length = 0; length < Long.BYTES; length++) {
            addRegions(cases, "short range=" + length, "a".repeat(length), "A".repeat(length));
        }
        addRegion(cases, "negative range", "UU", "\u0100a", 2, "\u0100b", 2, -1);
        // A neighboring surrogate outside the range must not complete a code point.
        addRegion(cases, "surrogate before range", "UU", "\ud801\udc00a", 1, "\udc00A", 0, 2);
        addRegion(cases, "surrogate after range", "UU", "\ud801\udc00", 0, "\ud801x", 0, 1);

        for (int round = 0; round < 256; round++) {
            for (Comparison test : cases) {
                boolean actual = test.whole()
                        ? test.left().equalsIgnoreCase(test.right())
                        : test.left().regionMatches(true, test.leftOffset(), test.right(), test.rightOffset(), test.length());
                if (actual != test.expected()) {
                    throw new AssertionError(test.label() + ": expected=" + test.expected() + ", actual=" + actual);
                }
            }
        }
        System.out.println("Verified mixed-encoding comparisons: " + 256L * cases.size());
    }

    private static char[] ascii(int length, boolean upper) {
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = (char) ((upper ? 'A' : 'a') + i % 20);
        }
        return chars;
    }

    private static void addRegions(List<Comparison> cases, String label, String left, String right) {
        addRegion(cases, label, "LL", ">" + left + "!", 1, "##" + right + "?", 2, left.length());
        addRegion(cases, label, "LU", ">" + left + "!", 1, "\u0100#" + right + "?", 2, left.length());
        addRegion(cases, label, "UL", "\u0100" + left + "!", 1, "##" + right + "?", 2, left.length());
        addRegion(cases, label, "UU", "\u0100" + left + "!", 1, "\u0100#" + right + "?", 2, left.length());
    }

    private static void addAvailableEncodings(List<Comparison> cases, String label, String left, String right) {
        addWhole(cases, label, coderPair(left, right), left, right);
        addWhole(cases, label + " reversed", coderPair(right, left), right, left);
        addRegion(cases, label, coderPair(left, "\u0100" + right), left + "!", 0,
                "\u0100#" + right + "?", 2, left.length());
        addRegion(cases, label + " reversed", coderPair("\u0100" + right, left),
                "\u0100" + right + "!", 1, "##" + left + "?", 2, left.length());
        addRegion(cases, label, "UU", "\u0100" + left + "!", 1, "\u0100#" + right + "?", 2, left.length());
    }

    private static void addWhole(List<Comparison> cases, String label, String coders, String left, String right) {
        add(cases, label, coders, left, 0, right, 0, left.length(), true);
    }

    private static void addRegion(List<Comparison> cases, String label, String coders,
                                  String left, int leftOffset, String right, int rightOffset, int length) {
        add(cases, label, coders, left, leftOffset, right, rightOffset, length, false);
    }

    private static void add(List<Comparison> cases, String label, String coders,
                            String left, int leftOffset, String right, int rightOffset, int length, boolean whole) {
        if (!coderPair(left, right).equals(coders)) {
            throw new AssertionError(label + ": expected coder pair " + coders);
        }
        boolean expected = (!whole || left.length() == right.length())
                && reference(left, leftOffset, right, rightOffset, length);
        cases.add(new Comparison(coders + " " + label, left, leftOffset, right, rightOffset, length, whole, expected));
    }

    private static String coderPair(String left, String right) {
        try {
            return (CODER.getByte(left) == 0 ? "L" : "U") + (CODER.getByte(right) == 0 ? "L" : "U");
        } catch (IllegalAccessException e) {
            throw new AssertionError(e);
        }
    }

    private static boolean reference(String left, int leftOffset, String right, int rightOffset, int length) {
        int leftEnd = leftOffset + length;
        int rightEnd = rightOffset + length;
        for (int i = leftOffset, j = rightOffset; i < leftEnd && j < rightEnd; i++, j++) {
            int a = left.charAt(i);
            int b = right.charAt(j);
            if (equivalent(a, b)) {
                continue;
            }
            int ca = includingCodePoint(left, i, leftOffset, leftEnd);
            int cb = includingCodePoint(right, j, rightOffset, rightEnd);
            if (ca < 0) {
                ca = -ca;
                i++;
            }
            if (cb < 0) {
                cb = -cb;
                j++;
            }
            if (!equivalent(ca, cb)) {
                return false;
            }
        }
        return true;
    }

    private static int includingCodePoint(String value, int index, int start, int end) {
        char c = value.charAt(index);
        if (Character.isHighSurrogate(c) && index + 1 < end && Character.isLowSurrogate(value.charAt(index + 1))) {
            return -Character.toCodePoint(c, value.charAt(index + 1));
        }
        if (Character.isLowSurrogate(c) && index > start && Character.isHighSurrogate(value.charAt(index - 1))) {
            return Character.toCodePoint(value.charAt(index - 1), c);
        }
        return c;
    }

    private record Comparison(String label, String left, int leftOffset, String right, int rightOffset,
                              int length, boolean whole, boolean expected) { }

    private static void checkShortRanges() {
        // Different suffixes catch a prefix read extending beyond the requested range.
        for (int length = 0; length < Long.BYTES; length++) {
            String left = ">" + "a".repeat(length) + "!";
            String right = "##" + "A".repeat(length) + "?";
            if (!left.regionMatches(true, 1, right, 2, length)) {
                throw new AssertionError("short range length=" + length);
            }
        }
        if (!new String(new char[0]).equalsIgnoreCase(new String(new char[0]))
                || !"a".regionMatches(true, 1, "b", 1, 0)
                || !"a".regionMatches(true, 1, "b", 1, -1)) {
            throw new AssertionError("empty or negative range");
        }
    }
}
