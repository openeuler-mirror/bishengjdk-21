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

/*
 * @test
 * @summary Preserves equalsIgnoreCase semantics for malformed UTF-16
 * @run main/othervm -XX:+CompactStrings EqualsIgnoreCaseMalformedSurrogates
 * @run main/othervm -XX:-CompactStrings EqualsIgnoreCaseMalformedSurrogates
 */

import java.util.Random;

public class EqualsIgnoreCaseMalformedSurrogates {
    private static final int PHASE_LENGTH = 258;
    private static final int PHASE_ANCHOR = 127;
    private static final int POST_PAIR_ANCHOR = 193;
    private static final int CORPUS_ROUNDS = 128;
    private static final long RANDOM_SEED = 0x5eed_c0de_25L;
    private static final int[] LENGTHS = {
            0, 1, 7, 8, 15, 16, 17, 31, 32, 33, 65
    };

    public static void main(String[] args) {
        checkShortCases();
        checkLongCases();
        checkSeededCorpus();
    }

    private static void checkShortCases() {
        String first = "\uD801\uDC00X";
        String second = "\uD801\uD801\uDC28";
        checkBoth("legacy malformed pairing", first, second, true);
        checkEquals("reverse legacy malformed pairing",
                second.equalsIgnoreCase(first), true);

        checkBoth("equal lone high surrogate", "\uD800", "\uD800", true);
        checkBoth("equal lone low surrogate", "\uDC00", "\uDC00", true);
        checkBoth("different lone surrogates", "\uD800", "\uDC00", false);
        checkBoth("supplementary case pair",
                "\uD801\uDC00", "\uD801\uDC28", true);

        String firstContainer = "p" + first + "!";
        String secondContainer = "qq" + second + "?";
        checkEquals("one-code-unit prefix",
                firstContainer.regionMatches(true, 1,
                        secondContainer, 2, 1), true);
        checkEquals("truncated malformed pair",
                firstContainer.regionMatches(true, 1,
                        secondContainer, 2, 2), false);
    }

    private static void checkLongCases() {
        String lowerPrefix = "a".repeat(PHASE_ANCHOR);
        String upperPrefix = "A".repeat(PHASE_ANCHOR);
        String lowerTail = "b".repeat(129);
        String upperTail = "B".repeat(129);

        String legalLeft = lowerPrefix + "\uD801\uDC00" + lowerTail;
        String legalMatch = upperPrefix + "\uD801\uDC28" + upperTail;
        String legalMismatch =
                upperPrefix + "\uD801\uDC28" + "B".repeat(128) + "!";
        checkPhaseLength(legalLeft, legalMatch, legalMismatch);
        checkBoth("long legal supplementary match",
                legalLeft, legalMatch, true);
        checkBoth("long legal supplementary mismatch",
                legalLeft, legalMismatch, false);

        String malformedLeft = lowerPrefix + "\uD800X" + lowerTail;
        String malformedMatch = upperPrefix + "\uD800x" + upperTail;
        String earlierMismatch =
                "?" + "A".repeat(PHASE_ANCHOR - 1)
                        + "\uD800x" + upperTail;
        checkPhaseLength(malformedLeft, malformedMatch, earlierMismatch);
        checkBoth("long malformed checkpoint",
                malformedLeft, malformedMatch, true);
        checkBoth("mismatch before malformed checkpoint",
                malformedLeft, earlierMismatch, false);

        checkPostPairCase('\uD800', "lone high surrogate");
        checkPostPairCase('\uDC00', "lone low surrogate");
    }

    private static void checkPostPairCase(char surrogate, String label) {
        int middleLength = POST_PAIR_ANCHOR - PHASE_ANCHOR - 2;
        int tailLength = PHASE_LENGTH - POST_PAIR_ANCHOR - 1;
        String left = "a".repeat(PHASE_ANCHOR)
                + "\uD801\uDC00"
                + "b".repeat(middleLength)
                + surrogate
                + "c".repeat(tailLength);
        String match = "A".repeat(PHASE_ANCHOR)
                + "\uD801\uDC28"
                + "B".repeat(middleLength)
                + surrogate
                + "C".repeat(tailLength);
        String earlierMismatch = "A".repeat(PHASE_ANCHOR)
                + "\uD801\uDC28"
                + "B".repeat(middleLength - 1) + "!"
                + surrogate
                + "C".repeat(tailLength);
        checkPhaseLength(left, match, earlierMismatch);
        checkBoth("post-pair " + label, left, match, true);
        checkBoth("mismatch before post-pair " + label,
                left, earlierMismatch, false);
    }

    private static void checkSeededCorpus() {
        Random random = new Random(RANDOM_SEED);
        for (int round = 0; round < CORPUS_ROUNDS; round++) {
            for (int len : LENGTHS) {
                char[] first = randomCodeUnits(random, len);
                char[] second = first.clone();
                int scenario = (round + len) & 3;
                boolean expected = true;

                if (scenario == 1) {
                    addAsciiCasePairs(first, second, round);
                } else if (scenario == 2 && len != 0) {
                    int mismatch = Math.floorMod(round * 17 + len, len);
                    first[mismatch] = '0';
                    second[mismatch] = '1';
                    expected = false;
                } else if (scenario == 3 && len >= 3) {
                    int tail = len - 3;
                    first[tail] = '\uD801';
                    first[tail + 1] = '\uDC00';
                    first[tail + 2] = 'X';
                    second[tail] = '\uD801';
                    second[tail + 1] = '\uD801';
                    second[tail + 2] = '\uDC28';
                }

                checkBoth("round=" + round + ", length=" + len,
                        new String(first), new String(second), expected);
            }
        }
    }

    private static char[] randomCodeUnits(Random random, int length) {
        char[] value = new char[length];
        for (int i = 0; i < value.length; i++) {
            value[i] = switch (random.nextInt(8)) {
                case 0 -> (char) (0xD800 + random.nextInt(0x400));
                case 1 -> (char) (0xDC00 + random.nextInt(0x400));
                case 2 -> (char) ('A' + random.nextInt(26));
                case 3 -> (char) ('a' + random.nextInt(26));
                case 4 -> (char) random.nextInt(1 << 16);
                case 5 -> (char) (0xFF21 + random.nextInt(26));
                case 6 -> (char) (0xFF41 + random.nextInt(26));
                default -> (char) random.nextInt(0x100);
            };
        }
        return value;
    }

    private static void addAsciiCasePairs(char[] first, char[] second,
                                          int round) {
        for (int i = round & 3; i < first.length; i += 5) {
            int letter = Math.floorMod(round + i, 26);
            first[i] = (char) ('A' + letter);
            second[i] = (char) ('a' + letter);
        }
    }

    private static void checkBoth(String label, String first, String second,
                                  boolean expected) {
        checkEquals(label + " equalsIgnoreCase",
                first.equalsIgnoreCase(second), expected);
        String firstContainer = "p" + first + "!";
        String secondContainer = "qq" + second + "?";
        checkEquals(label + " regionMatches",
                firstContainer.regionMatches(true, 1,
                        secondContainer, 2, first.length()), expected);
    }

    private static void checkPhaseLength(String... values) {
        for (String value : values) {
            if (value.length() != PHASE_LENGTH) {
                throw new AssertionError("phase length: expected="
                        + PHASE_LENGTH + ", actual=" + value.length());
            }
        }
    }

    private static void checkEquals(String label, boolean actual,
                                    boolean expected) {
        if (actual != expected) {
            throw new AssertionError(label + ": expected=" + expected
                    + ", actual=" + actual);
        }
    }
}
