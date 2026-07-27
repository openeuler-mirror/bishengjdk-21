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
 * @summary Verify the combined String case-conversion and fold protocol
 * @run main GenerateStringCaseMappingsTest
 */

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GenerateStringCaseMappingsTest {
    private static final int UNICODE_LIMIT = Character.MAX_CODE_POINT + 1;
    private static final int PAGE_SHIFT = 8;
    private static final int PAGE_SIZE = 1 << PAGE_SHIFT;
    private static final String GENERATOR_RELATIVE_PATH =
            "make/src/classes/build/tools/generatestringcasemappings/"
                    + "GenerateStringCaseMappings.java";

    public static void main(String[] args) throws Exception {
        Path repository = repositoryRoot();
        Path generator = repository.resolve(GENERATOR_RELATIVE_PATH);
        if (!Files.isRegularFile(generator)) {
            throw new AssertionError("RED: missing generator source: " + generator);
        }

        Path scratch = Files.createTempDirectory("string-case-mappings-test");
        Path unicodeData = scratch.resolve("UnicodeData.txt");
        Path specialCasing = scratch.resolve("SpecialCasing.txt");
        Path template = scratch.resolve("stringCaseMappings.hpp.template");
        Path firstOutput = scratch.resolve("first/stringCaseMappings.hpp");
        Path secondOutput = scratch.resolve("second/stringCaseMappings.hpp");

        Files.writeString(unicodeData, unicodeDataFixture(), StandardCharsets.UTF_8);
        Files.writeString(specialCasing, specialCasingFixture(), StandardCharsets.UTF_8);
        Files.writeString(template,
                "#ifndef STRING_CASE_MAPPINGS_TEST_HPP\n"
                        + "#define STRING_CASE_MAPPINGS_TEST_HPP\n"
                        + "@@STRING_CASE_TABLES@@\n"
                        + "#endif\n",
                StandardCharsets.UTF_8);

        runGenerator(generator, unicodeData, specialCasing, template, firstOutput);
        runGenerator(generator, unicodeData, specialCasing, template, secondOutput);

        byte[] firstBytes = Files.readAllBytes(firstOutput);
        byte[] secondBytes = Files.readAllBytes(secondOutput);
        if (!Arrays.equals(firstBytes, secondBytes)) {
            throw new AssertionError("same generator inputs produced different bytes");
        }

        verifyGeneratedProtocol(new String(firstBytes, StandardCharsets.UTF_8));

        verifyGeneratorFailure(generator, scratch, "utf16-width-change",
                unicodeDataLine("0041", "TEST BMP TO SUPPLEMENTARY", "Lu",
                        "", "10000") + "\n",
                "changes UTF-16 width");
    }

    private static Path repositoryRoot() {
        String testRoot = System.getProperty("test.root");
        if (testRoot == null) {
            throw new AssertionError("test.root is not set");
        }
        Path jdkTestRoot = Path.of(testRoot).toAbsolutePath().normalize();
        Path testDirectory = jdkTestRoot.getParent();
        if (testDirectory == null || testDirectory.getParent() == null) {
            throw new AssertionError("cannot derive repository root from test.root="
                    + testRoot);
        }
        return testDirectory.getParent();
    }

    private static void runGenerator(Path generator, Path unicodeData,
                                     Path specialCasing, Path template,
                                     Path output) throws Exception {
        ProcessResult result = invokeGenerator(generator, unicodeData,
                specialCasing, template, output);
        if (result.exitCode != 0) {
            throw new AssertionError("generator process exited with "
                    + result.exitCode + ":\n" + result.output);
        }
        if (!Files.isRegularFile(output)) {
            throw new AssertionError("generator did not create output: " + output
                    + "\n" + result.output);
        }
    }

    private static ProcessResult invokeGenerator(Path generator,
                                                  Path unicodeData,
                                                  Path specialCasing,
                                                  Path template,
                                                  Path output) throws Exception {
        Path java = Path.of(System.getProperty("java.home"), "bin",
                isWindows() ? "java.exe" : "java");
        ProcessBuilder builder = new ProcessBuilder(
                java.toString(), generator.toString(), unicodeData.toString(),
                specialCasing.toString(), template.toString(), output.toString());
        builder.directory(output.getParent().getParent().toFile());
        builder.redirectErrorStream(true);
        builder.environment().remove("CLASSPATH");

        Process process = builder.start();
        String processOutput = new String(process.getInputStream().readAllBytes(),
                StandardCharsets.UTF_8);
        int exitCode = process.waitFor();
        return new ProcessResult(exitCode, processOutput);
    }

    private static void verifyGeneratorFailure(Path generator, Path scratch,
                                               String fixtureName,
                                               String unicodeDataFixture,
                                               String expectedDiagnostic)
            throws Exception {
        Path fixture = scratch.resolve(fixtureName);
        Files.createDirectories(fixture);
        Path unicodeData = fixture.resolve("UnicodeData.txt");
        Path specialCasing = fixture.resolve("SpecialCasing.txt");
        Path template = fixture.resolve("stringCaseMappings.hpp.template");
        Path output = fixture.resolve("output/stringCaseMappings.hpp");
        Files.writeString(unicodeData, unicodeDataFixture,
                StandardCharsets.UTF_8);
        Files.writeString(specialCasing, "", StandardCharsets.UTF_8);
        Files.writeString(template, "@@STRING_CASE_TABLES@@\n",
                StandardCharsets.UTF_8);

        ProcessResult result = invokeGenerator(generator, unicodeData,
                specialCasing, template, output);
        if (result.exitCode == 0) {
            throw new AssertionError(fixtureName
                    + " unexpectedly generated output successfully");
        }
        if (!result.output.contains(expectedDiagnostic)) {
            throw new AssertionError(fixtureName + " diagnostic did not contain '"
                    + expectedDiagnostic + "':\n" + result.output);
        }
    }

    private static boolean isWindows() {
        return System.getProperty("os.name").toLowerCase(Locale.ROOT)
                .contains("windows");
    }

    private static void verifyGeneratedProtocol(String output) {
        requirePattern(output, "static\\s+constexpr\\s+int\\s+"
                + "string_case_page_shift\\s*=\\s*8\\s*;",
                "page-shift protocol constant");
        requirePattern(output, "static\\s+constexpr\\s+int\\s+"
                + "string_case_page_mask\\s*=\\s*0x0*ff\\s*;",
                "page-mask protocol constant");
        requirePattern(output, "static\\s+constexpr\\s+uint16_t\\s+"
                + "string_case_identity_sentinel\\s*=\\s*0x0{4}\\s*;",
                "identity-sentinel protocol constant");
        requirePattern(output, "static\\s+constexpr\\s+uint16_t\\s+"
                + "string_case_fallback_sentinel\\s*=\\s*0xd800\\s*;",
                "fallback-sentinel protocol constant");

        CppArray lowerPageIndex = parseArray(output, "uint8_t",
                "string_case_lower_page_index", false);
        CppArray lowerMap = parseArray(output, "uint16_t",
                "string_case_lower_map", true);
        CppArray upperPageIndex = parseArray(output, "uint8_t",
                "string_case_upper_page_index", false);
        CppArray upperMap = parseArray(output, "uint16_t",
                "string_case_upper_map", true);
        verifyBmpTable(lowerPageIndex, lowerMap, "lower");
        verifyBmpTable(upperPageIndex, upperMap, "upper");

        checkEntry(0x0049, 0x0069, lowerPageIndex, lowerMap,
                "BMP simple lowercase");
        checkEntry(0x017f, 0x0053, upperPageIndex, upperMap,
                "BMP simple uppercase");
        checkEntry(0x00df, 0xd800, upperPageIndex, upperMap,
                "one-to-many uppercase fallback");
        checkEntry(0x0130, 0xd800, lowerPageIndex, lowerMap,
                "one-to-many lowercase fallback");
        checkEntry(0x03a3, 0xd800, lowerPageIndex, lowerMap,
                "context-sensitive lowercase fallback");
        checkEntry(0xd800, 0xd800, lowerPageIndex, lowerMap,
                "lowercase surrogate fallback");
        checkEntry(0xd800, 0xd800, upperPageIndex, upperMap,
                "uppercase surrogate fallback");

        CppArray foldPageIndex = parseArray(output, "uint8_t",
                "string_case_fold_page_index", false);
        CppArray foldMap = parseArray(output, "uint32_t",
                "string_case_fold_map", true);
        checkEquals(UNICODE_LIMIT / PAGE_SIZE, foldPageIndex.firstDimension,
                "full Unicode page-index count");
        checkEquals(1, foldPageIndex.secondDimension, "page-index rank");
        checkEquals(PAGE_SIZE, foldMap.secondDimension, "fold row width");
        if (foldMap.firstDimension == 0 || foldMap.firstDimension > 256) {
            throw new AssertionError("fold row count does not fit a byte: "
                    + foldMap.firstDimension);
        }
        for (long row : foldPageIndex.values) {
            if (row < 0 || row >= foldMap.firstDimension || row > 0xff) {
                throw new AssertionError("invalid byte page index: " + row);
            }
        }
        for (int slot = 0; slot < PAGE_SIZE; slot++) {
            checkEquals(0, foldMap.values[slot],
                    "row zero identity sentinel at slot " + slot);
        }

        int[] upper = identityMappings(UNICODE_LIMIT);
        int[] lower = identityMappings(UNICODE_LIMIT);
        upper[0x017f] = 0x0053;
        upper[0x03c2] = 0x03a3;
        upper[0xe000] = 0xe001;
        lower[0x0049] = 0x0069;
        lower[0x004b] = 0x006b;
        lower[0x0053] = 0x0073;
        lower[0x0130] = 0x0069;
        lower[0x03a3] = 0x03c3;
        lower[0x212a] = 0x006b;
        lower[0xe001] = 0xe002;
        lower[0x10400] = 0x10428;
        lower[0x10401] = 0x10429;
        lower[0x10403] = 0x1042b;
        lower[0x10410] = 0x10450;
        upper[0x10428] = 0x10400;
        upper[0x10429] = 0x10401;
        upper[0x1042b] = 0x10403;
        upper[0x10450] = 0x10410;

        for (int codePoint = 0; codePoint < UNICODE_LIMIT; codePoint++) {
            long expected = lower[upper[codePoint]];
            if (expected == codePoint) {
                expected = 0;
            } else if (codePoint != 0 && expected == 0) {
                throw new AssertionError("fixture maps nonzero U+"
                        + hex(codePoint) + " to the identity sentinel");
            }
            checkEquals(expected, tableEntry(codePoint, foldPageIndex, foldMap),
                    "full fold at U+" + hex(codePoint));
        }

        checkEntry(0x0021, 0x0000, foldPageIndex, foldMap, "unmapped identity");
        checkEntry(0x00df, 0x0000, foldPageIndex, foldMap,
                "one-to-many sharp-s stays identity");
        checkEntry(0x0130, 0x0069, foldPageIndex, foldMap,
                "simple dotted-I mapping wins over one-to-many SpecialCasing");
        checkEntry(0x017f, 0x0073, foldPageIndex, foldMap, "long-s fold");
        checkEntry(0x03c2, 0x03c3, foldPageIndex, foldMap, "final-sigma fold");
        checkEntry(0x212a, 0x006b, foldPageIndex, foldMap, "Kelvin-sign fold");
        checkEntry(0x0049, 0x0069, foldPageIndex, foldMap,
                "locale-specific Turkish rule excluded");
        checkEntry(0x03a3, 0x03c3, foldPageIndex, foldMap,
                "context-specific final-sigma rule excluded");
        checkEntry(0xd800, 0x0000, foldPageIndex, foldMap, "high surrogate identity");
        checkEntry(0xdc00, 0x0000, foldPageIndex, foldMap, "low surrogate identity");
        checkEntry(0xdfff, 0x0000, foldPageIndex, foldMap, "surrogate boundary identity");
        checkEntry(0xe000, 0xe002, foldPageIndex, foldMap,
                "fixture-defined upper-then-lower composition");
        checkEntry(0x10400, 0x10428, foldPageIndex, foldMap,
                "supplementary adjacent range start");
        checkEntry(0x10401, 0x10429, foldPageIndex, foldMap,
                "supplementary adjacent range end");
        checkEntry(0x10402, 0, foldPageIndex, foldMap,
                "supplementary range hole");
        checkEntry(0x10403, 0x1042b, foldPageIndex, foldMap,
                "supplementary range after hole");
        checkEntry(0x10410, 0x10450, foldPageIndex, foldMap,
                "supplementary different delta");
        checkEquals(0, foldPageIndex.values[0x02], "identity page uses row zero");
        checkEquals(0, foldPageIndex.values[0xd8], "surrogate page uses row zero");
    }

    private static void verifyBmpTable(CppArray pageIndex, CppArray map,
                                       String operation) {
        checkEquals(1 << (16 - PAGE_SHIFT), pageIndex.firstDimension,
                operation + " BMP page-index count");
        checkEquals(1, pageIndex.secondDimension,
                operation + " page-index rank");
        checkEquals(PAGE_SIZE, map.secondDimension,
                operation + " row width");
        if (map.firstDimension == 0 || map.firstDimension > 256) {
            throw new AssertionError(operation
                    + " row count does not fit a byte: " + map.firstDimension);
        }
        for (long row : pageIndex.values) {
            if (row < 0 || row >= map.firstDimension || row > 0xff) {
                throw new AssertionError("invalid " + operation
                        + " byte page index: " + row);
            }
        }
        for (int slot = 0; slot < PAGE_SIZE; slot++) {
            checkEquals(0, map.values[slot],
                    operation + " row zero identity sentinel at slot " + slot);
        }
    }

    private static int[] identityMappings(int length) {
        int[] mappings = new int[length];
        for (int codePoint = 0; codePoint < mappings.length; codePoint++) {
            mappings[codePoint] = codePoint;
        }
        return mappings;
    }

    private static long tableEntry(int codePoint, CppArray pageIndex,
                                   CppArray foldMap) {
        int row = Math.toIntExact(pageIndex.values[codePoint >>> PAGE_SHIFT]);
        return foldMap.values[row * PAGE_SIZE + (codePoint & (PAGE_SIZE - 1))];
    }

    private static void checkEntry(int codePoint, long expected,
                                   CppArray pageIndex, CppArray foldMap,
                                   String description) {
        checkEquals(expected, tableEntry(codePoint, pageIndex, foldMap),
                description + " at U+" + hex(codePoint));
    }

    private static void requirePattern(String output, String regex,
                                       String description) {
        if (!Pattern.compile(regex).matcher(output).find()) {
            throw new AssertionError("missing " + description + ": " + regex);
        }
    }

    private static CppArray parseArray(String output, String type, String name,
                                       boolean twoDimensional) {
        String dimensions = twoDimensional
                ? "\\[(\\d+)\\]\\s*\\[(\\d+)\\]"
                : "\\[(\\d+)\\]";
        Pattern declaration = Pattern.compile("static\\s+constexpr\\s+" + type
                + "\\s+" + name + "\\s*" + dimensions + "\\s*=\\s*\\{");
        Matcher matcher = declaration.matcher(output);
        if (!matcher.find()) {
            throw new AssertionError("missing " + type + " array " + name);
        }

        int firstDimension = Integer.parseInt(matcher.group(1));
        int secondDimension = twoDimensional
                ? Integer.parseInt(matcher.group(2)) : 1;
        int openingBrace = matcher.end() - 1;
        int closingBrace = matchingBrace(output, openingBrace);
        String initializer = output.substring(openingBrace + 1, closingBrace)
                .replaceAll("(?m)//.*$", "")
                .replaceAll("(?s)/\\*.*?\\*/", "");

        Matcher valuesMatcher = Pattern.compile("0[xX]([0-9a-fA-F]+)")
                .matcher(initializer);
        List<Long> parsed = new ArrayList<>();
        while (valuesMatcher.find()) {
            parsed.add(Long.parseUnsignedLong(valuesMatcher.group(1), 16));
        }
        int expectedCount = Math.multiplyExact(firstDimension, secondDimension);
        checkEquals(expectedCount, parsed.size(), name + " initializer count");
        long[] values = new long[parsed.size()];
        for (int index = 0; index < parsed.size(); index++) {
            values[index] = parsed.get(index);
        }
        return new CppArray(firstDimension, secondDimension, values);
    }

    private static int matchingBrace(String text, int openingBrace) {
        int depth = 0;
        for (int index = openingBrace; index < text.length(); index++) {
            char character = text.charAt(index);
            if (character == '{') {
                depth++;
            } else if (character == '}' && --depth == 0) {
                return index;
            }
        }
        throw new AssertionError("unterminated generated initializer");
    }

    private static String unicodeDataFixture() {
        return String.join("\n",
                unicodeDataLine("0049", "LATIN CAPITAL LETTER I", "Lu", "", "0069"),
                unicodeDataLine("004B", "LATIN CAPITAL LETTER K", "Lu", "", "006B"),
                unicodeDataLine("0053", "LATIN CAPITAL LETTER S", "Lu", "", "0073"),
                unicodeDataLine("00DF", "LATIN SMALL LETTER SHARP S", "Ll", "", ""),
                unicodeDataLine("0130", "LATIN CAPITAL LETTER I WITH DOT ABOVE", "Lu", "", "0069"),
                unicodeDataLine("017F", "LATIN SMALL LETTER LONG S", "Ll", "0053", ""),
                unicodeDataLine("03A3", "GREEK CAPITAL LETTER SIGMA", "Lu", "", "03C3"),
                unicodeDataLine("03C2", "GREEK SMALL LETTER FINAL SIGMA", "Ll", "03A3", ""),
                unicodeDataLine("212A", "KELVIN SIGN", "Lu", "", "006B"),
                unicodeDataLine("E000", "TEST PRIVATE USE UPPER", "Co", "E001", ""),
                unicodeDataLine("E001", "TEST PRIVATE USE LOWER", "Co", "", "E002"),
                unicodeDataLine("10400", "TEST SUPPLEMENTARY UPPER A", "Lu", "", "10428"),
                unicodeDataLine("10401", "TEST SUPPLEMENTARY UPPER B", "Lu", "", "10429"),
                unicodeDataLine("10403", "TEST SUPPLEMENTARY UPPER AFTER HOLE", "Lu", "", "1042B"),
                unicodeDataLine("10410", "TEST SUPPLEMENTARY DIFFERENT DELTA", "Lu", "", "10450"),
                unicodeDataLine("10428", "TEST SUPPLEMENTARY LOWER A", "Ll", "10400", ""),
                unicodeDataLine("10429", "TEST SUPPLEMENTARY LOWER B", "Ll", "10401", ""),
                unicodeDataLine("1042B", "TEST SUPPLEMENTARY LOWER AFTER HOLE", "Ll", "10403", ""),
                unicodeDataLine("10450", "TEST SUPPLEMENTARY LOWER DIFFERENT DELTA", "Ll", "10410", "")) + "\n";
    }

    private static String unicodeDataLine(String codePoint, String name,
                                          String category, String upper,
                                          String lower) {
        String[] fields = {
                codePoint, name, category, "0", "L", "", "", "", "", "N",
                "", "", upper, lower, upper
        };
        return String.join(";", fields);
    }

    private static String specialCasingFixture() {
        return "# Code; Lower; Title; Upper; Condition\n"
                + "00DF; 00DF; 0053 0073; 0053 0053; # one-to-many\n"
                + "0130; 0069 0307; 0130; 0130; # one-to-many\n"
                + "0049; 0131; 0049; 0049; tr; # locale-specific\n"
                + "03A3; 03C2; 03A3; 03A3; Final_Sigma; # context-specific\n";
    }

    private static String hex(int value) {
        return String.format(Locale.ROOT, "%04X", value);
    }

    private static void checkEquals(long expected, long actual, String message) {
        if (expected != actual) {
            throw new AssertionError(message + ": expected 0x"
                    + Long.toHexString(expected) + ", got 0x"
                    + Long.toHexString(actual));
        }
    }

    private record CppArray(int firstDimension, int secondDimension,
                            long[] values) {}

    private record ProcessResult(int exitCode, String output) {}
}
