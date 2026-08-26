/*
 * Copyright (c) 2026, Huawei Technologies Co., Ltd. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
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

package build.tools.generatestringcasemappings;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Generates the immutable AArch64 simple-case conversion and fold tables.
 *
 * <p>Usage: {@code GenerateStringCaseMappings
 * <UnicodeData.txt> <SpecialCasing.txt> <template> <output>}
 */
public final class GenerateStringCaseMappings {
    private static final String TABLES_MARKER = "@@STRING_CASE_TABLES@@";

    private static final int PAGE_SHIFT = 8;
    private static final int PAGE_SIZE = 1 << PAGE_SHIFT;
    private static final int BMP_CODE_POINT_COUNT = 1 << 16;
    private static final int PAGE_COUNT = BMP_CODE_POINT_COUNT / PAGE_SIZE;
    private static final int UNICODE_CODE_POINT_COUNT = 0x110000;
    private static final int MAX_CODE_POINT = UNICODE_CODE_POINT_COUNT - 1;
    private static final int MIN_SURROGATE = 0xd800;
    private static final int MAX_SURROGATE = 0xdfff;

    private static final char IDENTITY = 0;
    private static final char FALLBACK = MIN_SURROGATE;
    private static final int NO_SIMPLE_MAPPING = -1;

    private enum Operation {
        LOWER("lower"),
        UPPER("upper");

        private final String tableName;

        Operation(String tableName) {
            this.tableName = tableName;
        }
    }

    private record MappingTable(int[] pageIndexes, List<char[]> rows) {
        int entry(int codePoint) {
            int page = codePoint >>> PAGE_SHIFT;
            return rows.get(pageIndexes[page])[codePoint & (PAGE_SIZE - 1)];
        }
    }

    private record FoldTable(int[] pageIndexes, List<int[]> rows) {
        int entry(int codePoint) {
            int page = codePoint >>> PAGE_SHIFT;
            return rows.get(pageIndexes[page])[codePoint & (PAGE_SIZE - 1)];
        }
    }

    private record CaseData(int[] lowerTargets, int[] upperTargets,
                            boolean[] lowerFallback, boolean[] upperFallback) {
        int map(Operation operation, int codePoint) {
            boolean fallback = operation == Operation.LOWER
                    ? lowerFallback[codePoint] : upperFallback[codePoint];
            if (fallback || isSurrogate(codePoint)) {
                return NO_SIMPLE_MAPPING;
            }
            int target = operation == Operation.LOWER
                    ? lowerTargets[codePoint] : upperTargets[codePoint];
            return isBmpNonSurrogate(target) ? target : NO_SIMPLE_MAPPING;
        }

        int fold(int codePoint) {
            return lowerTargets[upperTargets[codePoint]];
        }
    }

    private record InputLine(Path file, int number, String text) {
        IllegalArgumentException error(String message) {
            return new IllegalArgumentException(file + ":" + number + ": "
                    + message + ": " + text);
        }
    }

    private GenerateStringCaseMappings() {}

    public static void main(String[] args) throws Exception {
        if (args.length != 4) {
            throw new IllegalArgumentException(
                    "Usage: GenerateStringCaseMappings <UnicodeData.txt> "
                    + "<SpecialCasing.txt> <template> <output>");
        }

        Path unicodeDataPath = Path.of(args[0]);
        Path specialCasingPath = Path.of(args[1]);
        Path templatePath = Path.of(args[2]);
        Path outputPath = Path.of(args[3]);

        CaseData caseData = readCaseData(unicodeDataPath, specialCasingPath);
        verifySimpleMappings(caseData);
        MappingTable lower = buildTable(Operation.LOWER, caseData);
        MappingTable upper = buildTable(Operation.UPPER, caseData);
        int[] foldTargets = buildAndVerifyFullFold(caseData);
        FoldTable fold = buildFoldTable(foldTargets);

        verifyTable(Operation.LOWER, lower, caseData);
        verifyTable(Operation.UPPER, upper, caseData);
        verifyFoldTable(fold, foldTargets);

        String template = normalizeLineEndings(
                Files.readString(templatePath, StandardCharsets.UTF_8));
        String output = renderTemplate(template, lower, upper, fold);
        writeAtomically(outputPath, output);
    }

    private static CaseData readCaseData(Path unicodeDataPath,
                                         Path specialCasingPath)
            throws IOException {
        int[] lowerTargets = identityMappings();
        int[] upperTargets = identityMappings();
        boolean[] lowerFallback = new boolean[BMP_CODE_POINT_COUNT];
        boolean[] upperFallback = new boolean[BMP_CODE_POINT_COUNT];

        parseUnicodeData(unicodeDataPath, lowerTargets, upperTargets);
        parseSpecialCasing(specialCasingPath, lowerFallback, upperFallback);
        return new CaseData(lowerTargets, upperTargets,
                lowerFallback, upperFallback);
    }

    private static int[] identityMappings() {
        int[] mappings = new int[UNICODE_CODE_POINT_COUNT];
        for (int codePoint = 0; codePoint < mappings.length; codePoint++) {
            mappings[codePoint] = codePoint;
        }
        return mappings;
    }

    private static void parseUnicodeData(Path path, int[] lowerTargets,
                                         int[] upperTargets)
            throws IOException {
        boolean[] seen = new boolean[UNICODE_CODE_POINT_COUNT];
        try (BufferedReader reader = Files.newBufferedReader(
                path, StandardCharsets.UTF_8)) {
            String text;
            int lineNumber = 0;
            while ((text = reader.readLine()) != null) {
                InputLine line = new InputLine(path, ++lineNumber, text);
                String[] fields = text.split(";", -1);
                if (fields.length != 15) {
                    throw line.error("expected 15 UnicodeData fields, found "
                            + fields.length);
                }

                int source = parseCodePoint(fields[0], "source", line);
                int upper = parseOptionalCodePoint(
                        fields[12], "simple uppercase mapping", source, line);
                int lower = parseOptionalCodePoint(
                        fields[13], "simple lowercase mapping", source, line);
                if (seen[source]) {
                    throw line.error("duplicate source U+" + hex(source));
                }
                seen[source] = true;
                upperTargets[source] = upper;
                lowerTargets[source] = lower;
            }
        }
    }

    private static void parseSpecialCasing(Path path, boolean[] lowerFallback,
                                           boolean[] upperFallback)
            throws IOException {
        Set<Integer> unconditionalSources = new HashSet<>();
        try (BufferedReader reader = Files.newBufferedReader(
                path, StandardCharsets.UTF_8)) {
            String text;
            int lineNumber = 0;
            while ((text = reader.readLine()) != null) {
                InputLine line = new InputLine(path, ++lineNumber, text);
                String content = stripComment(text).trim();
                if (content.isEmpty()) {
                    continue;
                }

                String[] fields = content.split(";", -1);
                if (fields.length == 6 && fields[5].trim().isEmpty()
                        && !fields[4].trim().isEmpty()) {
                    fields = Arrays.copyOf(fields, 5);
                }
                if (fields.length != 5) {
                    throw line.error("expected 5 SpecialCasing fields, found "
                            + fields.length);
                }

                int source = parseCodePoint(fields[0], "source", line);
                int[] lower = parseCodePointSequence(
                        fields[1], "lower mapping", line);
                int[] upper = parseCodePointSequence(
                        fields[3], "upper mapping", line);
                String conditionField = fields[4].trim();
                List<String> conditions = conditionField.isEmpty()
                        ? List.of()
                        : List.of(conditionField.split("\\s+"));

                if (conditions.isEmpty()
                        && !unconditionalSources.add(source)) {
                    throw line.error("duplicate unconditional record for U+"
                            + hex(source));
                }
                if (source >= BMP_CODE_POINT_COUNT) {
                    continue;
                }

                if (conditions.isEmpty()) {
                    if (lower.length != 1
                            || !isBmpNonSurrogate(lower[0])) {
                        lowerFallback[source] = true;
                    }
                    upperFallback[source] = true;
                } else if (conditions.stream().noneMatch(
                        GenerateStringCaseMappings::isLocale)) {
                    lowerFallback[source] = true;
                }
            }
        }
    }

    private static int parseOptionalCodePoint(String value, String field,
                                              int defaultValue,
                                              InputLine line) {
        return value.trim().isEmpty()
                ? defaultValue
                : parseCodePoint(value, field, line);
    }

    private static int[] parseCodePointSequence(String value, String field,
                                                InputLine line) {
        String sequence = value.trim();
        if (sequence.isEmpty()) {
            return new int[0];
        }
        String[] values = sequence.split("\\s+");
        int[] codePoints = new int[values.length];
        for (int i = 0; i < values.length; i++) {
            codePoints[i] = parseCodePoint(values[i], field, line);
        }
        return codePoints;
    }

    private static int parseCodePoint(String value, String field,
                                      InputLine line) {
        String digits = value.trim();
        if (!isAsciiHex(digits)) {
            throw line.error("invalid ASCII hexadecimal " + field
                    + " '" + value + "'");
        }

        long parsed;
        try {
            parsed = Long.parseLong(digits, 16);
        } catch (NumberFormatException exception) {
            throw line.error(field + " is outside the Unicode range '"
                    + value + "'");
        }
        if (parsed > MAX_CODE_POINT) {
            throw line.error(field + " is outside the Unicode range '"
                    + value + "'");
        }
        return (int)parsed;
    }

    private static boolean isAsciiHex(String value) {
        if (value.isEmpty()) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (!((c >= '0' && c <= '9')
                    || (c >= 'A' && c <= 'F')
                    || (c >= 'a' && c <= 'f'))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isLocale(String condition) {
        if (condition.isEmpty()) {
            return false;
        }
        char first = condition.charAt(0);
        return first >= 'a' && first <= 'z';
    }

    private static String stripComment(String value) {
        int comment = value.indexOf('#');
        return comment < 0 ? value : value.substring(0, comment);
    }

    private static void verifySimpleMappings(CaseData caseData) {
        for (int codePoint = 0; codePoint < UNICODE_CODE_POINT_COUNT;
                codePoint++) {
            verifyMapping("simple uppercase", codePoint,
                    caseData.upperTargets()[codePoint]);
            verifyMapping("simple lowercase", codePoint,
                    caseData.lowerTargets()[codePoint]);
        }
    }

    private static void verifyMapping(String operation, int source,
                                      int target) {
        if (characterWidth(source) != characterWidth(target)) {
            throw new IllegalStateException(operation + " changes UTF-16 width: U+"
                    + hex(source) + " -> U+" + hex(target));
        }
        if ((isSurrogate(source) && target != source)
                || (!isSurrogate(source) && isSurrogate(target))) {
            throw new IllegalStateException(operation + " has invalid surrogate "
                    + "mapping: U+" + hex(source) + " -> U+" + hex(target));
        }
    }

    private static int[] buildAndVerifyFullFold(CaseData caseData) {
        int[] foldTargets = new int[UNICODE_CODE_POINT_COUNT];
        for (int codePoint = 0; codePoint < foldTargets.length; codePoint++) {
            int fold = caseData.fold(codePoint);
            if (codePoint != 0 && fold == IDENTITY) {
                throw new IllegalStateException("Nonzero U+" + hex(codePoint)
                        + " folds to the identity sentinel");
            }
            verifyMapping("fold", codePoint, fold);
            foldTargets[codePoint] = fold;
        }
        return foldTargets;
    }

    private static int characterWidth(int codePoint) {
        return codePoint < BMP_CODE_POINT_COUNT ? 1 : 2;
    }

    private static MappingTable buildTable(Operation operation,
                                           CaseData caseData) {
        char[][] pages = new char[PAGE_COUNT][PAGE_SIZE];
        for (int codePoint = 0; codePoint < BMP_CODE_POINT_COUNT; codePoint++) {
            pages[codePoint >>> PAGE_SHIFT][codePoint & (PAGE_SIZE - 1)] =
                    encode(codePoint, caseData.map(operation, codePoint));
        }

        List<char[]> rows = new ArrayList<>();
        rows.add(new char[PAGE_SIZE]); // Row zero is always the identity row.
        int[] pageIndexes = new int[PAGE_COUNT];
        for (int page = 0; page < PAGE_COUNT; page++) {
            int row = findRow(rows, pages[page]);
            if (row < 0) {
                row = rows.size();
                rows.add(pages[page].clone());
            }
            pageIndexes[page] = row;
        }

        if (rows.size() > 1 << Byte.SIZE) {
            throw new IllegalStateException(operation + " needs " + rows.size()
                    + " rows; page indexes no longer fit in uint8_t");
        }
        return new MappingTable(pageIndexes, List.copyOf(rows));
    }

    private static char encode(int source, int target) {
        if (target == NO_SIMPLE_MAPPING) {
            return FALLBACK;
        }
        if (target == source) {
            return IDENTITY;
        }
        if (target == IDENTITY || target == FALLBACK
                || !isBmpNonSurrogate(target)) {
            throw new IllegalStateException("Unencodable mapping U+"
                    + hex(source) + " -> U+" + hex(target));
        }
        return (char)target;
    }

    private static int findRow(List<char[]> rows, char[] candidate) {
        for (int row = 0; row < rows.size(); row++) {
            if (Arrays.equals(rows.get(row), candidate)) {
                return row;
            }
        }
        return -1;
    }

    private static FoldTable buildFoldTable(int[] foldTargets) {
        int pageCount = UNICODE_CODE_POINT_COUNT / PAGE_SIZE;
        int[][] pages = new int[pageCount][PAGE_SIZE];
        for (int codePoint = 0; codePoint < UNICODE_CODE_POINT_COUNT;
                codePoint++) {
            int fold = foldTargets[codePoint];
            pages[codePoint >>> PAGE_SHIFT][codePoint & (PAGE_SIZE - 1)] =
                    fold == codePoint ? IDENTITY : fold;
        }

        List<int[]> rows = new ArrayList<>();
        rows.add(new int[PAGE_SIZE]); // Row zero is always the identity row.
        int[] pageIndexes = new int[pageCount];
        for (int page = 0; page < pageCount; page++) {
            int row = findFoldRow(rows, pages[page]);
            if (row < 0) {
                row = rows.size();
                rows.add(pages[page].clone());
            }
            pageIndexes[page] = row;
        }

        if (rows.size() > 1 << Byte.SIZE) {
            throw new IllegalStateException("FOLD needs " + rows.size()
                    + " rows; page indexes no longer fit in uint8_t");
        }
        return new FoldTable(pageIndexes, List.copyOf(rows));
    }

    private static int findFoldRow(List<int[]> rows, int[] candidate) {
        for (int row = 0; row < rows.size(); row++) {
            if (Arrays.equals(rows.get(row), candidate)) {
                return row;
            }
        }
        return -1;
    }

    private static void verifyTable(Operation operation, MappingTable table,
                                    CaseData caseData) {
        if (table.pageIndexes().length != PAGE_COUNT) {
            throw new IllegalStateException(operation
                    + " has an invalid page-index count");
        }
        if (table.rows().isEmpty()
                || table.rows().size() > 1 << Byte.SIZE) {
            throw new IllegalStateException(operation + " has an invalid row count");
        }
        for (char entry : table.rows().get(0)) {
            if (entry != IDENTITY) {
                throw new IllegalStateException(operation
                        + " row zero is not identity");
            }
        }

        for (int page = 0; page < PAGE_COUNT; page++) {
            int row = table.pageIndexes()[page];
            if (row < 0 || row >= table.rows().size()) {
                throw new IllegalStateException(operation + " page " + page
                        + " has invalid row " + row);
            }
        }

        for (int codePoint = 0; codePoint < BMP_CODE_POINT_COUNT; codePoint++) {
            int expected = encode(
                    codePoint, caseData.map(operation, codePoint));
            int actual = table.entry(codePoint);
            if (actual != expected) {
                throw new IllegalStateException(operation + " mismatch at U+"
                        + hex(codePoint) + ": expected 0x" + hex(expected)
                        + ", got 0x" + hex(actual));
            }
            if (isSurrogate(codePoint) && actual != FALLBACK) {
                throw new IllegalStateException(operation + " surrogate U+"
                        + hex(codePoint) + " is not a fallback");
            }
            if (actual != IDENTITY && actual != FALLBACK
                    && isSurrogate(actual)) {
                throw new IllegalStateException(operation + " maps U+"
                        + hex(codePoint) + " to a surrogate");
            }
        }
    }

    private static void verifyFoldTable(FoldTable table, int[] foldTargets) {
        int pageCount = UNICODE_CODE_POINT_COUNT / PAGE_SIZE;
        if (table.pageIndexes().length != pageCount) {
            throw new IllegalStateException("FOLD has an invalid page count");
        }
        if (table.rows().isEmpty() || table.rows().size() > 1 << Byte.SIZE) {
            throw new IllegalStateException("FOLD has an invalid row count");
        }
        for (int entry : table.rows().get(0)) {
            if (entry != IDENTITY) {
                throw new IllegalStateException("FOLD row zero is not identity");
            }
        }
        for (int page = 0; page < table.pageIndexes().length; page++) {
            int row = table.pageIndexes()[page];
            if (row < 0 || row >= table.rows().size()) {
                throw new IllegalStateException("FOLD page " + page
                        + " has invalid row " + row);
            }
        }
        for (int codePoint = 0; codePoint < UNICODE_CODE_POINT_COUNT;
                codePoint++) {
            int fold = foldTargets[codePoint];
            int expected = fold == codePoint ? IDENTITY : fold;
            int actual = table.entry(codePoint);
            if (actual != expected) {
                throw new IllegalStateException("FOLD mismatch at U+"
                        + hex(codePoint) + ": expected 0x" + hex(expected)
                        + ", got 0x" + hex(actual));
            }
        }
    }

    private static String renderTemplate(String template, MappingTable lower,
                                         MappingTable upper, FoldTable fold) {
        int marker = template.indexOf(TABLES_MARKER);
        if (marker < 0 || marker != template.lastIndexOf(TABLES_MARKER)) {
            throw new IllegalArgumentException("Template must contain exactly one "
                    + TABLES_MARKER + " marker");
        }

        StringWriter tables = new StringWriter();
        PrintWriter out = new PrintWriter(tables);
        emitProtocolConstants(out);
        emitTable(out, Operation.LOWER, lower);
        emitTable(out, Operation.UPPER, upper);
        emitFoldTable(out, fold);
        out.flush();
        return template.replace(
                TABLES_MARKER, tables.toString().stripTrailing());
    }

    private static void emitProtocolConstants(PrintWriter out) {
        out.printf(Locale.ROOT,
                "static constexpr int string_case_page_shift = %d;%n",
                PAGE_SHIFT);
        out.printf(Locale.ROOT,
                "static constexpr int string_case_page_mask = 0x%02x;%n",
                PAGE_SIZE - 1);
        out.printf(Locale.ROOT,
                "static constexpr uint16_t string_case_identity_sentinel "
                + "= 0x%04x;%n", (int)IDENTITY);
        out.printf(Locale.ROOT,
                "static constexpr uint16_t string_case_fallback_sentinel "
                + "= 0x%04x;%n%n", (int)FALLBACK);
    }

    private static void emitTable(PrintWriter out, Operation operation,
                                  MappingTable table) {
        String baseName = "string_case_" + operation.tableName;
        out.printf(Locale.ROOT,
                "alignas(64) static constexpr uint8_t %s_page_index[%d] = {\n",
                baseName, PAGE_COUNT);
        for (int first = 0; first < PAGE_COUNT; first += 16) {
            int last = Math.min(first + 16, PAGE_COUNT);
            out.print("  ");
            for (int page = first; page < last; page++) {
                out.printf(Locale.ROOT, "0x%02x,", table.pageIndexes()[page]);
                if (page + 1 < last) {
                    out.print(" ");
                }
            }
            out.printf(Locale.ROOT, " // pages 0x%02x..0x%02x\n",
                    first, last - 1);
        }
        out.print("};\n\n");

        out.printf(Locale.ROOT,
                "alignas(64) static constexpr uint16_t %s_map[%d][%d] = {\n",
                baseName, table.rows().size(), PAGE_SIZE);
        for (int row = 0; row < table.rows().size(); row++) {
            out.printf(Locale.ROOT, "  { // row %d, used by %d page(s)\n",
                    row, pageUseCount(table, row));
            char[] entries = table.rows().get(row);
            for (int first = 0; first < PAGE_SIZE; first += 8) {
                int last = Math.min(first + 8, PAGE_SIZE);
                out.print("    ");
                for (int offset = first; offset < last; offset++) {
                    out.printf(Locale.ROOT, "0x%04x,", (int)entries[offset]);
                    if (offset + 1 < last) {
                        out.print(" ");
                    }
                }
                out.print('\n');
            }
            out.print("  },\n");
        }
        out.print("};\n\n");
    }

    private static void emitFoldTable(PrintWriter out, FoldTable table) {
        out.printf(Locale.ROOT,
                "alignas(64) static constexpr uint8_t "
                + "string_case_fold_page_index[%d] = {\n",
                table.pageIndexes().length);
        for (int first = 0; first < table.pageIndexes().length; first += 16) {
            int last = Math.min(first + 16, table.pageIndexes().length);
            out.print("  ");
            for (int page = first; page < last; page++) {
                out.printf(Locale.ROOT, "0x%02x,", table.pageIndexes()[page]);
                if (page + 1 < last) {
                    out.print(' ');
                }
            }
            out.printf(Locale.ROOT, " // pages 0x%04x..0x%04x\n",
                    first, last - 1);
        }
        out.print("};\n\n");

        out.printf(Locale.ROOT,
                "alignas(64) static constexpr uint32_t "
                + "string_case_fold_map[%d][%d] = {\n",
                table.rows().size(), PAGE_SIZE);
        for (int row = 0; row < table.rows().size(); row++) {
            out.printf(Locale.ROOT, "  { // row %d, used by %d page(s)\n",
                    row, foldPageUseCount(table, row));
            int[] entries = table.rows().get(row);
            for (int first = 0; first < PAGE_SIZE; first += 8) {
                int last = Math.min(first + 8, PAGE_SIZE);
                out.print("    ");
                for (int offset = first; offset < last; offset++) {
                    out.printf(Locale.ROOT, "0x%08x,", entries[offset]);
                    if (offset + 1 < last) {
                        out.print(' ');
                    }
                }
                out.print('\n');
            }
            out.print("  },\n");
        }
        out.print("};\n\n");
    }

    private static int pageUseCount(MappingTable table, int wantedRow) {
        int count = 0;
        for (int row : table.pageIndexes()) {
            if (row == wantedRow) {
                count++;
            }
        }
        return count;
    }

    private static int foldPageUseCount(FoldTable table, int wantedRow) {
        int count = 0;
        for (int row : table.pageIndexes()) {
            if (row == wantedRow) {
                count++;
            }
        }
        return count;
    }

    private static void writeAtomically(Path outputPath, String output)
            throws IOException {
        Path absoluteOutput = outputPath.toAbsolutePath();
        Path parent = absoluteOutput.getParent();
        Path fileName = absoluteOutput.getFileName();
        if (parent == null || fileName == null) {
            throw new IOException("Output path must name a file: " + outputPath);
        }

        Files.createDirectories(parent);
        String prefix = fileName.toString();
        if (prefix.length() < 3) {
            prefix = (prefix + "___").substring(0, 3);
        }
        Path temporary = Files.createTempFile(parent, prefix, ".tmp");
        try {
            Files.writeString(temporary, output, StandardCharsets.UTF_8);
            try {
                Files.move(temporary, absoluteOutput,
                        StandardCopyOption.ATOMIC_MOVE,
                        StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException exception) {
                throw new IOException("Atomic replacement is required but not "
                        + "supported for output " + absoluteOutput, exception);
            } catch (IOException exception) {
                throw new IOException("Failed to atomically replace output "
                        + absoluteOutput, exception);
            }
        } catch (IOException | RuntimeException | Error failure) {
            try {
                Files.deleteIfExists(temporary);
            } catch (IOException cleanupFailure) {
                failure.addSuppressed(cleanupFailure);
            }
            throw failure;
        }
        Files.deleteIfExists(temporary);
    }

    private static boolean isBmpNonSurrogate(int codePoint) {
        return codePoint >= 0 && codePoint < BMP_CODE_POINT_COUNT
                && !isSurrogate(codePoint);
    }

    private static boolean isSurrogate(int codePoint) {
        return codePoint >= MIN_SURROGATE && codePoint <= MAX_SURROGATE;
    }

    private static String normalizeLineEndings(String value) {
        return value.replace("\r\n", "\n").replace('\r', '\n');
    }

    private static String hex(int value) {
        return String.format(Locale.ROOT, "%04X", value);
    }
}
