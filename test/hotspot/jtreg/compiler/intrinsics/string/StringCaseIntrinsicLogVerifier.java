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

package compiler.intrinsics.string;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class StringCaseIntrinsicLogVerifier {
    private static final Pattern TASK_METHOD = Pattern.compile("method='([^']+)'");
    private static final Pattern INTRINSIC_ID = Pattern.compile("<intrinsic id='([^']+)'");

    private StringCaseIntrinsicLogVerifier() {}

    static void verify(Path logFile, String holderClass,
                       Map<String, String> wrapperIntrinsics,
                       boolean expectIntrinsics) throws IOException {
        Set<String> stringCaseIntrinsics = Set.copyOf(wrapperIntrinsics.values());
        Map<String, Set<String>> taskIntrinsics = new HashMap<>();
        Set<String> seenWrappers = new HashSet<>();
        String currentWrapper = null;

        for (String line : Files.readAllLines(logFile)) {
            String trimmed = line.trim();
            if (trimmed.startsWith("<task ")) {
                currentWrapper = wrapperFromTask(
                        trimmed, holderClass, wrapperIntrinsics.keySet());
                if (currentWrapper != null) {
                    seenWrappers.add(currentWrapper);
                    taskIntrinsics.computeIfAbsent(currentWrapper,
                            unused -> new HashSet<>());
                }
            }

            if (currentWrapper != null) {
                Matcher intrinsicMatcher = INTRINSIC_ID.matcher(trimmed);
                while (intrinsicMatcher.find()) {
                    taskIntrinsics.get(currentWrapper).add(intrinsicMatcher.group(1));
                }
            }

            if (trimmed.equals("</task>")) {
                currentWrapper = null;
            }
        }

        for (Map.Entry<String, String> entry : wrapperIntrinsics.entrySet()) {
            String wrapper = entry.getKey();
            String expectedIntrinsic = entry.getValue();
            if (!seenWrappers.contains(wrapper)) {
                throw new AssertionError("No compilation task for "
                        + holderClass + "::" + wrapper + " in " + logFile);
            }

            Set<String> found = taskIntrinsics.get(wrapper);
            if (expectIntrinsics) {
                if (!found.contains(expectedIntrinsic)) {
                    throw new AssertionError("Intrinsic " + expectedIntrinsic
                            + " was not used by " + holderClass + "::" + wrapper
                            + " in " + logFile + "; found " + found);
                }
            } else if (!disjoint(found, stringCaseIntrinsics)) {
                throw new AssertionError("String case intrinsic was unexpectedly used by "
                        + holderClass + "::" + wrapper + " in " + logFile
                        + "; found " + found);
            }
        }
    }

    private static String wrapperFromTask(String task, String holderClass,
                                          Set<String> wrappers) {
        Matcher methodMatcher = TASK_METHOD.matcher(task);
        if (!methodMatcher.find()) {
            return null;
        }

        String method = methodMatcher.group(1).replace('/', '.');
        String prefix = holderClass + " ";
        if (!method.startsWith(prefix)) {
            return null;
        }

        int nameStart = prefix.length();
        int nameEnd = method.indexOf(' ', nameStart);
        if (nameEnd < 0) {
            return null;
        }
        String methodName = method.substring(nameStart, nameEnd);
        return wrappers.contains(methodName) ? methodName : null;
    }

    private static boolean disjoint(Set<String> left, Set<String> right) {
        for (String value : left) {
            if (right.contains(value)) {
                return false;
            }
        }
        return true;
    }
}
