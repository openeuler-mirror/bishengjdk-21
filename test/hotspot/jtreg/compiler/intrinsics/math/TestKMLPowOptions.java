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
 * @summary Verify KMLLibraryPath diagnostics when UseKMLPow is disabled
 * @library /test/lib
 * @requires os.family == "linux" & os.arch == "aarch64" & vm.flavor != "zero" & vm.flagless
 * @run driver compiler.intrinsics.math.TestKMLPowOptions
 */

package compiler.intrinsics.math;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class TestKMLPowOptions {
    private static final String KML_LIBRARY_PATH_IGNORED =
            "-XX:KMLLibraryPath is ignored because -XX:+UseKMLPow is not enabled";
    private static final String UNUSED_KML_LIBRARY_PATH =
            "/path/that/does/not/need/to/exist";

    public static void main(String[] args) throws Exception {
        OutputAnalyzer defaultDisabled = ProcessTools.executeTestJava(
                "-XX:KMLLibraryPath=" + UNUSED_KML_LIBRARY_PATH,
                "-Xlog:library=info",
                "-version");
        assertPathIgnored(defaultDisabled);

        OutputAnalyzer explicitlyDisabled = ProcessTools.executeTestJava(
                "-XX:-UseKMLPow",
                "-XX:KMLLibraryPath=" + UNUSED_KML_LIBRARY_PATH,
                "-Xlog:library=info",
                "-version");
        assertPathIgnored(explicitlyDisabled);

        OutputAnalyzer loggingOnly = ProcessTools.executeTestJava(
                "-XX:-UseKMLPow",
                "-Xlog:library=info",
                "-version");
        loggingOnly.shouldHaveExitValue(0);
        loggingOnly.shouldNotContain(KML_LIBRARY_PATH_IGNORED);

        OutputAnalyzer loggingDisabled = ProcessTools.executeTestJava(
                "-XX:-UseKMLPow",
                "-XX:KMLLibraryPath=" + UNUSED_KML_LIBRARY_PATH,
                "-version");
        loggingDisabled.shouldHaveExitValue(0);
        loggingDisabled.shouldNotContain(KML_LIBRARY_PATH_IGNORED);
    }

    private static void assertPathIgnored(OutputAnalyzer output) {
        output.shouldHaveExitValue(0);
        output.shouldContain(KML_LIBRARY_PATH_IGNORED);
        output.shouldNotContain("Loaded KML library");
    }
}
