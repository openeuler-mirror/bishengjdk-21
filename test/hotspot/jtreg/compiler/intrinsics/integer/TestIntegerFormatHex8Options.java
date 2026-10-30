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
 * @summary Check the HiSilicon hex intrinsic flags and fallback controls
 * @requires os.arch == "aarch64" & vm.compiler2.enabled & vm.flagless
 * @library /test/lib
 * @modules java.base/java.lang:open jdk.management
 * @build TestIntegerFormatHex8 jdk.test.whitebox.WhiteBox
 * @run driver jdk.test.lib.helpers.ClassFileInstaller jdk.test.whitebox.WhiteBox
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options false
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch -XX:-UseIntegerToHexStringIntrinsic --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options false
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch -XX:+UseIntegerToHexStringIntrinsic --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options true
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch -XX:+UseHisiOptimizations --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options true
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch -XX:+UseHisiOptimizations -XX:-UseIntegerToHexStringIntrinsic --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options false
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:-TieredCompilation -Xbatch -XX:+UseIntegerToHexStringIntrinsic -XX:DisableIntrinsic=_formatHex8 --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options true
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -XX:TieredStopAtLevel=1 -Xbatch -XX:+UseIntegerToHexStringIntrinsic --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options true
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -Xint -XX:+UseIntegerToHexStringIntrinsic --add-opens=java.base/java.lang=ALL-UNNAMED TestIntegerFormatHex8Options true
 */

import com.sun.management.HotSpotDiagnosticMXBean;
import java.lang.management.ManagementFactory;

import jdk.test.whitebox.WhiteBox;

public class TestIntegerFormatHex8Options {
    public static void main(String[] args) throws Throwable {
        boolean expected = Boolean.parseBoolean(args[0])
                && WhiteBox.getWhiteBox().getCPUFeatures().startsWith("0x48:");
        HotSpotDiagnosticMXBean bean = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean.class);
        boolean actual = Boolean.parseBoolean(bean.getVMOption("UseIntegerToHexStringIntrinsic").getValue());
        if (actual != expected) {
            throw new AssertionError("UseIntegerToHexStringIntrinsic=" + actual + "; expected " + expected);
        }
        TestIntegerFormatHex8.main(new String[0]);
    }
}
