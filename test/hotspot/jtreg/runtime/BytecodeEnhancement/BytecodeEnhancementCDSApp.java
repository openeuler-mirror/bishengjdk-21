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
 *
 */

import jdk.test.whitebox.WhiteBox;

class BytecodeEnhancementCDSApp {
    public static void main(String[] args) throws Exception {
        String mode = args[0];
        boolean enhanced = mode.equals("enhanced") ||
                          mode.equals("sharing-off") ||
                          mode.equals("dynamic-dump-enhanced") ||
                          mode.equals("partial-enhanced");
        boolean sharingOff = mode.equals("sharing-off");
        boolean dumpRun = mode.startsWith("dynamic-dump-");
        boolean partialTop = mode.startsWith("partial-");

        BytecodeEnhancementCDSOuter outer = new BytecodeEnhancementCDSOuter();
        expect(enhanced ? "OUTER-NEW" : "OUTER-OLD", outer.marker(), "outer marker");
        expect("nest-secret", outer.readThroughInner(), "real nestmate access");
        expect(enhanced ? "OUTER-NEW" : "OUTER-OLD", BytecodeEnhancementCDSClient.call(), "shared client call");
        expect(enhanced ? "OUTER-NEW" : "OUTER-OLD", new BytecodeEnhancementCDSSub().marker(), "subclass call");
        expect(enhanced ? "IFACE-NEW" : "IFACE-OLD", new BytecodeEnhancementCDSImpl().marker(), "interface call");
        Class<?> addedClass = loadAddedClass(enhanced);

        WhiteBox wb = WhiteBox.getWhiteBox();
        if (dumpRun) {
            // Dynamic dump runs exercise the classes but cannot assert sharing
            // until the resulting top archive is loaded.
        } else if (sharingOff) {
            expectShared(wb, BytecodeEnhancementCDSOuter.class, false);
            expectShared(wb, BytecodeEnhancementCDSOuter.Inner.class, false);
            expectShared(wb, BytecodeEnhancementCDSClient.class, false);
            expectShared(wb, BytecodeEnhancementCDSSub.class, false);
            expectShared(wb, BytecodeEnhancementCDSInterface.class, false);
            expectShared(wb, BytecodeEnhancementCDSImpl.class, false);
        } else if (enhanced || partialTop) {
            expectShared(wb, BytecodeEnhancementCDSApp.class, true);
            expectShared(wb, BytecodeEnhancementCDSOuter.class, false);
            expectShared(wb, BytecodeEnhancementCDSOuter.Inner.class, true);
            expectShared(wb, BytecodeEnhancementCDSClient.class, true);
            expectShared(wb, BytecodeEnhancementCDSSub.class, false);
            expectShared(wb, BytecodeEnhancementCDSInterface.class, false);
            expectShared(wb, BytecodeEnhancementCDSImpl.class, false);
        } else {
            expectShared(wb, BytecodeEnhancementCDSApp.class, true);
            expectShared(wb, BytecodeEnhancementCDSOuter.class, true);
            expectShared(wb, BytecodeEnhancementCDSOuter.Inner.class, true);
            expectShared(wb, BytecodeEnhancementCDSClient.class, true);
            expectShared(wb, BytecodeEnhancementCDSSub.class, true);
            expectShared(wb, BytecodeEnhancementCDSInterface.class, true);
            expectShared(wb, BytecodeEnhancementCDSImpl.class, true);
        }
        if (!dumpRun && addedClass != null) {
            expectShared(wb, addedClass, false);
        }

        System.out.println("CDS-COMPAT:" + mode + ":PASS");
    }

    private static Class<?> loadAddedClass(boolean expected) throws Exception {
        try {
            Class<?> klass = Class.forName("BytecodeEnhancementCDSAdded");
            if (!expected) {
                throw new RuntimeException("unexpectedly loaded BytecodeEnhancementCDSAdded");
            }
            expect("ADDED", (String) klass.getMethod("marker").invoke(null), "added class marker");
            return klass;
        } catch (ClassNotFoundException expectedException) {
            if (expected) {
                throw expectedException;
            }
            return null;
        }
    }

    private static void expect(String expected, String actual, String what) {
        if (!expected.equals(actual)) {
            throw new RuntimeException(what + ": expected=" + expected + ", actual=" + actual);
        }
    }

    private static void expectShared(WhiteBox wb, Class<?> klass, boolean expected) {
        boolean actual = wb.isSharedClass(klass);
        if (actual != expected) {
            throw new RuntimeException(klass.getName() + " shared=" + actual + ", expected=" + expected);
        }
        System.out.println("CDS-COMPAT:shared:" + klass.getName() + "=" + actual);
    }
}

class BytecodeEnhancementCDSOuter {
    private String secret = "nest-secret";

    public String marker() {
        return "OUTER-OLD";
    }

    public String readThroughInner() {
        return new Inner().read(this);
    }

    public static class Inner {
        public String read(BytecodeEnhancementCDSOuter outer) {
            return outer.secret;
        }
    }
}

class BytecodeEnhancementCDSClient {
    public static String call() {
        return new BytecodeEnhancementCDSOuter().marker();
    }
}

class BytecodeEnhancementCDSSub extends BytecodeEnhancementCDSOuter {
}

interface BytecodeEnhancementCDSInterface {
    default String marker() {
        return "IFACE-OLD";
    }
}

class BytecodeEnhancementCDSImpl implements BytecodeEnhancementCDSInterface {
}

class BytecodeEnhancementCDSAdded {
    public static String marker() {
        return "ADDED";
    }
}
