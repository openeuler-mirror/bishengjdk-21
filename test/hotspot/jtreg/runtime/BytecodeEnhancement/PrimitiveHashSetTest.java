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

/*
 * @test
 * @summary Test the JDK internal HashSet classdata replacement
 * @requires os.arch == "aarch64"
 * @requires vm.flagless
 * @library /test/lib
 * @run driver PrimitiveHashSetTest
 */

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashSet;

import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;

public class PrimitiveHashSetTest {
    private static final String NUM_HASH_SET_FIELD = "NumHashSet";
    private static final String NUM_HASH_SET_CLASS = "java.util.HashSet$NumHashSet";

    public static void main(String[] args) throws Exception {
        if (args.length != 0) {
            boolean expected = Boolean.parseBoolean(args[0]);
            Field field = null;
            try {
                field = HashSet.class.getDeclaredField(NUM_HASH_SET_FIELD);
            } catch (NoSuchFieldException e) {
                // Expected when the replacement is disabled.
            }
            boolean present = field != null;
            if (present != expected) {
                throw new RuntimeException("HashSet NumHashSet field present=" + present
                                           + ", expected=" + expected);
            }
            Class<?> numHashSetClass = null;
            try {
                numHashSetClass = Class.forName(NUM_HASH_SET_CLASS, false, null);
            } catch (ClassNotFoundException e) {
                if (expected) {
                    throw e;
                }
            }
            if ((numHashSetClass != null) != expected) {
                throw new RuntimeException("HashSet.NumHashSet class present="
                                           + (numHashSetClass != null) + ", expected=" + expected);
            }
            if (expected) {
                int modifiers = numHashSetClass.getModifiers();
                if (!Modifier.isPrivate(modifiers) || !Modifier.isStatic(modifiers)) {
                    throw new RuntimeException("HashSet.NumHashSet must be private static");
                }
                if (numHashSetClass.getClassLoader() != null) {
                    throw new RuntimeException("HashSet.NumHashSet must be defined by the boot loader");
                }
                if (field.getType() != int.class) {
                    throw new RuntimeException("NumHashSet field has unexpected type " + field.getType());
                }
            }
            if (!Modifier.isPublic(HashSet.class.getModifiers())) {
                throw new RuntimeException("Replacement HashSet must remain public");
            }
            HashSet<String> set = new HashSet<>();
            if (!set.add("enhanced") || set.add("enhanced") || !set.contains("enhanced")
                    || set.size() != 1 || !set.remove("enhanced") || !set.isEmpty()) {
                throw new RuntimeException("HashSet does not preserve basic Set semantics after replacement");
            }
            return;
        }

        run(false, "-XX:-UsePrimitiveHashSet");
        run(true, "-XX:+UsePrimitiveHashSet");
        // If this run fails with a replacement CRC mismatch:
        // 1. Check whether the original HashSet has been updated.
        // 2. Adapt the replacement HashSet to the updated implementation.
        // 3. Update the expected CRC in BytecodeEnhancement.
        // 4. Rerun the functional and performance tests.
        run(true, "-XX:+UsePrimitiveHashSet", "-XX:+ExitOnBytecodeEnhancementFailure");
    }

    private static void run(boolean expected, String... flags) throws Exception {
        String[] args = new String[flags.length + 2];
        System.arraycopy(flags, 0, args, 0, flags.length);
        args[flags.length] = PrimitiveHashSetTest.class.getName();
        args[flags.length + 1] = Boolean.toString(expected);
        ProcessBuilder builder = ProcessTools.createLimitedTestJavaProcessBuilder(
                args);
        OutputAnalyzer output = ProcessTools.executeProcess(builder);
        output.shouldHaveExitValue(0);
    }
}
