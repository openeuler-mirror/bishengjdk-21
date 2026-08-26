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

/* @test
 * @summary Verifies the Java protocol for String equalsIgnoreCase intrinsics
 * @modules java.base/java.lang:open
 * @run main/othervm -XX:+CompactStrings EqualsIgnoreCaseIntrinsicProtocol
 */

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;

public class EqualsIgnoreCaseIntrinsicProtocol {
    private static final int CI_MATCH = -1;
    private static final int CI_MISMATCH = -2;
    private static final int[] LENGTHS = { 0, 1, 15, 16, 17, 33 };
    private static final String INTRINSIC_CANDIDATE =
            "jdk.internal.vm.annotation.IntrinsicCandidate";

    private static final Field STRING_VALUE;
    private static final Field STRING_CODER;

    static {
        try {
            STRING_VALUE = String.class.getDeclaredField("value");
            STRING_VALUE.setAccessible(true);
            STRING_CODER = String.class.getDeclaredField("coder");
            STRING_CODER.setAccessible(true);
        } catch (ReflectiveOperationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static void main(String[] args) throws Exception {
        Method latin1 = requireHelper("java.lang.StringLatin1",
                "regionMatchesCIResult");
        Method latin1Utf16 = requireHelper("java.lang.StringLatin1",
                "regionMatchesCI_UTF16Result");
        Method utf16 = requireHelper("java.lang.StringUTF16",
                "regionMatchesCIResult");

        for (int len : LENGTHS) {
            checkLatin1(latin1, len);
            checkLatin1Utf16(latin1Utf16, len);
            checkUtf16(utf16, len);
        }
    }

    private static Method requireHelper(String holderName, String methodName)
            throws Exception {
        Class<?> holder = Class.forName(holderName);
        Method method = holder.getDeclaredMethod(methodName,
                byte[].class, int.class, byte[].class, int.class, int.class);

        if (method.getReturnType() != int.class) {
            throw new AssertionError(method + " must return int");
        }
        int modifiers = method.getModifiers();
        if (!Modifier.isPrivate(modifiers) || !Modifier.isStatic(modifiers)) {
            throw new AssertionError(method + " must be private static");
        }
        boolean isIntrinsicCandidate = Arrays.stream(method.getDeclaredAnnotations())
                .map(Annotation::annotationType)
                .map(Class::getName)
                .anyMatch(INTRINSIC_CANDIDATE::equals);
        if (!isIntrinsicCandidate) {
            throw new AssertionError(method + " must be @IntrinsicCandidate");
        }
        method.setAccessible(true);
        return method;
    }

    private static void checkLatin1(Method method, int len) throws Exception {
        int toffset = 2;
        int ooffset = 3;
        byte[] value = latin1Bytes(len + toffset + 2, toffset, false);
        byte[] other = latin1Bytes(len + ooffset + 2, ooffset, true);
        assertResult(method, value, toffset, other, ooffset, len, CI_MATCH);

        if (len != 0) {
            other[ooffset + len - 1] = (byte) '#';
            assertResult(method, value, toffset, other, ooffset, len, CI_MISMATCH);
        }
    }

    private static void checkLatin1Utf16(Method method, int len) throws Exception {
        int toffset = 2;
        int ooffset = 3;
        byte[] value = latin1Bytes(len + toffset + 2, toffset, false);
        char[] other = utf16Chars(len + ooffset + 2, ooffset, true, false);
        assertResult(method, value, toffset, utf16Bytes(other), ooffset, len, CI_MATCH);

        if (len != 0) {
            other[ooffset + len - 1] = '#';
            assertResult(method, value, toffset, utf16Bytes(other), ooffset, len,
                    CI_MISMATCH);
        }
    }

    private static void checkUtf16(Method method, int len) throws Exception {
        int toffset = 2;
        int ooffset = 3;
        char[] value = utf16Chars(len + toffset + 2, toffset, false, true);
        char[] other = utf16Chars(len + ooffset + 2, ooffset, true, true);
        assertResult(method, utf16Bytes(value), toffset, utf16Bytes(other),
                ooffset, len, CI_MATCH);

        if (len != 0) {
            other[ooffset + len - 1] = '\u2603';
            assertResult(method, utf16Bytes(value), toffset, utf16Bytes(other),
                    ooffset, len, CI_MISMATCH);
        }
    }

    private static byte[] latin1Bytes(int size, int offset, boolean lowerCase) {
        byte[] value = new byte[size];
        Arrays.fill(value, (byte) '!');
        for (int i = offset; i < size - 2; i++) {
            value[i] = (byte) ((lowerCase ? 'a' : 'A') + (i - offset) % 26);
        }
        return value;
    }

    private static char[] utf16Chars(int size, int offset, boolean lowerCase,
                                     boolean nonLatinRegion) {
        char[] value = new char[size];
        Arrays.fill(value, '!');
        value[0] = '\u0100'; // Force UTF-16 storage with CompactStrings enabled.
        for (int i = offset; i < size - 2; i++) {
            if (nonLatinRegion) {
                value[i] = (char) ((lowerCase ? '\uFF41' : '\uFF21')
                        + (i - offset) % 26);
            } else {
                value[i] = (char) ((lowerCase ? 'a' : 'A') + (i - offset) % 26);
            }
        }
        return value;
    }

    private static byte[] utf16Bytes(char[] chars) throws IllegalAccessException {
        String string = new String(chars);
        byte coder = (byte) STRING_CODER.get(string);
        if (coder != 1) {
            throw new AssertionError("test input was not stored as UTF-16");
        }
        return (byte[]) STRING_VALUE.get(string);
    }

    private static void assertResult(Method method, byte[] value, int toffset,
                                     byte[] other, int ooffset, int len,
                                     int expected) throws Exception {
        int actual = (int) method.invoke(null, value, toffset, other, ooffset, len);
        if (actual != expected) {
            throw new AssertionError(method + " returned " + actual
                    + ", expected " + expected + " for offsets " + toffset
                    + "/" + ooffset + " and length " + len);
        }
    }
}
