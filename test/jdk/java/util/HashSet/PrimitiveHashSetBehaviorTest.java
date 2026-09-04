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
 */

/*
 * @test
 * @summary Primitive-specialized HashSet behavior through BytecodeEnhancement
 * @requires os.arch == "aarch64"
 * @requires vm.flagless
 * @modules java.base/java.util:open
 * @run main/othervm -XX:+UsePrimitiveHashSet -XX:+ExitOnBytecodeEnhancementFailure
 *      -Xlog:class+load+enhancement=info PrimitiveHashSetBehaviorTest
 */

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.Spliterator;
import java.util.stream.Collectors;

public class PrimitiveHashSetBehaviorTest {
    private static final int SPECIALIZED_SIZE = 1024;
    public static void main(String[] args) throws Exception {
        constructors();
        addContainsRemove();
        bulkOperations();
        iteratorBehavior();
        spliteratorBehavior();
        toArrayBehavior();
        cloneBehavior();
        hashCodeAndForEach();
        specializedForEachBehavior();
        specializedHashCode();
        serialization();
        fallbackToHashMap();
        primitiveCompatibility();
        backingOwnership();
        subtypeCompatibility();
        specializedBulkOperations();
        iteratorTransitions();
        specializedIteratorRemoval();
        randomizedDifferential();
        streamOperations();
        failedSpecializationFallsBack();
    }

    private static void constructors() {
        HashSet<Long> empty = new HashSet<>();
        assertTrue(empty.isEmpty());
        assertEquals(0, empty.size());

        HashSet<Long> withCapacity = new HashSet<>(64);
        assertTrue(withCapacity.isEmpty());

        HashSet<Long> withCapacityAndLoadFactor = new HashSet<>(64, 0.75f);
        assertTrue(withCapacityAndLoadFactor.isEmpty());

        HashSet<Long> fromCollection = new HashSet<>(Arrays.asList(1L, 2L, 3L));
        assertEquals(3, fromCollection.size());
        assertTrue(fromCollection.containsAll(Arrays.asList(1L, 2L, 3L)));

        HashSet<Long> fromEmptyCollection = new HashSet<>(Collections.emptyList());
        assertTrue(fromEmptyCollection.isEmpty());

        assertThrows(IllegalArgumentException.class, () -> new HashSet<Long>(-1));
        assertThrows(IllegalArgumentException.class, () -> new HashSet<Long>(-1, 0.75f));
        assertThrows(IllegalArgumentException.class, () -> HashSet.newHashSet(-1));
    }

    private static void addContainsRemove() {
        HashSet<Long> set = new HashSet<>();
        assertTrue(set.add(1L));
        assertFalse(set.add(1L));
        assertTrue(set.add(2L));
        assertTrue(set.add(Long.MAX_VALUE));
        assertTrue(set.add(Long.MIN_VALUE));
        assertEquals(4, set.size());

        assertTrue(set.contains(1L));
        assertTrue(set.contains(2L));
        assertTrue(set.contains(Long.MAX_VALUE));
        assertTrue(set.contains(Long.MIN_VALUE));
        assertFalse(set.contains(3L));
        assertFalse(set.contains("not a long"));

        assertTrue(set.remove(1L));
        assertFalse(set.remove(1L));
        assertFalse(set.remove("not a long"));
        assertEquals(3, set.size());

        set.clear();
        assertTrue(set.isEmpty());
    }

    private static void bulkOperations() {
        HashSet<Long> set = new HashSet<>();
        assertTrue(set.addAll(Arrays.asList(1L, 2L, 3L)));
        assertFalse(set.addAll(Arrays.asList(1L, 2L, 3L)));
        assertTrue(set.containsAll(Arrays.asList(1L, 2L)));
        assertFalse(set.containsAll(Arrays.asList(1L, 4L)));

        assertTrue(set.removeAll(Arrays.asList(1L, 3L)));
        assertEquals(1, set.size());
        assertTrue(set.contains(2L));

        set.addAll(Arrays.asList(3L, 4L, 5L));
        assertTrue(set.retainAll(Arrays.asList(2L, 5L)));
        assertEquals(2, set.size());
        assertTrue(set.containsAll(Arrays.asList(2L, 5L)));

        assertTrue(set.removeIf(v -> v > 2));
        assertEquals(1, set.size());
        assertTrue(set.contains(2L));
    }

    private static void iteratorBehavior() {
        HashSet<Long> set = specializedLongSet();
        List<Long> collected = new ArrayList<>();
        for (Long value : set) {
            collected.add(value);
        }
        assertEquals(SPECIALIZED_SIZE + 2, collected.size());

        Iterator<Long> it = set.iterator();
        it.next();
        it.remove();
        assertEquals(SPECIALIZED_SIZE + 1, set.size());
        assertThrows(IllegalStateException.class, it::remove);

        Iterator<Long> failFast = set.iterator();
        set.add(10_000L);
        assertTrue(failFast.hasNext());
        assertThrows(IllegalStateException.class, failFast::remove);
        assertThrows(ConcurrentModificationException.class, failFast::next);

        HashSet<Long> empty = new HashSet<>();
        assertFalse(empty.iterator().hasNext());
        assertThrows(NoSuchElementException.class, () -> empty.iterator().next());

        HashSet<Long> withNullAndZero = specializedLongSet();
        assertTrue(withNullAndZero.add(null));
        Iterator<Long> withNull = withNullAndZero.iterator();
        assertNull(withNull.next());
        withNull.remove();
        assertFalse(withNullAndZero.contains(null));
        assertTrue(withNullAndZero.contains(0L));
        assertEquals(SPECIALIZED_SIZE + 2, withNullAndZero.size());
    }

    private static void spliteratorBehavior() {
        HashSet<Long> set = new HashSet<>(Arrays.asList(1L, 2L, 3L));
        Spliterator<Long> sp = set.spliterator();
        assertTrue(sp.hasCharacteristics(Spliterator.SIZED));
        assertTrue(sp.hasCharacteristics(Spliterator.DISTINCT));
        assertEquals(3L, sp.estimateSize());

        List<Long> collected = new ArrayList<>();
        sp.forEachRemaining(collected::add);
        assertEquals(3, collected.size());
        assertTrue(collected.containsAll(Arrays.asList(1L, 2L, 3L)));

        HashSet<Long> large = new HashSet<>();
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            large.add(i);
        }
        large.add(null);
        Spliterator<Long> specialized = large.spliterator();
        assertEquals(SPECIALIZED_SIZE + 1L, specialized.estimateSize());
        Spliterator<Long> split = specialized.trySplit();
        assertNotNull(split);
        List<Long> specializedValues = new ArrayList<>();
        split.forEachRemaining(specializedValues::add);
        specialized.forEachRemaining(specializedValues::add);
        assertEquals(large.size(), specializedValues.size());
        assertEquals(large, new HashSet<>(specializedValues));
        assertTrue(specializedValues.contains(null));
        assertTrue(specializedValues.contains(0L));

        HashSet<Integer> integers = new HashSet<>();
        for (int i = 0; i < SPECIALIZED_SIZE; i++) {
            integers.add(i);
        }
        integers.add(null);
        Spliterator<Integer> intSpliterator = integers.spliterator();
        Spliterator<Integer> intSplit = intSpliterator.trySplit();
        assertNotNull(intSplit);
        List<Integer> intValues = new ArrayList<>();
        intSplit.forEachRemaining(intValues::add);
        intSpliterator.forEachRemaining(intValues::add);
        assertEquals(integers.size(), intValues.size());
        assertEquals(integers, new HashSet<>(intValues));

        Spliterator<Long> failFast = large.spliterator();
        assertTrue(failFast.tryAdvance(v -> { }));
        large.add(10_000L);
        assertThrows(ConcurrentModificationException.class,
                () -> failFast.tryAdvance(v -> { }));
        assertFalse(new HashSet<Long>().spliterator().tryAdvance(v -> fail("empty spliterator advanced")));
    }

    private static void toArrayBehavior() {
        HashSet<Long> set = new HashSet<>(Arrays.asList(1L, 2L, 3L));
        Object[] objects = set.toArray();
        assertEquals(3, objects.length);
        assertTrue(Arrays.asList(objects).containsAll(Arrays.asList(1L, 2L, 3L)));

        Long[] compact = set.toArray(new Long[0]);
        assertEquals(3, compact.length);
        assertTrue(Arrays.asList(compact).containsAll(Arrays.asList(1L, 2L, 3L)));

        Long[] oversized = new Long[5];
        Long[] result = set.toArray(oversized);
        assertSame(oversized, result);
        assertEquals(3, Arrays.stream(result).filter(Objects::nonNull).count());
        assertNull(result[3]);

        HashSet<Long> specialized = specializedLongSet();
        specialized.add(null);
        Object[] specializedObjects = specialized.toArray();
        assertEquals(specialized.size(), specializedObjects.length);
        assertEquals(specialized, new HashSet<>(Arrays.asList(specializedObjects)));

        Long[] specializedCompact = specialized.toArray(new Long[0]);
        assertEquals(specialized.size(), specializedCompact.length);
        assertEquals(specialized, new HashSet<>(Arrays.asList(specializedCompact)));

        Long[] specializedOversized = new Long[specialized.size() + 3];
        assertSame(specializedOversized, specialized.toArray(specializedOversized));
        assertNull(specializedOversized[specialized.size()]);
        assertEquals(specialized, new HashSet<>(Arrays.asList(specializedOversized)));
        assertThrows(ArrayStoreException.class, () -> specialized.toArray(new Integer[0]));
    }

    @SuppressWarnings("unchecked")
    private static void cloneBehavior() {
        HashSet<Long> set = specializedLongSet();
        HashSet<Long> clone = (HashSet<Long>) set.clone();
        assertEquals(set.size(), clone.size());
        assertTrue(clone.containsAll(set));
        clone.add(10_000L);
        assertFalse(set.contains(10_000L));
    }

    private static void hashCodeAndForEach() {
        HashSet<Long> set = new HashSet<>(Arrays.asList(1L, 2L));
        assertEquals(Long.hashCode(1L) + Long.hashCode(2L), set.hashCode());

        List<Long> collected = new ArrayList<>();
        set.forEach(collected::add);
        assertEquals(2, collected.size());
        assertTrue(collected.containsAll(Arrays.asList(1L, 2L)));
        assertThrows(NullPointerException.class, () -> set.forEach(null));
    }

    private static void specializedForEachBehavior() {
        HashSet<Long> longs = new HashSet<>();
        Set<Long> expectedLongs = new LinkedHashSet<>();
        HashSet<Integer> integers = new HashSet<>();
        Set<Integer> expectedIntegers = new LinkedHashSet<>();
        for (int i = 0; i < SPECIALIZED_SIZE; i++) {
            long longValue = i;
            longs.add(longValue);
            expectedLongs.add(longValue);
            integers.add(i);
            expectedIntegers.add(i);
        }
        longs.add(null);
        expectedLongs.add(null);
        integers.add(null);
        expectedIntegers.add(null);

        List<Long> collectedLongs = new ArrayList<>();
        longs.forEach(collectedLongs::add);
        assertEquals(expectedLongs.size(), collectedLongs.size());
        assertEquals(expectedLongs, new LinkedHashSet<>(collectedLongs));

        List<Integer> collectedIntegers = new ArrayList<>();
        integers.forEach(collectedIntegers::add);
        assertEquals(expectedIntegers.size(), collectedIntegers.size());
        assertEquals(expectedIntegers, new LinkedHashSet<>(collectedIntegers));

        boolean[] longModified = new boolean[1];
        assertThrows(ConcurrentModificationException.class, () ->
                longs.forEach(value -> {
                    if (!longModified[0]) {
                        longModified[0] = true;
                        longs.add((long) SPECIALIZED_SIZE);
                    }
                }));

        boolean[] integerModified = new boolean[1];
        assertThrows(ConcurrentModificationException.class, () ->
                integers.forEach(value -> {
                    if (!integerModified[0]) {
                        integerModified[0] = true;
                        integers.add(SPECIALIZED_SIZE);
                    }
                }));
    }

    private static void specializedHashCode() {
        HashSet<Long> longs = new HashSet<>();
        Set<Long> expectedLongs = new LinkedHashSet<>();
        HashSet<Integer> integers = new HashSet<>();
        Set<Integer> expectedIntegers = new LinkedHashSet<>();
        for (int i = -SPECIALIZED_SIZE; i < SPECIALIZED_SIZE; i++) {
            long longValue = (long) i * Integer.MAX_VALUE;
            longs.add(longValue);
            expectedLongs.add(longValue);
            integers.add(i);
            expectedIntegers.add(i);
        }
        longs.add(null);
        expectedLongs.add(null);
        integers.add(null);
        expectedIntegers.add(null);
        assertEquals(expectedLongs.hashCode(), longs.hashCode());
        assertEquals(expectedIntegers.hashCode(), integers.hashCode());
    }

    private static void serialization() throws Exception {
        HashSet<Long> set = specializedLongSet();
        set.add(null);
        HashSet<Long> deserialized = serialClone(set);
        assertEquals(set.size(), deserialized.size());
        assertTrue(deserialized.containsAll(set));
        deserialized.add(10_000L);
        assertFalse(set.contains(10_000L));

        HashSet<Long> customLoadFactor = new HashSet<>(16, 0.5f);
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            customLoadFactor.add(i);
        }
        byte[] bytes = serialize(customLoadFactor);
        try (RecordingObjectInputStream ois = new RecordingObjectInputStream(
                new ByteArrayInputStream(bytes))) {
            HashSet<?> restored = (HashSet<?>) ois.readObject();
            assertEquals(customLoadFactor, restored);
            assertEquals(0.5f, ois.hashSetLoadFactor);
        }
    }

    private static void fallbackToHashMap() {
        HashSet<Number> set = new HashSet<>();
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            assertTrue(set.add(i));
        }
        assertTrue(set.add(3.14d));
        assertEquals(SPECIALIZED_SIZE + 1, set.size());
        assertTrue(set.contains(1L));
        assertTrue(set.contains(2L));
        assertTrue(set.contains(3.14d));
        assertTrue(set.remove(1L));
        assertTrue(set.remove(3.14d));
        assertEquals(SPECIALIZED_SIZE - 1, set.size());
    }

    private static void primitiveCompatibility() throws Exception {
        HashSet<Number> longs = new HashSet<>();
        longs.add(null);
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            longs.add(i);
        }
        assertEquals(SPECIALIZED_SIZE + 1, longs.size());
        assertTrue(longs.contains(null));
        assertTrue(longs.contains(0L));
        assertFalse(longs.contains(0));
        assertFalse(longs.contains(1));
        assertFalse(longs.remove(1));
        assertTrue(longs.contains(1L));
        assertTrue(longs.remove(null));
        assertFalse(longs.contains(null));
        assertTrue(longs.contains(0L));
        assertTrue(longs.add(null));
        assertTrue(longs.remove(0L));
        assertTrue(longs.contains(null));
        assertFalse(longs.contains(0L));
        assertTrue(longs.add(0L));

        HashSet<Number> integers = new HashSet<>();
        integers.add(null);
        for (int i = 0; i < SPECIALIZED_SIZE; i++) {
            integers.add(i);
        }
        assertEquals(SPECIALIZED_SIZE + 1, integers.size());
        assertTrue(integers.contains(null));
        assertTrue(integers.contains(0));
        assertFalse(integers.contains(0L));
        assertFalse(integers.remove(1L));
        assertTrue(integers.remove(null));
        assertTrue(integers.contains(0));
        assertTrue(integers.add(null));

        HashSet<Number> expectedLongs = new HashSet<>();
        expectedLongs.addAll(longs);
        assertTrue(longs.equals(expectedLongs));
        assertTrue(expectedLongs.equals(longs));
        assertFalse(longs.equals(integers));
        assertFalse(integers.equals(longs));

        HashSet<Number> restored = serialClone(longs);
        assertTrue(restored.contains(null));
        assertTrue(restored.contains(0L));
        assertFalse(restored.contains(0));
    }

    private static void backingOwnership() throws Exception {
        Field mapField = HashSet.class.getDeclaredField("map");
        Field primitiveSetField = HashSet.class.getDeclaredField("primitiveHashSet");
        mapField.setAccessible(true);
        primitiveSetField.setAccessible(true);

        HashSet<Long> ordinary = new HashSet<>();
        ordinary.add(1L);
        assertNotNull(mapField.get(ordinary));
        assertNull(primitiveSetField.get(ordinary));

        HashSet<Number> specialized = new HashSet<>();
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            specialized.add(i);
        }
        assertNull(mapField.get(specialized));
        Object backing = primitiveSetField.get(specialized);
        assertNotNull(backing);
        assertTrue(backing.getClass().getSimpleName().equals("LongHashSet"));
        specialized.add(null);
        assertNull(mapField.get(specialized));
        assertSame(backing, primitiveSetField.get(specialized));

        specialized.add(3.14d);
        assertNotNull(mapField.get(specialized));
        assertNull(primitiveSetField.get(specialized));
        assertTrue(specialized.contains(null));
        assertTrue(specialized.contains(0L));
        assertTrue(specialized.contains(3.14d));

        HashSet<Integer> integers = new HashSet<>();
        for (int i = 0; i < SPECIALIZED_SIZE; i++) {
            integers.add(i);
        }
        assertTrue(primitiveSetField.get(integers).getClass().getSimpleName().equals("IntHashSet"));

        HashSet<Long> cleared = specializedLongSet();
        Object clearedBacking = primitiveSetField.get(cleared);
        cleared.clear();
        assertTrue(cleared.isEmpty());
        assertNull(mapField.get(cleared));
        assertSame(clearedBacking, primitiveSetField.get(cleared));
        assertTrue(cleared.add(42L));
        assertEquals(Set.of(42L), cleared);
    }

    private static void subtypeCompatibility() throws Exception {
        Field mapField = HashSet.class.getDeclaredField("map");
        Field primitiveSetField = HashSet.class.getDeclaredField("primitiveHashSet");
        mapField.setAccessible(true);
        primitiveSetField.setAccessible(true);

        LinkedHashSet<Long> linked = new LinkedHashSet<>();
        DerivedHashSet<Long> derived = new DerivedHashSet<>();
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            linked.add(i);
            derived.add(i);
        }
        assertNotNull(mapField.get(linked));
        assertNull(primitiveSetField.get(linked));
        assertNotNull(mapField.get(derived));
        assertNull(primitiveSetField.get(derived));
        assertEquals(Long.valueOf(0L), linked.iterator().next());
    }

    private static void specializedBulkOperations() {
        HashSet<Number> set = new HashSet<>();
        set.add(null);
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            set.add(i);
        }
        assertTrue(set.containsAll(Arrays.asList(null, 0L, 1L)));
        assertFalse(set.containsAll(Arrays.asList(null, 0, 1L)));
        assertTrue(set.removeAll(Arrays.asList(null, 0, 1L, 2L)));
        assertFalse(set.contains(null));
        assertTrue(set.contains(0L));
        assertFalse(set.contains(1L));
        assertTrue(set.retainAll(Arrays.asList(0L, 3L, 4L)));
        assertEquals(Set.of(0L, 3L, 4L), set);

        set = new HashSet<>();
        set.add(null);
        for (int i = 0; i < SPECIALIZED_SIZE; i++) {
            set.add(i);
        }
        assertTrue(set.removeIf(v -> v == null || v.intValue() % 2 == 0));
        assertFalse(set.contains(null));
        assertFalse(set.contains(0));
        assertTrue(set.contains(1));
        assertEquals(SPECIALIZED_SIZE / 2, set.size());
    }

    private static void iteratorTransitions() {
        HashSet<Number> set = new HashSet<>();
        set.add(null);
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            set.add(i);
        }

        Iterator<Number> beforeRollback = set.iterator();
        set.add(3.14d);
        assertTrue(beforeRollback.hasNext());
        assertThrows(ConcurrentModificationException.class, beforeRollback::next);

        HashSet<Number> values = new HashSet<>();
        values.add(null);
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            values.add(i);
        }
        Iterator<Number> iterator = values.iterator();
        assertNull(iterator.next());
        iterator.remove();
        while (iterator.hasNext()) {
            Number value = iterator.next();
            if (Long.valueOf(0L).equals(value)) {
                iterator.remove();
                break;
            }
        }
        assertFalse(values.contains(null));
        assertFalse(values.contains(0L));
        assertEquals(SPECIALIZED_SIZE - 1, values.size());
    }

    private static void specializedIteratorRemoval() {
        HashSet<Long> actual = specializedLongSet();
        Set<Long> expected = new LinkedHashSet<>(actual);
        Iterator<Long> iterator = actual.iterator();
        while (iterator.hasNext()) {
            Long value = iterator.next();
            if ((value.longValue() & 3L) == 0L) {
                iterator.remove();
                expected.remove(value);
            }
        }
        assertEquals(expected, actual);
        assertEquals(expected.hashCode(), actual.hashCode());
    }

    private static void streamOperations() {
        HashSet<Long> set = specializedLongSet();
        List<Long> filtered = set.stream().filter(v -> v >= SPECIALIZED_SIZE - 3L)
                .collect(Collectors.toList());
        assertEquals(4, filtered.size());
        assertTrue(filtered.containsAll(Arrays.asList(
                (long) SPECIALIZED_SIZE - 3L,
                (long) SPECIALIZED_SIZE - 2L,
                (long) SPECIALIZED_SIZE - 1L,
                Long.MAX_VALUE)));
        assertEquals(set.size(), set.parallelStream().count());
        assertEquals(set, set.parallelStream().collect(Collectors.toSet()));
    }

    private static void failedSpecializationFallsBack() throws Exception {
        failedSpecializationFallsBack(true);
        failedSpecializationFallsBack(false);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static void failedSpecializationFallsBack(boolean useLongs) throws Exception {
        Field mapField = HashSet.class.getDeclaredField("map");
        Field primitiveSetField = HashSet.class.getDeclaredField("primitiveHashSet");
        Field candidateStateField = HashSet.class.getDeclaredField("primitiveCandidateState");
        mapField.setAccessible(true);
        primitiveSetField.setAccessible(true);
        candidateStateField.setAccessible(true);

        HashSet<Number> set = new HashSet<>();
        FailingIterationHashMap<Number, Object> map = new FailingIterationHashMap<>();
        for (int i = 0; i < 100; i++) {
            map.put(useLongs ? Long.valueOf(i) : Integer.valueOf(i), new Object());
        }
        mapField.set(set, map);
        candidateStateField.setInt(set, useLongs ? 1 : 2);

        Number added = useLongs ? Long.valueOf(100) : Integer.valueOf(100);
        assertTrue(set.add(added));
        assertSame(map, mapField.get(set));
        assertNull(primitiveSetField.get(set));
        assertEquals(-1, candidateStateField.getInt(set));
        assertTrue(set.contains(added));

        Number next = useLongs ? Long.valueOf(101) : Integer.valueOf(101);
        assertTrue(set.add(next));
        assertSame(map, mapField.get(set));
        assertNull(primitiveSetField.get(set));
        assertTrue(set.contains(next));
    }

    private static final class FailingIterationHashMap<K, V> extends HashMap<K, V> {
        @Override
        public Set<K> keySet() {
            Set<K> keys = super.keySet();
            return new java.util.AbstractSet<>() {
                @Override
                public Iterator<K> iterator() {
                    throw new ConcurrentModificationException();
                }

                @Override
                public int size() {
                    return keys.size();
                }
            };
        }
    }

    private static void randomizedDifferential() {
        runRandomizedDifferential(true);
        runRandomizedDifferential(false);
    }

    private static void runRandomizedDifferential(boolean useLongs) {
        HashSet<Number> actual = new HashSet<>();
        Set<Number> expected = new LinkedHashSet<>();
        actual.add(null);
        expected.add(null);
        for (int i = 0; i < SPECIALIZED_SIZE + 20; i++) {
            Number value = useLongs ? Long.valueOf(i - 60L) : Integer.valueOf(i - 60);
            actual.add(value);
            expected.add(value);
        }

        Random random = new Random(useLongs ? 0x5eedL : 0x1eedL);
        for (int i = 0; i < 5000; i++) {
            int raw = random.nextInt(257) - 128;
            Number compatible = useLongs ? Long.valueOf(raw) : Integer.valueOf(raw);
            Object probe;
            switch (random.nextInt(12)) {
                case 0 -> probe = null;
                case 1 -> probe = useLongs ? Integer.valueOf(raw) : Long.valueOf(raw);
                case 2 -> probe = Double.valueOf(raw);
                default -> probe = compatible;
            }
            switch (random.nextInt(10)) {
                case 0, 1, 2 -> assertEquals(expected.add((Number) probe), actual.add((Number) probe));
                case 3, 4, 5 -> assertEquals(expected.remove(probe), actual.remove(probe));
                case 6, 7, 8 -> assertEquals(expected.contains(probe), actual.contains(probe));
                default -> {
                    if ((i & 255) == 0) {
                        actual.clear();
                        expected.clear();
                    }
                }
            }
            assertEquals(expected.size(), actual.size());
            assertEquals(expected.hashCode(), actual.hashCode());
            assertTrue(actual.equals(expected));
            assertTrue(expected.equals(actual));
        }
    }

    private static HashSet<Long> specializedLongSet() {
        HashSet<Long> set = new HashSet<>();
        for (long i = 0; i < SPECIALIZED_SIZE; i++) {
            set.add(i);
        }
        set.add(Long.MIN_VALUE);
        set.add(Long.MAX_VALUE);
        return set;
    }

    @SuppressWarnings("unchecked")
    private static <T> T serialClone(T object) throws Exception {
        try (ObjectInputStream ois = new ObjectInputStream(
                new ByteArrayInputStream(serialize(object)))) {
            return (T) ois.readObject();
        }
    }

    private static byte[] serialize(Object object) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(bos)) {
            oos.writeObject(object);
        }
        return bos.toByteArray();
    }

    private static final class RecordingObjectInputStream extends ObjectInputStream {
        float hashSetLoadFactor = Float.NaN;

        RecordingObjectInputStream(ByteArrayInputStream input) throws IOException {
            super(input);
        }

        @Override
        public float readFloat() throws IOException {
            float value = super.readFloat();
            hashSetLoadFactor = value;
            return value;
        }
    }

    private static final class DerivedHashSet<E> extends HashSet<E> {
    }

    private static void assertTrue(boolean actual) {
        if (!actual) {
            fail("expected true");
        }
    }

    private static void assertFalse(boolean actual) {
        if (actual) {
            fail("expected false");
        }
    }

    private static void assertNull(Object actual) {
        if (actual != null) {
            fail("expected null, got " + actual);
        }
    }

    private static void assertNotNull(Object actual) {
        if (actual == null) {
            fail("expected non-null");
        }
    }

    private static void assertSame(Object expected, Object actual) {
        if (expected != actual) {
            fail("expected same object");
        }
    }

    private static void assertEquals(long expected, long actual) {
        if (expected != actual) {
            fail("expected " + expected + ", got " + actual);
        }
    }

    private static void assertEquals(float expected, float actual) {
        if (Float.compare(expected, actual) != 0) {
            fail("expected " + expected + " but got " + actual);
        }
    }

    private static void assertEquals(Object expected, Object actual) {
        if (!Objects.equals(expected, actual)) {
            fail("expected " + expected + ", got " + actual);
        }
    }

    private static <T extends Throwable> void assertThrows(Class<T> expected, ThrowingRunnable action) {
        try {
            action.run();
        } catch (Throwable actual) {
            if (expected.isInstance(actual)) {
                return;
            }
            throw new AssertionError("expected " + expected.getName() + ", got " + actual, actual);
        }
        throw new AssertionError("expected " + expected.getName());
    }

    private static void fail(String message) {
        throw new AssertionError(message);
    }

    @FunctionalInterface
    private interface ThrowingRunnable {
        void run() throws Throwable;
    }
}
