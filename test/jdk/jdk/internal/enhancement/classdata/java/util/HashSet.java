/*
 * Copyright (c) 1997, 2023, Oracle and/or its affiliates. All rights reserved.
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

package java.util;

import java.io.InvalidObjectException;
import java.util.function.Consumer;

import jdk.internal.access.SharedSecrets;

/**
 * This class implements the {@code Set} interface, backed by a hash table
 * (actually a {@code HashMap} instance).  It makes no guarantees as to the
 * iteration order of the set; in particular, it does not guarantee that the
 * order will remain constant over time.  This class permits the {@code null}
 * element.
 *
 * <p>This class offers constant time performance for the basic operations
 * ({@code add}, {@code remove}, {@code contains} and {@code size}),
 * assuming the hash function disperses the elements properly among the
 * buckets.  Iterating over this set requires time proportional to the sum of
 * the {@code HashSet} instance's size (the number of elements) plus the
 * "capacity" of the backing {@code HashMap} instance (the number of
 * buckets).  Thus, it's very important not to set the initial capacity too
 * high (or the load factor too low) if iteration performance is important.
 *
 * <p><strong>Note that this implementation is not synchronized.</strong>
 * If multiple threads access a hash set concurrently, and at least one of
 * the threads modifies the set, it <i>must</i> be synchronized externally.
 * This is typically accomplished by synchronizing on some object that
 * naturally encapsulates the set.
 *
 * If no such object exists, the set should be "wrapped" using the
 * {@link Collections#synchronizedSet Collections.synchronizedSet}
 * method.  This is best done at creation time, to prevent accidental
 * unsynchronized access to the set:<pre>
 *   Set s = Collections.synchronizedSet(new HashSet(...));</pre>
 *
 * <p>The iterators returned by this class's {@code iterator} method are
 * <i>fail-fast</i>: if the set is modified at any time after the iterator is
 * created, in any way except through the iterator's own {@code remove}
 * method, the Iterator throws a {@link ConcurrentModificationException}.
 * Thus, in the face of concurrent modification, the iterator fails quickly
 * and cleanly, rather than risking arbitrary, non-deterministic behavior at
 * an undetermined time in the future.
 *
 * <p>Note that the fail-fast behavior of an iterator cannot be guaranteed
 * as it is, generally speaking, impossible to make any hard guarantees in the
 * presence of unsynchronized concurrent modification.  Fail-fast iterators
 * throw {@code ConcurrentModificationException} on a best-effort basis.
 * Therefore, it would be wrong to write a program that depended on this
 * exception for its correctness: <i>the fail-fast behavior of iterators
 * should be used only to detect bugs.</i>
 *
 * <p>This class is a member of the
 * <a href="{@docRoot}/java.base/java/util/package-summary.html#CollectionsFramework">
 * Java Collections Framework</a>.
 *
 * @param <E> the type of elements maintained by this set
 *
 * @author  Josh Bloch
 * @author  Neal Gafter
 * @see     Collection
 * @see     Set
 * @see     TreeSet
 * @see     HashMap
 * @since   1.2
 */

public class HashSet<E>
    extends AbstractSet<E>
    implements Set<E>, Cloneable, java.io.Serializable
{
    @java.io.Serial
    static final long serialVersionUID = -5024744406713321676L;

    transient HashMap<E,Object> map;

    // Dummy value to associate with an Object in the backing Map
    static final Object PRESENT = new Object();

    private static final int PRIMITIVE_SPECIALIZATION_THRESHOLD = 100;
    private static final int NO_PRIMITIVE_CANDIDATE = 0;
    private static final int LONG_PRIMITIVE_CANDIDATE = 1;
    private static final int INT_PRIMITIVE_CANDIDATE = 2;
    private static final int PRIMITIVE_SPECIALIZATION_DISABLED = -1;

    transient PrimitiveHashSet primitiveHashSet;

    // Records the only primitive key type observed so far. null does not
    // select a type; mixing key types disables specialization.
    transient int primitiveCandidateState;

    /**
     * Constructs a new, empty set; the backing {@code HashMap} instance has
     * default initial capacity (16) and load factor (0.75).
     */
    public HashSet() {
        map = new HashMap<>();
    }

    /**
     * Constructs a new set containing the elements in the specified
     * collection.  The {@code HashMap} is created with default load factor
     * (0.75) and an initial capacity sufficient to contain the elements in
     * the specified collection.
     *
     * @param c the collection whose elements are to be placed into this set
     * @throws NullPointerException if the specified collection is null
     */
    public HashSet(Collection<? extends E> c) {
        map = HashMap.newHashMap(Math.max(c.size(), 12));
        addAll(c);
    }

    /**
     * Constructs a new, empty set; the backing {@code HashMap} instance has
     * the specified initial capacity and the specified load factor.
     *
     * @apiNote
     * To create a {@code HashSet} with an initial capacity that accommodates
     * an expected number of elements, use {@link #newHashSet(int) newHashSet}.
     *
     * @param      initialCapacity   the initial capacity of the hash map
     * @param      loadFactor        the load factor of the hash map
     * @throws     IllegalArgumentException if the initial capacity is less
     *             than zero, or if the load factor is nonpositive
     */
    public HashSet(int initialCapacity, float loadFactor) {
        map = new HashMap<>(initialCapacity, loadFactor);
    }

    /**
     * Constructs a new, empty set; the backing {@code HashMap} instance has
     * the specified initial capacity and default load factor (0.75).
     *
     * @apiNote
     * To create a {@code HashSet} with an initial capacity that accommodates
     * an expected number of elements, use {@link #newHashSet(int) newHashSet}.
     *
     * @param      initialCapacity   the initial capacity of the hash table
     * @throws     IllegalArgumentException if the initial capacity is less
     *             than zero
     */
    public HashSet(int initialCapacity) {
        map = new HashMap<>(initialCapacity);
    }

    /**
     * Constructs a new, empty linked hash set.  (This package private
     * constructor is only used by LinkedHashSet.) The backing
     * HashMap instance is a LinkedHashMap with the specified initial
     * capacity and the specified load factor.
     *
     * @param      initialCapacity   the initial capacity of the hash map
     * @param      loadFactor        the load factor of the hash map
     * @param      dummy             ignored (distinguishes this
     *             constructor from other int, float constructor.)
     * @throws     IllegalArgumentException if the initial capacity is less
     *             than zero, or if the load factor is nonpositive
     */
    HashSet(int initialCapacity, float loadFactor, boolean dummy) {
        map = new LinkedHashMap<>(initialCapacity, loadFactor);
    }

    /**
     * Returns an iterator over the elements in this set.  The elements
     * are returned in no particular order.
     *
     * @return an Iterator over the elements in this set
     * @see ConcurrentModificationException
     */
    @SuppressWarnings("unchecked")
    public Iterator<E> iterator() {
        if (primitiveHashSet != null) {
            return (Iterator<E>) (Iterator<?>) primitiveHashSet.iterator();
        }
        return map.keySet().iterator();
    }

    /**
     * Returns the number of elements in this set (its cardinality).
     *
     * @return the number of elements in this set (its cardinality)
     */
    public int size() {
        if (primitiveHashSet != null) {
            return primitiveHashSet.size();
        }
        return map.size();
    }

    /**
     * Returns {@code true} if this set contains no elements.
     *
     * @return {@code true} if this set contains no elements
     */
    public boolean isEmpty() {
        if (primitiveHashSet != null) {
            return primitiveHashSet.isEmpty();
        }
        return map.isEmpty();
    }

    /**
     * Returns {@code true} if this set contains the specified element.
     * More formally, returns {@code true} if and only if this set
     * contains an element {@code e} such that
     * {@code Objects.equals(o, e)}.
     *
     * @param o element whose presence in this set is to be tested
     * @return {@code true} if this set contains the specified element
     */
    public boolean contains(Object o) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.contains(o);
        }
        return map.containsKey(o);
    }

    /**
     * Adds the specified element to this set if it is not already present.
     * More formally, adds the specified element {@code e} to this set if
     * this set contains no element {@code e2} such that
     * {@code Objects.equals(e, e2)}.
     * If this set already contains the element, the call leaves the set
     * unchanged and returns {@code false}.
     *
     * @param e element to be added to this set
     * @return {@code true} if this set did not already contain the specified
     * element
     */
    public boolean add(E e) {
        PrimitiveHashSet specializedSet = primitiveHashSet;
        HashMap<E, Object> currentMap = map;
        if (specializedSet != null) {
            if (e == null) {
                return specializedSet.addNull();
            }
            if (specializedSet instanceof LongHashSet longHashSet) {
                if (e instanceof Long key) {
                    return longHashSet.addLong(key.longValue());
                }
            } else if (e instanceof Integer key) {
                return ((IntHashSet) specializedSet).addInt(key.intValue());
            }
            HashMap<E, Object> rollbackMap = rollbackToHashMap(specializedSet);
            return rollbackMap.put(e, PRESENT) == null;
        } else if (currentMap != null) {
            return addToMap(e, currentMap);
        }
        return false;
    }

    private boolean addToMap(E e, HashMap<E, Object> currentMap) {
        boolean result = currentMap.put(e, PRESENT) == null;
        if (!result) {
            return false;
        }
        if (primitiveCandidateState == PRIMITIVE_SPECIALIZATION_DISABLED) {
            return true;
        }
        // LinkedHashSet must retain LinkedHashMap ordering and cannot switch
        // to the unordered primitive HashSet representation.
        if (currentMap instanceof LinkedHashMap || getClass() != HashSet.class) {
            primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
            return true;
        }
        // null is supported by either specialized representation, but it
        // must not select the primitive type by itself.
        if (e == null) {
            return true;
        }
        if (e instanceof Long
                && (primitiveCandidateState == NO_PRIMITIVE_CANDIDATE
                    || primitiveCandidateState == LONG_PRIMITIVE_CANDIDATE)) {
            primitiveCandidateState = LONG_PRIMITIVE_CANDIDATE;
            if (currentMap.size() >= PRIMITIVE_SPECIALIZATION_THRESHOLD) {
                switchToLongHashSet(currentMap);
            }
        } else if (e instanceof Integer
                && (primitiveCandidateState == NO_PRIMITIVE_CANDIDATE
                    || primitiveCandidateState == INT_PRIMITIVE_CANDIDATE)) {
            primitiveCandidateState = INT_PRIMITIVE_CANDIDATE;
            if (currentMap.size() >= PRIMITIVE_SPECIALIZATION_THRESHOLD) {
                switchToIntHashSet(currentMap);
            }
        } else {
            primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
        }
        return true;
    }

    /**
     * Removes the specified element from this set if it is present.
     * More formally, removes an element {@code e} such that
     * {@code Objects.equals(o, e)},
     * if this set contains such an element.  Returns {@code true} if
     * this set contained the element (or equivalently, if this set
     * changed as a result of the call).  (This set will not contain the
     * element once the call returns.)
     *
     * @param o object to be removed from this set, if present
     * @return {@code true} if the set contained the specified element
     */
    public boolean remove(Object o) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.remove(o);
        }
        return map.remove(o)==PRESENT;
    }

    /**
     * Removes all of the elements from this set.
     * The set will be empty after this call returns.
     */
    public void clear() {
        if (primitiveHashSet != null) {
            primitiveHashSet.clear();
            return;
        }
        map.clear();
    }

    /**
     * Returns a shallow copy of this {@code HashSet} instance: the elements
     * themselves are not cloned.
     *
     * @return a shallow copy of this set
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        try {
            HashSet<E> newSet = (HashSet<E>) super.clone();
            if (primitiveHashSet != null) {
                newSet.primitiveHashSet = primitiveHashSet.clone();
            } else {
                newSet.map = (HashMap<E, Object>) map.clone();
            }
            return newSet;
        } catch (CloneNotSupportedException e) {
            throw new InternalError(e);
        }
    }

    /**
     * Save the state of this {@code HashSet} instance to a stream (that is,
     * serialize it).
     *
     * @serialData The capacity of the backing {@code HashMap} instance
     *             (int), and its load factor (float) are emitted, followed by
     *             the size of the set (the number of elements it contains)
     *             (int), followed by all of its elements (each an Object) in
     *             no particular order.
     */
    @java.io.Serial
    private void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {
        // Write out any hidden serialization magic
        s.defaultWriteObject();
        if (primitiveHashSet != null) {
            // The serialized fields describe the HashMap that readObject will
            // reconstruct, not the open-addressed primitive table.
            float loadFactor = primitiveHashSet.hashMapLoadFactor;
            int capacity = HashMap.tableSizeFor((int) Math.min(
                    Math.ceil(size() / (double) loadFactor),
                    HashMap.MAXIMUM_CAPACITY));
            s.writeInt(capacity);
            s.writeFloat(loadFactor);
            s.writeInt(size());
            for (E e : this)
                s.writeObject(e);
            return;
        }

        // Write out HashMap capacity and load factor
        s.writeInt(map.capacity());
        s.writeFloat(map.loadFactor());

        // Write out size
        s.writeInt(map.size());

        // Write out all elements in the proper order.
        for (E e : map.keySet())
            s.writeObject(e);
    }

    /**
     * Reconstitute the {@code HashSet} instance from a stream (that is,
     * deserialize it).
     */
    @java.io.Serial
    private void readObject(java.io.ObjectInputStream s)
        throws java.io.IOException, ClassNotFoundException {
        // Consume and ignore stream fields (currently zero).
        s.readFields();

        // Read capacity and verify non-negative.
        int capacity = s.readInt();
        if (capacity < 0) {
            throw new InvalidObjectException("Illegal capacity: " +
                                             capacity);
        }

        // Read load factor and verify positive and non NaN.
        float loadFactor = s.readFloat();
        if (loadFactor <= 0 || Float.isNaN(loadFactor)) {
            throw new InvalidObjectException("Illegal load factor: " +
                                             loadFactor);
        }
        // Clamp load factor to range of 0.25...4.0.
        loadFactor = Math.clamp(loadFactor, 0.25f, 4.0f);

        // Read size and verify non-negative.
        int size = s.readInt();
        if (size < 0) {
            throw new InvalidObjectException("Illegal size: " + size);
        }

        // Set the capacity according to the size and load factor ensuring that
        // the HashMap is at least 25% full but clamping to maximum capacity.
        capacity = (int) Math.min(size * Math.min(1 / loadFactor, 4.0f),
                HashMap.MAXIMUM_CAPACITY);

        // Constructing the backing map will lazily create an array when the first element is
        // added, so check it before construction. Call HashMap.tableSizeFor to compute the
        // actual allocation size. Check Map.Entry[].class since it's the nearest public type to
        // what is actually created.
        SharedSecrets.getJavaObjectInputStreamAccess()
                     .checkArray(s, Map.Entry[].class, HashMap.tableSizeFor(capacity));

        // Create backing HashMap
        map = (this instanceof LinkedHashSet ?
               new LinkedHashMap<>(capacity, loadFactor) :
               new HashMap<>(capacity, loadFactor));

        // Read in all elements in the proper order.
        for (int i=0; i<size; i++) {
            @SuppressWarnings("unchecked")
                E e = (E) s.readObject();
            map.put(e, PRESENT);
        }
        primitiveHashSet = null;
        primitiveCandidateState = 0;
    }

    /**
     * Creates a <em><a href="Spliterator.html#binding">late-binding</a></em>
     * and <em>fail-fast</em> {@link Spliterator} over the elements in this
     * set.
     *
     * <p>The {@code Spliterator} reports {@link Spliterator#SIZED} and
     * {@link Spliterator#DISTINCT}.  Overriding implementations should document
     * the reporting of additional characteristic values.
     *
     * @return a {@code Spliterator} over the elements in this set
     * @since 1.8
     */
    @SuppressWarnings("unchecked")
    public Spliterator<E> spliterator() {
        if (primitiveHashSet != null) {
            return (Spliterator<E>) (Spliterator<?>) primitiveHashSet.spliterator();
        }
        return new HashMap.KeySpliterator<>(map, 0, -1, 0, 0);
    }

    @Override
    public Object[] toArray() {
        if (primitiveHashSet != null) {
            return primitiveHashSet.toArray();
        }
        return map.keysToArray(new Object[map.size()]);
    }

    @Override
    public <T> T[] toArray(T[] a) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.toArray(a);
        }
        return map.keysToArray(map.prepareArray(a));
    }

    /**
     * Creates a new, empty HashSet suitable for the expected number of elements.
     * The returned set uses the default load factor of 0.75, and its initial capacity is
     * generally large enough so that the expected number of elements can be added
     * without resizing the set.
     *
     * @param numElements    the expected number of elements
     * @param <T>         the type of elements maintained by the new set
     * @return the newly created set
     * @throws IllegalArgumentException if numElements is negative
     * @since 19
     */
    public static <T> HashSet<T> newHashSet(int numElements) {
        if (numElements < 0) {
            throw new IllegalArgumentException("Negative number of elements: " + numElements);
        }
        return new HashSet<>(HashMap.calculateHashMapCapacity(numElements));
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean addAll(Collection<? extends E> c) {
        if (primitiveHashSet == null) {
            return super.addAll(c);
        }
        boolean modified = false;
        for (E e : c) {
            modified |= add(e);
        }
        return modified;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.removeAll(c);
        }
        return super.removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.retainAll(c);
        }
        return super.retainAll(c);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        if (primitiveHashSet != null) {
            return primitiveHashSet.containsAll(c);
        }
        return super.containsAll(c);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void forEach(Consumer<? super E> action) {
        if (primitiveHashSet != null) {
            Objects.requireNonNull(action);
            primitiveHashSet.forEach((Consumer<? super Number>) action);
            return;
        }
        super.forEach(action);
    }

    @Override
    public int hashCode() {
        if (primitiveHashSet != null) {
            return primitiveHashSet.hashCode();
        }
        return super.hashCode();
    }

    private void switchToLongHashSet(HashMap<E, Object> sourceMap) {
        if (primitiveHashSet != null) {
            return;
        }
        try {
            int expectedModCount = sourceMap.modCount;
            LongHashSet newLongHashSet = new LongHashSet();
            newLongHashSet.hashMapLoadFactor = sourceMap.loadFactor();
            for (E key : sourceMap.keySet()) {
                if (key == null) {
                    newLongHashSet.addNull();
                } else if (key.getClass() == Long.class) {
                    newLongHashSet.addLong(((Long) key).longValue());
                } else {
                    primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
                    return;
                }
            }
            if (sourceMap.modCount != expectedModCount || map != sourceMap
                    || primitiveHashSet != null) {
                disablePrimitiveSpecialization(sourceMap);
                return;
            }
            // Publish only after the temporary set has been fully populated.
            primitiveHashSet = newLongHashSet;
            primitiveCandidateState = NO_PRIMITIVE_CANDIDATE;
            map = null;
        } catch (RuntimeException e) {
            disablePrimitiveSpecialization(sourceMap);
        }
    }

    private void switchToIntHashSet(HashMap<E, Object> sourceMap) {
        if (primitiveHashSet != null) {
            return;
        }
        try {
            int expectedModCount = sourceMap.modCount;
            IntHashSet newIntHashSet = new IntHashSet();
            newIntHashSet.hashMapLoadFactor = sourceMap.loadFactor();
            for (E key : sourceMap.keySet()) {
                if (key == null) {
                    newIntHashSet.addNull();
                } else if (key.getClass() == Integer.class) {
                    newIntHashSet.addInt(((Integer) key).intValue());
                } else {
                    primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
                    return;
                }
            }
            if (sourceMap.modCount != expectedModCount || map != sourceMap
                    || primitiveHashSet != null) {
                disablePrimitiveSpecialization(sourceMap);
                return;
            }
            // Publish only after the temporary set has been fully populated.
            primitiveHashSet = newIntHashSet;
            primitiveCandidateState = NO_PRIMITIVE_CANDIDATE;
            map = null;
        } catch (RuntimeException e) {
            disablePrimitiveSpecialization(sourceMap);
        }
    }

    private void disablePrimitiveSpecialization(HashMap<E, Object> sourceMap) {
        if (map == sourceMap && primitiveHashSet == null) {
            primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
        }
    }

    private HashMap<E, Object> rollbackToHashMap(PrimitiveHashSet specializedSet) {
        float loadFactor = specializedSet.hashMapLoadFactor;
        int capacity = (int) Math.min(
                Math.ceil(specializedSet.size() / (double) loadFactor),
                HashMap.MAXIMUM_CAPACITY);
        HashMap<E, Object> newMap = new HashMap<>(capacity, loadFactor);
        for (Number key : specializedSet) {
            @SuppressWarnings("unchecked")
            E element = (E) key;
            newMap.put(element, PRESENT);
        }
        specializedSet.invalidateIterators();
        this.map = newMap;
        this.primitiveHashSet = null;
        this.primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
        return newMap;
    }

}
