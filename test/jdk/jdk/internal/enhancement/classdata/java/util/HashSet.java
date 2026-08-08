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
            rollbackToHashMap();
            return this.map.put(e, PRESENT) == null;
        }

        return addToMap(e);
    }

    private boolean addToMap(E e) {
        boolean result = map.put(e, PRESENT) == null;
        if (!result) {
            return false;
        }
        if (primitiveCandidateState == PRIMITIVE_SPECIALIZATION_DISABLED) {
            return true;
        }
        // LinkedHashSet must retain LinkedHashMap ordering and cannot switch
        // to the unordered primitive HashSet representation.
        if (map instanceof LinkedHashMap || getClass() != HashSet.class) {
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
            if (map.size() >= PRIMITIVE_SPECIALIZATION_THRESHOLD) {
                switchToLongHashSet();
            }
        } else if (e instanceof Integer
                && (primitiveCandidateState == NO_PRIMITIVE_CANDIDATE
                    || primitiveCandidateState == INT_PRIMITIVE_CANDIDATE)) {
            primitiveCandidateState = INT_PRIMITIVE_CANDIDATE;
            if (map.size() >= PRIMITIVE_SPECIALIZATION_THRESHOLD) {
                switchToIntHashSet();
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

    private void switchToLongHashSet() {
        if (primitiveHashSet != null) {
            return;
        }
        LongHashSet newLongHashSet = new LongHashSet();
        newLongHashSet.hashMapLoadFactor = this.map.loadFactor();
        for (E key : this.map.keySet()) {
            if (key == null) {
                newLongHashSet.addNull();
            } else if (key.getClass() == Long.class) {
                newLongHashSet.addLong(((Long) key).longValue());
            } else {
                this.primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
                return;
            }
        }
        this.primitiveHashSet = newLongHashSet;
        this.primitiveCandidateState = 0;
        this.map = null;
    }

    private void switchToIntHashSet() {
        if (primitiveHashSet != null) {
            return;
        }
        IntHashSet newIntHashSet = new IntHashSet();
        newIntHashSet.hashMapLoadFactor = this.map.loadFactor();
        for (E key : this.map.keySet()) {
            if (key == null) {
                newIntHashSet.addNull();
            } else if (key.getClass() == Integer.class) {
                newIntHashSet.addInt(((Integer) key).intValue());
            } else {
                this.primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
                return;
            }
        }
        this.primitiveHashSet = newIntHashSet;
        this.primitiveCandidateState = 0;
        this.map = null;
    }

    private void rollbackToHashMap() {
        if (this.primitiveHashSet == null) {
            return;
        }
        float loadFactor = this.primitiveHashSet.hashMapLoadFactor;
        int capacity = (int) Math.min(
                Math.ceil(size() / (double) loadFactor),
                HashMap.MAXIMUM_CAPACITY);
        HashMap<E, Object> newMap = new HashMap<>(capacity, loadFactor);
        for (Number key : this.primitiveHashSet) {
            @SuppressWarnings("unchecked")
            E element = (E) key;
            newMap.put(element, PRESENT);
        }
        this.primitiveHashSet.invalidateIterators();
        this.map = newMap;
        this.primitiveHashSet = null;
        this.primitiveCandidateState = PRIMITIVE_SPECIALIZATION_DISABLED;
    }

    private static abstract class PrimitiveHashSet extends AbstractSet<Number> implements Cloneable {
        int mask;

        boolean containsNull;

        boolean containsZero;

        int capacity;

        int threshold;

        int minCapacity;

        int size;

        int modCount;

        float loadFactor;

        // Retained only to emit the standard HashSet serialization metadata.
        // It is intentionally independent of this table's occupancy factor.
        float hashMapLoadFactor = HashMap.DEFAULT_LOAD_FACTOR;

        static final long LONG_PHI = 0x9E3779B97F4A7C15L;

        static final int DEFAULT_INITIAL_CAPACITY = 16;

        static final float DEFAULT_LOAD_FACTOR = .75f;

        public PrimitiveHashSet(final int initialCapacity, final float loadFactor) {
            if (loadFactor <= 0 || Float.isNaN(loadFactor) || loadFactor >= 1) {
                throw new IllegalArgumentException("Load factor must be between 0 and 1");
            }
            if (initialCapacity < 0) {
                throw new IllegalArgumentException("Initial capacity must be non-negative");
            }
            this.loadFactor = loadFactor;
            this.minCapacity = this.capacity = PrimitiveHashSet.leastArraySize(initialCapacity, loadFactor);
            this.mask = this.capacity - 1;
            this.threshold = PrimitiveHashSet.maxEntriesFill(this.capacity, loadFactor);
        }

        protected int realSize() {
            return this.size - (this.containsZero ? 1 : 0) - (this.containsNull ? 1 : 0);
        }

        protected static long mixBits(final long n) {
            long res = n * LONG_PHI;
            res ^= res >>> 32;
            return res ^ (res >>> 16);
        }

        protected static int maxEntriesFill(int capacity, float loadFactor) {
            return Math.min((int) Math.ceil(capacity * loadFactor), capacity - 1);
        }

        protected static int leastArraySize(int expectedElements, float loadFactor) {
            final long size = Math.max(2, nextPowerOfTwo((long) Math.ceil(expectedElements / loadFactor)));
            if (size > (1 << 30)) {
                throw new IllegalArgumentException("Expected number of elements is too large: " + expectedElements
                        + " with load factor " + loadFactor);
            }
            return (int) size;
        }

        protected static long nextPowerOfTwo(long n) {
            if (n == 0) {
                return 1;
            }
            n--;
            n |= n >> 1;
            n |= n >> 2;
            n |= n >> 4;
            n |= n >> 8;
            n |= n >> 16;
            return (n | n >> 32) + 1;
        }

        @Override
        public int size() {
            return this.size;
        }

        public boolean isEmpty() {
            return this.size == 0;
        }


        abstract Iterator<? extends Number> valueIterator();

        abstract void forEachElement(Consumer<? super Number> action);

        final boolean addNull() {
            if (containsNull) {
                return false;
            }
            containsNull = true;
            size++;
            modCount++;
            return true;
        }

        final boolean removeNull() {
            if (!containsNull) {
                return false;
            }
            containsNull = false;
            size--;
            modCount++;
            return true;
        }

        final void invalidateIterators() {
            modCount++;
        }

        @Override
        public final Iterator<Number> iterator() {
            return new Iterator<>() {
                private final Iterator<? extends Number> values = valueIterator();
                private int expectedModCount = modCount;
                private boolean nullPending = containsNull;
                private boolean canRemove;
                private boolean lastWasNull;

                private void checkForComodification() {
                    if (expectedModCount != modCount) {
                        throw new ConcurrentModificationException();
                    }
                }

                @Override
                public boolean hasNext() {
                    checkForComodification();
                    return nullPending || values.hasNext();
                }

                @Override
                public Number next() {
                    checkForComodification();
                    if (nullPending) {
                        nullPending = false;
                        canRemove = true;
                        lastWasNull = true;
                        return null;
                    }
                    Number value = values.next();
                    canRemove = true;
                    lastWasNull = false;
                    return value;
                }

                @Override
                public void remove() {
                    checkForComodification();
                    if (!canRemove) {
                        throw new IllegalStateException();
                    }
                    if (lastWasNull) {
                        removeNull();
                    } else {
                        values.remove();
                    }
                    expectedModCount = modCount;
                    canRemove = false;
                }
            };
        }

        @Override
        public final void forEach(Consumer<? super Number> action) {
            Objects.requireNonNull(action);
            int expectedModCount = modCount;
            if (containsNull) {
                action.accept(null);
            }
            forEachElement(action);
            if (expectedModCount != modCount) {
                throw new ConcurrentModificationException();
            }
        }

        @Override
        public abstract boolean contains(Object e);

        @Override
        public abstract boolean remove(Object e);

        public PrimitiveHashSet clone() {
            try {
                PrimitiveHashSet cloned = (PrimitiveHashSet) super.clone();
                return cloned;
            } catch (CloneNotSupportedException e) {
                throw new InternalError(e);
            }
        }
    }

    private static class LongHashSet extends PrimitiveHashSet {
        long[] keys;
        public LongHashSet() {
            this(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR);
        }

        public LongHashSet(final int initialCapacity, final float loadFactor) {
            super(initialCapacity, loadFactor);
            this.keys = new long[this.capacity + 1];
        }

        boolean addLong(long key) {
            int pos;
            if (key == 0L) {
                if (this.containsZero) {
                    return false;
                }
                this.containsZero = true;
            } else {
                long current;
                final long[] curKeys = this.keys;
                if (!((current = curKeys[pos = (int) LongHashSet.mixBits(key) & this.mask]) == 0)) {
                    if (current == key) {
                        return false;
                    }
                    while (!((current = curKeys[pos = (pos + 1) & this.mask]) == 0)) {
                        if (current == key) {
                            return false;
                        }
                    }
                }
                curKeys[pos] = key;
            }
            if (this.size++ >= this.threshold) {
                rehash(LongHashSet.leastArraySize(this.size + 1, this.loadFactor));
            }
            this.modCount++;
            return true;
        }

        @Override
        public boolean remove(Object key) {
            if (key == null) {
                return removeNull();
            }
            return key instanceof Long && removeLong((Long) key);
        }

        boolean removeLong(long key) {
            if (key == 0L) {
                if (this.containsZero) {
                    return removeNullEntry();
                }
                return false;
            }
            long current;
            final long[] curKeys = this.keys;
            int pos;
            if (((current = curKeys[pos = (int) LongHashSet.mixBits(key) & this.mask]) == 0)) {
                return false;
            }
            if (current == key) {
                return removeEntry(pos);
            }
            while (true) {
                if (((current = curKeys[pos = (pos + 1) & this.mask]) == 0)) {
                    return false;
                }
                if (current == key) {
                    return removeEntry(pos);
                }
            }
        }

        @Override
        public boolean contains(Object key) {
            if (key == null) {
                return this.containsNull;
            }
            return key instanceof Long && containsLong((Long) key);
        }

        boolean containsLong(long key) {
            if (key == 0L) return this.containsZero;
            long current;
            final long[] curKeys = this.keys;
            int pos = (int) LongHashSet.mixBits(key) & this.mask;
            if ((current = curKeys[pos]) == 0) return false;
            if (current == key) return true;
            while ((current = curKeys[pos = (pos + 1) & this.mask]) != 0) {
                if (current == key) return true;
            }
            return false;
        }


        Iterator<Long> valueIterator() {
            return new LongIterator();
        }

        @Override
        public Spliterator<Number> spliterator() {
            return new LongSpliterator(0, -1, 0, 0, false, false, false);
        }

        @Override
        void forEachElement(Consumer<? super Number> action) {
            if (containsZero) {
                action.accept(Long.valueOf(0));
            }
            final long[] curKeys = keys;
            for (int i = capacity; i-- != 0;) {
                long key = curKeys[i];
                if (key != 0) {
                    action.accept(Long.valueOf(key));
                }
            }
        }

        public void clear() {
            if (this.size == 0) {
                return;
            }
            this.size = 0;
            this.containsZero = false;
            this.containsNull = false;
            Arrays.fill(this.keys, 0);
            this.modCount++;
        }

        @Override
        public LongHashSet clone() {
            LongHashSet newSet = (LongHashSet) super.clone();
            newSet.keys = this.keys.clone();
            return newSet;
        }

        @Override
        public int hashCode() {
            int h = 0;
            for (int j = realSize(), i = 0; j-- != 0;) {
                while (this.keys[i] == 0) {
                    i++;
                }
                h += (int) (this.keys[i] ^ (this.keys[i] >>> 32));
                i++;
            }
            return h;
        }

        private boolean removeEntry(int pos) {
            this.size--;
            shiftKeys(pos);
            if (this.capacity > this.minCapacity && this.size < this.threshold >> 2
                    && this.capacity > DEFAULT_INITIAL_CAPACITY) {
                rehash(this.capacity >> 1);
            }
            this.modCount++;
            return true;
        }

        private void shiftKeys(int pos) {
            int last, slot;
            long current;
            final long[] curKeys = this.keys;
            for (;;) {
                pos = ((last = pos) + 1) & this.mask;
                for (;;) {
                    if (((current = curKeys[pos]) == 0)) {
                        curKeys[last] = 0;
                        return;
                    }
                    slot = (int) LongHashSet.mixBits(current) & this.mask;
                    if (last <= pos ? (last >= slot || slot > pos) : (last >= slot && slot > pos)) {
                        break;
                    }
                    pos = (pos + 1) & this.mask;
                }
                curKeys[last] = current;
            }
        }

        private boolean removeNullEntry() {
            this.containsZero = false;
            this.keys[this.capacity] = 0;
            this.size--;
            if (this.capacity > this.minCapacity && this.size < this.threshold >> 2
                    && this.capacity > DEFAULT_INITIAL_CAPACITY) {
                rehash(this.capacity >> 1);
            }
            this.modCount++;
            return true;
        }

        private void rehash(final int newCapacity) {
            final long[] oldKeys = this.keys;
            final int newMask = newCapacity - 1;
            final long[] newKeys = new long[newCapacity + 1];
            int i = this.capacity, pos;
            for (int j = realSize(); j-- != 0;) {
                while ((oldKeys[--i] == 0))
                    ;
                if (!((newKeys[pos = (int) LongHashSet.mixBits(oldKeys[i]) & newMask]) == 0)) {
                    while (!((newKeys[pos = (pos + 1) & newMask]) == 0))
                        ;
                }
                newKeys[pos] = oldKeys[i];
            }
            this.capacity = newCapacity;
            this.mask = newMask;
            this.threshold = LongHashSet.maxEntriesFill(this.capacity, this.loadFactor);
            this.keys = newKeys;
        }

        private final class LongIterator implements PrimitiveIterator.OfLong {
            private int position = LongHashSet.this.capacity;
            private int last = -1;
            private int counter = LongHashSet.this.size - (LongHashSet.this.containsNull ? 1 : 0);
            private boolean mustReturnZero = LongHashSet.this.containsZero;
            private LongArrayList wrapped;

            @Override
            public long nextLong() {
                if (!hasNext()) {
                    throw new java.util.NoSuchElementException();
                }
                this.counter--;
                if (this.mustReturnZero) {
                    this.mustReturnZero = false;
                    last = LongHashSet.this.capacity;
                    return LongHashSet.this.keys[LongHashSet.this.capacity];
                }
                final long[] curKeys = LongHashSet.this.keys;
                for (;;) {
                    if (--this.position < 0) {
                        this.last = Integer.MIN_VALUE;
                        return this.wrapped.getLong(-this.position - 1);
                    }
                    if (!(curKeys[this.position] == 0)) {
                        return curKeys[this.last = this.position];
                    }
                }
            }

            @Override
            public boolean hasNext() {
                return this.counter != 0;
            }

            private final void shiftKeys(int pos) {
                int lastPos, slot;
                long current;
                final long[] curKeys = LongHashSet.this.keys;
                for (;;) {
                    pos = ((lastPos = pos) + 1) & LongHashSet.this.mask;
                    for (;;) {
                        if (((current = curKeys[pos]) == 0)) {
                            curKeys[lastPos] = 0;
                            return;
                        }
                        slot = (int) LongHashSet.mixBits(current) & LongHashSet.this.mask;
                        if (lastPos <= pos ? (lastPos >= slot || slot > pos) : (lastPos >= slot && slot > pos)) {
                            break;
                        }
                        pos = (pos + 1) & LongHashSet.this.mask;
                    }
                    if (pos < lastPos) {
                        if (wrapped == null) {
                            wrapped = new LongArrayList(2);
                        }
                        wrapped.add(curKeys[pos]);
                    }
                    curKeys[lastPos] = current;
                }
            }

            @Override
            public void remove() {
                if (this.last == -1) {
                    throw new IllegalStateException();
                }
                if (this.last == LongHashSet.this.capacity) {
                    LongHashSet.this.containsZero = false;
                    LongHashSet.this.keys[LongHashSet.this.capacity] = 0;
                } else if (this.position >= 0) {
                    shiftKeys(this.last);
                } else {
                    LongHashSet.this.remove(this.wrapped.getLong(-this.position - 1));
                    this.last = -1;
                    return;
                }
                LongHashSet.this.size--;
                LongHashSet.this.modCount++;
                this.last = -1;
            }

        }

        private final class LongSpliterator implements Spliterator<Number> {
            private int position;
            private int fence;
            private int expectedModCount;
            private long estimatedSize;
            private boolean nullPending;
            private boolean zeroPending;
            private boolean hasSplit;

            LongSpliterator(int position, int fence, int expectedModCount,
                    long estimatedSize, boolean nullPending,
                    boolean zeroPending, boolean hasSplit) {
                this.position = position;
                this.fence = fence;
                this.expectedModCount = expectedModCount;
                this.estimatedSize = estimatedSize;
                this.nullPending = nullPending;
                this.zeroPending = zeroPending;
                this.hasSplit = hasSplit;
            }

            private int getFence() {
                if (fence < 0) {
                    expectedModCount = modCount;
                    estimatedSize = size;
                    nullPending = containsNull;
                    zeroPending = containsZero;
                    fence = capacity;
                }
                return fence;
            }

            private void checkForComodification() {
                if (expectedModCount != modCount) {
                    throw new ConcurrentModificationException();
                }
            }

            @Override
            public Spliterator<Number> trySplit() {
                int high = getFence();
                int low = position;
                int middle = (low + high) >>> 1;
                if (low >= middle) {
                    return null;
                }
                long splitEstimate = estimatedSize >>> 1;
                LongSpliterator split = new LongSpliterator(low, middle,
                        expectedModCount, splitEstimate, nullPending,
                        zeroPending, true);
                position = middle;
                estimatedSize -= splitEstimate;
                nullPending = false;
                zeroPending = false;
                hasSplit = true;
                return split;
            }

            @Override
            public boolean tryAdvance(Consumer<? super Number> action) {
                Objects.requireNonNull(action);
                int high = getFence();
                if (nullPending) {
                    nullPending = false;
                    if (estimatedSize > 0) {
                        estimatedSize--;
                    }
                    action.accept(null);
                    checkForComodification();
                    return true;
                }
                if (zeroPending) {
                    zeroPending = false;
                    if (estimatedSize > 0) {
                        estimatedSize--;
                    }
                    action.accept(Long.valueOf(0L));
                    checkForComodification();
                    return true;
                }
                final long[] curKeys = keys;
                while (position < high) {
                    long key = curKeys[position++];
                    if (key != 0L) {
                        if (estimatedSize > 0) {
                            estimatedSize--;
                        }
                        action.accept(Long.valueOf(key));
                        checkForComodification();
                        return true;
                    }
                }
                checkForComodification();
                return false;
            }

            @Override
            public void forEachRemaining(Consumer<? super Number> action) {
                Objects.requireNonNull(action);
                int high = getFence();
                if (nullPending) {
                    nullPending = false;
                    action.accept(null);
                }
                if (zeroPending) {
                    zeroPending = false;
                    action.accept(Long.valueOf(0L));
                }
                final long[] curKeys = keys;
                while (position < high) {
                    long key = curKeys[position++];
                    if (key != 0L) {
                        action.accept(Long.valueOf(key));
                    }
                }
                estimatedSize = 0;
                checkForComodification();
            }

            @Override
            public long estimateSize() {
                getFence();
                return estimatedSize;
            }

            @Override
            public int characteristics() {
                return Spliterator.DISTINCT | (hasSplit ? 0 : Spliterator.SIZED);
            }
        }

        private static class LongArrayList {
            /** The initial default capacity of an array list. */
            public static final int DEFAULT_INITIAL_CAPACITY = 10;
            /** The backing array. */
            protected long a[];
            /**
             * The current actual size of the list (never greater than the backing-array
             * length).
             */
            protected int size;

            private static final long[] DEFAULT_EMPTY_ARRAY = {};

            private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

            /**
             * Creates a new array list with given capacity.
             *
             * @param capacity the initial capacity of the array list (may be 0).
             */
            public LongArrayList(final int capacity) {
                initArrayFromCapacity(capacity);
            }

            private void initArrayFromCapacity(final int capacity) {
                if (capacity < 0)
                    throw new IllegalArgumentException("Initial capacity (" + capacity + ") is negative");
                if (capacity == 0)
                    a = LongArrayList.DEFAULT_EMPTY_ARRAY;
                else
                    a = new long[capacity];
            }

            public static long[] forceCapacity(final long[] array, final int length, final int preserve) {
                final long t[] = new long[length];
                System.arraycopy(array, 0, t, 0, preserve);
                return t;
            }

            /**
             * Grows this array list, ensuring that it can contain the given number of
             * entries without resizing,
             * and in case increasing the current capacity at least by a factor of 50%.
             *
             * @param capacity the new minimum capacity for this array list.
             */

            private void grow(int capacity) {
                if (capacity <= a.length)
                    return;
                if (a != LongArrayList.DEFAULT_EMPTY_ARRAY)
                    capacity = (int) Math.max(Math.min((long) a.length + (a.length >> 1), LongArrayList.MAX_ARRAY_SIZE),
                            capacity);
                else if (capacity < DEFAULT_INITIAL_CAPACITY)
                    capacity = DEFAULT_INITIAL_CAPACITY;
                a = LongArrayList.forceCapacity(a, capacity, size);
                assert size <= a.length;
            }

            public boolean add(final long k) {
                grow(size + 1);
                a[size++] = k;
                assert size <= a.length;
                return true;
            }

            public long getLong(final int index) {
                if (index >= size)
                    throw new IndexOutOfBoundsException(
                            "Index (" + index + ") is greater than or equal to list size (" + size + ")");
                return a[index];
            }

        }
    }

    private static class IntHashSet extends PrimitiveHashSet {
        int[] keys;

        public IntHashSet() {
            this(DEFAULT_INITIAL_CAPACITY, DEFAULT_LOAD_FACTOR);
        }

        public IntHashSet(final int initialCapacity, final float loadFactor) {
            super(initialCapacity, loadFactor);
            this.keys = new int[this.capacity + 1];
        }

        boolean addInt(int key) {
            int pos;
            if (key == 0) {
                if (this.containsZero) {
                    return false;
                }
                this.containsZero = true;
            } else {
                int current;
                final int[] curKeys = this.keys;
                if (!((current = curKeys[pos = (int) IntHashSet.mixBits(key) & this.mask]) == 0)) {
                    if (current == key) {
                        return false;
                    }
                    while (!((current = curKeys[pos = (pos + 1) & this.mask]) == 0)) {
                        if (current == key) {
                            return false;
                        }
                    }
                }
                curKeys[pos] = key;
            }
            if (this.size++ >= this.threshold) {
                rehash(IntHashSet.leastArraySize(this.size + 1, this.loadFactor));
            }
            this.modCount++;
            return true;
        }

        @Override
        public boolean remove(Object key) {
            if (key == null) {
                return removeNull();
            }
            return key instanceof Integer && removeInt((Integer) key);
        }

        boolean removeInt(int key) {
            if (key == 0) {
                if (this.containsZero) {
                    return removeNullEntry();
                }
                return false;
            }
            int current;
            final int[] curKeys = this.keys;
            int pos;
            if (((current = curKeys[pos = (int) IntHashSet.mixBits(key) & this.mask]) == 0)) {
                return false;
            }
            if (current == key) {
                return removeEntry(pos);
            }
            while (true) {
                if (((current = curKeys[pos = (pos + 1) & this.mask]) == 0)) {
                    return false;
                }
                if (current == key) {
                    return removeEntry(pos);
                }
            }
        }

        @Override
        public boolean contains(Object key) {
            if (key == null) {
                return this.containsNull;
            }
            return key instanceof Integer && containsInt((Integer) key);
        }

        boolean containsInt(int key) {
            if (key == 0) return this.containsZero;
            int current;
            final int[] curKeys = this.keys;
            int pos = (int) IntHashSet.mixBits(key) & this.mask;
            if ((current = curKeys[pos]) == 0) return false;
            if (current == key) return true;
            while ((current = curKeys[pos = (pos + 1) & this.mask]) != 0) {
                if (current == key) return true;
            }
            return false;
        }

        Iterator<Integer> valueIterator() {
            return new IntIterator();
        }

        @Override
        public Spliterator<Number> spliterator() {
            return new IntSpliterator(0, -1, 0, 0, false, false, false);
        }

        @Override
        void forEachElement(Consumer<? super Number> action) {
            if (containsZero) {
                action.accept(Integer.valueOf(0));
            }
            final int[] curKeys = keys;
            for (int i = capacity; i-- != 0;) {
                int key = curKeys[i];
                if (key != 0) {
                    action.accept(Integer.valueOf(key));
                }
            }
        }

        public void clear() {
            if (this.size == 0) {
                return;
            }
            this.size = 0;
            this.containsZero = false;
            this.containsNull = false;
            Arrays.fill(this.keys, 0);
            this.modCount++;
        }

        @Override
        public IntHashSet clone() {
            IntHashSet newSet = (IntHashSet) super.clone();
            newSet.keys = this.keys.clone();
            return newSet;
        }

        @Override
        public int hashCode() {
            int h = 0;
            for (int j = realSize(), i = 0; j-- != 0;) {
                while (this.keys[i] == 0) {
                    i++;
                }
                h += this.keys[i];
                i++;
            }
            return h;
        }

        private boolean removeEntry(int pos) {
            this.size--;
            shiftKeys(pos);
            if (this.capacity > this.minCapacity && this.size < this.threshold >> 2
                    && this.capacity > DEFAULT_INITIAL_CAPACITY) {
                rehash(this.capacity >> 1);
            }
            this.modCount++;
            return true;
        }

        private void shiftKeys(int pos) {
            int last, slot;
            int current;
            final int[] curKeys = this.keys;
            for (;;) {
                pos = ((last = pos) + 1) & this.mask;
                for (;;) {
                    if (((current = curKeys[pos]) == 0)) {
                        curKeys[last] = 0;
                        return;
                    }
                    slot = (int) IntHashSet.mixBits(current) & this.mask;
                    if (last <= pos ? (last >= slot || slot > pos) : (last >= slot && slot > pos)) {
                        break;
                    }
                    pos = (pos + 1) & this.mask;
                }
                curKeys[last] = current;
            }
        }

        private boolean removeNullEntry() {
            this.containsZero = false;
            this.keys[this.capacity] = 0;
            this.size--;
            if (this.capacity > this.minCapacity && this.size < this.threshold >> 2
                    && this.capacity > DEFAULT_INITIAL_CAPACITY) {
                rehash(this.capacity >> 1);
            }
            this.modCount++;
            return true;
        }

        private void rehash(final int newCapacity) {
            final int[] oldKeys = this.keys;
            final int newMask = newCapacity - 1;
            final int[] newKeys = new int[newCapacity + 1];
            int i = this.capacity, pos;
            for (int j = realSize(); j-- != 0;) {
                while ((oldKeys[--i] == 0))
                    ;
                if (!((newKeys[pos = (int) IntHashSet.mixBits(oldKeys[i]) & newMask]) == 0)) {
                    while (!((newKeys[pos = (pos + 1) & newMask]) == 0))
                        ;
                }
                newKeys[pos] = oldKeys[i];
            }
            this.capacity = newCapacity;
            this.mask = newMask;
            this.threshold = IntHashSet.maxEntriesFill(this.capacity, this.loadFactor);
            this.keys = newKeys;
        }

        private final class IntIterator implements PrimitiveIterator.OfInt {
            private int position = IntHashSet.this.capacity;
            private int last = -1;
            private int counter = IntHashSet.this.size - (IntHashSet.this.containsNull ? 1 : 0);
            private boolean mustReturnZero = IntHashSet.this.containsZero;
            private IntArrayList wrapped;

            @Override
            public int nextInt() {
                if (!hasNext()) {
                    throw new java.util.NoSuchElementException();
                }
                this.counter--;
                if (this.mustReturnZero) {
                    this.mustReturnZero = false;
                    last = IntHashSet.this.capacity;
                    return IntHashSet.this.keys[IntHashSet.this.capacity];
                }
                final int[] curKeys = IntHashSet.this.keys;
                for (;;) {
                    if (--this.position < 0) {
                        this.last = Integer.MIN_VALUE;
                        return this.wrapped.getInt(-this.position - 1);
                    }
                    if (!(curKeys[this.position] == 0)) {
                        return curKeys[this.last = this.position];
                    }
                }
            }

            @Override
            public boolean hasNext() {
                return this.counter != 0;
            }

            private final void shiftKeys(int pos) {
                int lastPos, slot;
                int current;
                final int[] curKeys = IntHashSet.this.keys;
                for (;;) {
                    pos = ((lastPos = pos) + 1) & IntHashSet.this.mask;
                    for (;;) {
                        if (((current = curKeys[pos]) == 0)) {
                            curKeys[lastPos] = 0;
                            return;
                        }
                        slot = (int) IntHashSet.mixBits(current) & IntHashSet.this.mask;
                        if (lastPos <= pos ? (lastPos >= slot || slot > pos) : (lastPos >= slot && slot > pos)) {
                            break;
                        }
                        pos = (pos + 1) & IntHashSet.this.mask;
                    }
                    if (pos < lastPos) {
                        if (wrapped == null) {
                            wrapped = new IntArrayList(2);
                        }
                        wrapped.add(curKeys[pos]);
                    }
                    curKeys[lastPos] = current;
                }
            }

            @Override
            public void remove() {
                if (this.last == -1) {
                    throw new IllegalStateException();
                }
                if (this.last == IntHashSet.this.capacity) {
                    IntHashSet.this.containsZero = false;
                    IntHashSet.this.keys[IntHashSet.this.capacity] = 0;
                } else if (this.position >= 0) {
                    shiftKeys(this.last);
                } else {
                    IntHashSet.this.remove(this.wrapped.getInt(-this.position - 1));
                    this.last = -1;
                    return;
                }
                IntHashSet.this.size--;
                IntHashSet.this.modCount++;
                this.last = -1;
            }

        }

        private final class IntSpliterator implements Spliterator<Number> {
            private int position;
            private int fence;
            private int expectedModCount;
            private long estimatedSize;
            private boolean nullPending;
            private boolean zeroPending;
            private boolean hasSplit;

            IntSpliterator(int position, int fence, int expectedModCount,
                    long estimatedSize, boolean nullPending,
                    boolean zeroPending, boolean hasSplit) {
                this.position = position;
                this.fence = fence;
                this.expectedModCount = expectedModCount;
                this.estimatedSize = estimatedSize;
                this.nullPending = nullPending;
                this.zeroPending = zeroPending;
                this.hasSplit = hasSplit;
            }

            private int getFence() {
                if (fence < 0) {
                    expectedModCount = modCount;
                    estimatedSize = size;
                    nullPending = containsNull;
                    zeroPending = containsZero;
                    fence = capacity;
                }
                return fence;
            }

            private void checkForComodification() {
                if (expectedModCount != modCount) {
                    throw new ConcurrentModificationException();
                }
            }

            @Override
            public Spliterator<Number> trySplit() {
                int high = getFence();
                int low = position;
                int middle = (low + high) >>> 1;
                if (low >= middle) {
                    return null;
                }
                long splitEstimate = estimatedSize >>> 1;
                IntSpliterator split = new IntSpliterator(low, middle,
                        expectedModCount, splitEstimate, nullPending,
                        zeroPending, true);
                position = middle;
                estimatedSize -= splitEstimate;
                nullPending = false;
                zeroPending = false;
                hasSplit = true;
                return split;
            }

            @Override
            public boolean tryAdvance(Consumer<? super Number> action) {
                Objects.requireNonNull(action);
                int high = getFence();
                if (nullPending) {
                    nullPending = false;
                    if (estimatedSize > 0) {
                        estimatedSize--;
                    }
                    action.accept(null);
                    checkForComodification();
                    return true;
                }
                if (zeroPending) {
                    zeroPending = false;
                    if (estimatedSize > 0) {
                        estimatedSize--;
                    }
                    action.accept(Integer.valueOf(0));
                    checkForComodification();
                    return true;
                }
                final int[] curKeys = keys;
                while (position < high) {
                    int key = curKeys[position++];
                    if (key != 0) {
                        if (estimatedSize > 0) {
                            estimatedSize--;
                        }
                        action.accept(Integer.valueOf(key));
                        checkForComodification();
                        return true;
                    }
                }
                checkForComodification();
                return false;
            }

            @Override
            public void forEachRemaining(Consumer<? super Number> action) {
                Objects.requireNonNull(action);
                int high = getFence();
                if (nullPending) {
                    nullPending = false;
                    action.accept(null);
                }
                if (zeroPending) {
                    zeroPending = false;
                    action.accept(Integer.valueOf(0));
                }
                final int[] curKeys = keys;
                while (position < high) {
                    int key = curKeys[position++];
                    if (key != 0) {
                        action.accept(Integer.valueOf(key));
                    }
                }
                estimatedSize = 0;
                checkForComodification();
            }

            @Override
            public long estimateSize() {
                getFence();
                return estimatedSize;
            }

            @Override
            public int characteristics() {
                return Spliterator.DISTINCT | (hasSplit ? 0 : Spliterator.SIZED);
            }
        }

        private static class IntArrayList {
            /** The initial default capacity of an array list. */
            public static final int DEFAULT_INITIAL_CAPACITY = 10;
            /** The backing array. */
            protected int a[];
            /**
             * The current actual size of the list (never greater than the backing-array
             * length).
             */
            protected int size;

            private static final int[] DEFAULT_EMPTY_ARRAY = {};

            private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

            /**
             * Creates a new array list with given capacity.
             *
             * @param capacity the initial capacity of the array list (may be 0).
             */
            public IntArrayList(final int capacity) {
                initArrayFromCapacity(capacity);
            }

            private void initArrayFromCapacity(final int capacity) {
                if (capacity < 0)
                    throw new IllegalArgumentException("Initial capacity (" + capacity + ") is negative");
                if (capacity == 0)
                    a = IntArrayList.DEFAULT_EMPTY_ARRAY;
                else
                    a = new int[capacity];
            }

            public static int[] forceCapacity(final int[] array, final int length, final int preserve) {
                final int t[] = new int[length];
                System.arraycopy(array, 0, t, 0, preserve);
                return t;
            }

            /**
             * Grows this array list, ensuring that it can contain the given number of
             * entries without resizing,
             * and in case increasing the current capacity at least by a factor of 50%.
             *
             * @param capacity the new minimum capacity for this array list.
             */

            private void grow(int capacity) {
                if (capacity <= a.length)
                    return;
                if (a != IntArrayList.DEFAULT_EMPTY_ARRAY)
                    capacity = (int) Math.max(Math.min((long) a.length + (a.length >> 1), IntArrayList.MAX_ARRAY_SIZE),
                            capacity);
                else if (capacity < DEFAULT_INITIAL_CAPACITY)
                    capacity = DEFAULT_INITIAL_CAPACITY;
                a = IntArrayList.forceCapacity(a, capacity, size);
                assert size <= a.length;
            }

            public boolean add(final int k) {
                grow(size + 1);
                a[size++] = k;
                assert size <= a.length;
                return true;
            }

            public int getInt(final int index) {
                if (index >= size)
                    throw new IndexOutOfBoundsException(
                            "Index (" + index + ") is greater than or equal to list size (" + size + ")");
                return a[index];
            }

        }
    }
}
