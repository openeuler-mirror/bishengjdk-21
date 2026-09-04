/*
 * Copyright (C) 2002-2026 Sebastiano Vigna
 * Copyright (c) 2026 Huawei Technologies Co., Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package java.util;

import java.util.function.Consumer;

/**
 * Primitive int representation used by the bytecode-enhanced HashSet.
 *
 * <p>This implementation incorporates and adapts ideas and portions from
 * fastutil OpenHashSet, with separate null and zero-key handling required by
 * java.util.HashSet semantics.
 */
class IntHashSet extends PrimitiveHashSet {
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
        private static final int[] DEFAULT_EMPTY_ARRAY = {};
        private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

        /** The backing array. */
        protected int a[];
        /**
         * The current actual size of the list (never greater than the backing-array
         * length).
         */
        protected int size;

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
