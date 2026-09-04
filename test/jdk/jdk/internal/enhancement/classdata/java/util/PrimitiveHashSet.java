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
 * Common contract for the primitive-specialized HashSet representations.
 *
 * <p>This implementation incorporates and adapts ideas and portions from
 * fastutil OpenHashSet. It is specialized for the bytecode-enhanced
 * java.util.HashSet implementation in BiSheng JDK.
 */
abstract class PrimitiveHashSet extends AbstractSet<Number> implements Cloneable {
    static final long LONG_PHI = 0x9E3779B97F4A7C15L;

    static final int DEFAULT_INITIAL_CAPACITY = 16;

    static final float DEFAULT_LOAD_FACTOR = .75f;

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
                if (!canRemove) {
                    throw new IllegalStateException();
                }
                checkForComodification();
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
