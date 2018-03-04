package com.thesurix.gesturerecycler.util;

import java.util.ArrayDeque;

/**
 * @author thesurix
 */
public class FixedSizeArrayDequeue<E> extends ArrayDeque<E> {

    private int mMaxSize;

    public FixedSizeArrayDequeue(final int maxSize) {
        super(maxSize);
        mMaxSize = maxSize;
    }

    @Override
    public boolean offer(final E e) {
        if (size() == mMaxSize) {
            removeFirst();
        }

        return super.offer(e);
    }
}
