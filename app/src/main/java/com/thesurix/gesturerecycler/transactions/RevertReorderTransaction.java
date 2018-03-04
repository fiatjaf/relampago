package com.thesurix.gesturerecycler.transactions;

import com.thesurix.gesturerecycler.GestureAdapter;
import com.thesurix.gesturerecycler.GestureViewHolder;

/**
 * @author thesurix
 */
public class RevertReorderTransaction<T> implements AdapterTransaction {

    private final GestureAdapter<T, ? extends GestureViewHolder> mAdapter;
    private final int mFrom;
    private final int mTo;

    public RevertReorderTransaction(final GestureAdapter<T, ? extends GestureViewHolder> adapter, final int from, final int to) {
        mAdapter = adapter;
        mFrom = from;
        mTo = to;
    }

    @Override
    public boolean perform() {
        return false;
    }

    @Override
    public boolean revert() {
        final T item = mAdapter.getData().remove(mTo);
        if (item != null) {
            mAdapter.notifyItemRemoved(mTo);
            mAdapter.getData().add(mFrom, item);
            mAdapter.notifyItemInserted(mFrom);
            return true;
        }

        return false;
    }
}
