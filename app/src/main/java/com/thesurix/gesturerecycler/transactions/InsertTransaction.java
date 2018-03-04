package com.thesurix.gesturerecycler.transactions;


import com.thesurix.gesturerecycler.GestureAdapter;
import com.thesurix.gesturerecycler.GestureViewHolder;

/**
 * @author thesurix
 */
public class InsertTransaction<T> implements AdapterTransaction {

    private final GestureAdapter<T, ? extends GestureViewHolder> mAdapter;
    private final T mItem;
    private final int mPosition;

    public InsertTransaction(final GestureAdapter<T, ? extends GestureViewHolder> adapter, final T item, final int position) {
        mAdapter = adapter;
        mItem = item;
        mPosition = position;
    }

    @Override
    public boolean perform() {
        mAdapter.getData().add(mPosition, mItem);
        mAdapter.notifyItemInserted(mPosition);
        return true;
    }

    @Override
    public boolean revert() {
        final T item = mAdapter.getData().remove(mPosition);
        final boolean success = item != null;
        if (success) {
            mAdapter.notifyItemRemoved(mPosition);
        }
        return success;
    }
}
