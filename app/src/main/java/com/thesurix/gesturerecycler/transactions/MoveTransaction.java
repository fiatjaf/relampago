package com.thesurix.gesturerecycler.transactions;


import com.thesurix.gesturerecycler.GestureAdapter;
import com.thesurix.gesturerecycler.GestureViewHolder;

public class MoveTransaction <T> implements AdapterTransaction {

    private final GestureAdapter<T, ? extends GestureViewHolder> mAdapter;
    private final int mFromPosition;
    private final int mToPosition;
    private T mItem;

    public MoveTransaction(final GestureAdapter<T, ? extends GestureViewHolder> adapter, final int fromPosition, final int toPosition) {
        mAdapter = adapter;
        mFromPosition = fromPosition;
        mToPosition = toPosition;
    }

    @Override
    public boolean perform() {
        mItem = mAdapter.getData().remove(mFromPosition);
        final boolean success = mItem != null;
        if (success) {
            mAdapter.getData().add(mToPosition, mItem);
            mAdapter.notifyItemMoved(mFromPosition, mToPosition);
        }
        return success;
    }

    @Override
    public boolean revert() {
        mItem = mAdapter.getData().remove(mToPosition);
        final boolean success = mItem != null;
        if (success) {
            mAdapter.getData().add(mFromPosition, mItem);
            mAdapter.notifyItemMoved(mToPosition, mFromPosition);
        }
        return success;
    }
}