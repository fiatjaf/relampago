package com.thesurix.gesturerecycler;


import android.support.v7.widget.helper.ItemTouchHelper;

/**
 * Default gesture listener that handles manual spawned drag gestures.
 * @author thesurix
 */
public class GestureListener implements GestureAdapter.OnGestureListener {

    private final ItemTouchHelper mTouchHelper;

    public GestureListener(final ItemTouchHelper touchHelper) {
        mTouchHelper = touchHelper;
    }

    @Override
    public void onStartDrag(final GestureViewHolder viewHolder) {
        mTouchHelper.startDrag(viewHolder);
    }
}
