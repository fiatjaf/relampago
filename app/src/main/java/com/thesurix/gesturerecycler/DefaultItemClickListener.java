package com.thesurix.gesturerecycler;

/**
 * Default implementation of the {@link RecyclerItemTouchListener.ItemClickListener}.
 * @author thesurix
 */
public class DefaultItemClickListener<T> implements RecyclerItemTouchListener.ItemClickListener<T> {

    @Override
    public boolean onItemClick(final T item, final int position) {
        return false;
    }

    @Override
    public void onItemLongPress(final T item, final int position) {

    }

    @Override
    public boolean onDoubleTap(final T item, final int position) {
        return false;
    }
}
