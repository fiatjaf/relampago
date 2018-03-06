package com.thesurix.gesturerecycler;

import android.support.annotation.Nullable;
import android.support.v7.widget.RecyclerView;
import android.view.View;

/**
 * Base view holder class for gesture compatible items.
 * @author thesurix
 */
public class GestureViewHolder extends RecyclerView.ViewHolder {

    public boolean swipable = false;

    public GestureViewHolder(final View itemView) {
        super(itemView);
    }

    /**
     * Returns view that can spawn drag gesture. If there is no view simply return null.
     * @return view that can spawn drag gesture
     */
    @Nullable
    public View getDraggableView() {
        return null;
    }

    /**
     * Returns top visible view (originally root view of the item),
     * override this method to use background view feature in case of swipe gestures.
     * @return top view
     */
    public View getForegroundView() {
        return itemView;
    }

    /**
     * Returns background view which is visible when foreground view is partially or fully swiped.
     * @return background view
     */
    @Nullable
    public View getBackgroundView() {
        return null;
    }

    /**
     * Method that shows view for manual drag gestures.
     * Called only when getDraggableView() returns valid view.
     */
    public void showDraggableView() {
        getDraggableView().setVisibility(View.VISIBLE);
    }

    /**
     * Method that hides view for manual drag gestures.
     * Called only when getDraggableView() returns valid view.
     */
    public void hideDraggableView() {
        getDraggableView().setVisibility(View.GONE);
    }

    /**
     * Indicates that view is selected.
     */
    public void onItemSelect() {}

    /**
     * Indicates that view has no selection.
     */
    public void onItemClear() {}

    /**
     * Returns information if we can drag this view.
     * @return true if draggable, false otherwise
     */
    public boolean canDrag() { return true; }

    /**
     * Returns information if we can swipe this view.
     * @return true if swipeable, false otherwise
     */
    public boolean canSwipe() { return swipable; }
}
