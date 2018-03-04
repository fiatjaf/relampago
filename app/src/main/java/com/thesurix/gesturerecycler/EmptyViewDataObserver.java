package com.thesurix.gesturerecycler;

import android.support.v7.widget.RecyclerView;
import android.view.View;

/**
 * Observer class for managing visibility of the adapter's empty view.
 */
class EmptyViewDataObserver extends RecyclerView.AdapterDataObserver {

    private RecyclerView mRecyclerView;
    private View mEmptyView;

    @Override
    public void onChanged() {
        updateEmptyViewState();
    }

    @Override
    public void onItemRangeInserted(final int positionStart, final int itemCount) {
        updateEmptyViewState();
    }

    @Override
    public void onItemRangeRemoved(final int positionStart, final int itemCount) {
        updateEmptyViewState();
    }

    public void setEmptyView(final View emptyView) {
        mEmptyView = emptyView;
        updateEmptyViewState();
    }

    public void setRecyclerView(final RecyclerView recyclerView) {
        mRecyclerView = recyclerView;
        updateEmptyViewState();
    }

    private void updateEmptyViewState() {
        if (mEmptyView != null && mRecyclerView != null) {
            if (mRecyclerView.getAdapter().getItemCount() == 0) {
                mEmptyView.setVisibility(View.VISIBLE);
                mRecyclerView.setVisibility(View.GONE);
            } else {
                mEmptyView.setVisibility(View.GONE);
                mRecyclerView.setVisibility(View.VISIBLE);
            }
        }
    }
}
