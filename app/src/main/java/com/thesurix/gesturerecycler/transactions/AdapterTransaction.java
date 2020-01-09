package com.thesurix.gesturerecycler.transactions;

/**
 * @author thesurix
 */
public interface AdapterTransaction {

    boolean perform();

    boolean revert();
}
