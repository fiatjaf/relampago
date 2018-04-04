package com.lightning.walletapp;


public class Informer {
    // These should be added on start
    public static final int PEER = 1;
    public static final int LNSTATE = 2;

    // Temporary btc and ln infos
    public static final int BTCEVENT = 3;
    public static final int CHAINSYNC = 4;
    public static final int CODECHECK = 5;
    public static final int TXCONFIRMED = 6;
    public static final int LNPAYMENT = 7;

    // Special emergency state
    public static final int EMERGENCY = 8;

    public int tag;
    public String value;
    public Informer(String val, int tag)
    {
        this.value = val;
        this.tag = tag;
    }
}
