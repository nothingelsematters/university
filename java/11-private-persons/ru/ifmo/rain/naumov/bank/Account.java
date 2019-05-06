package ru.ifmo.rain.naumov.bank;

import java.rmi.*;

/**
 * Bank account interface.
 * @author Simon Naumov
 */
public interface Account extends Remote {
    /** Returns account identifier. */
    String getId() throws RemoteException;

    /** Returns amount of money at the account. */
    int getAmount() throws RemoteException;

    /** Sets amount of money at the account. */
    void setAmount(int amount) throws RemoteException;
}
