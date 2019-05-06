package ru.ifmo.rain.naumov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Bank client interface.
 * @author Simon Naumov
 */
public interface Person extends Remote {
    /** Returns person's name. */
    String getName() throws RemoteException;

    /** Returns person's surname. */
    String getSurname() throws RemoteException;

    /** Returns person's passport number. */
    long getPassportId() throws RemoteException;
}
