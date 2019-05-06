package ru.ifmo.rain.naumov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

/**
 * Bank interface.
 * @author Simon Naumov
 */
public interface Bank extends Remote {
    /**
     * Creates a new person with specified identifiers if it is not already exists.
     * @param  name            person's name
     * @param  surname         person's surname
     * @param  passportNumber  person's passport number
     * @return                 created or existing person
     */
    Person createPerson(String name, String surname, long passportNumber) throws RemoteException;

    /**
     * Returns serializable person by passport.
     * @param  passport person's passport number
     * @return          serializable person with specified passport number of {@code null} if such person dows not exist.
     */
    Person getLocalPerson(long passport) throws RemoteException;

    /**
     * Returns person by passport.
     * @param  passport person's passport number
     * @return          person with specified passport number of {@code null} if such person dows not exist.
     */
    Person getRemotePerson(long passport) throws RemoteException;

    /**
     * Returns a set of person's accounts
     * @param  person requested person
     * @return        a set of accounts
     */
    Set<String> getPersonAccounts(Person person) throws RemoteException;

    /**
     * Creates a new account with person and specified identifier if it is not already exists.
     * @param person account owner
     * @param id     account id
     * @return created or existing account.
     */
    Account createAccount(Person person, String id) throws RemoteException;

    /**
     * Returns account by person and identifier.
     * @param person account owner
     * @param id     account id
     * @return account with specified identifier or {@code null} if such account does not exist.
     */
    Account getAccount(Person person, String id) throws RemoteException;
}
