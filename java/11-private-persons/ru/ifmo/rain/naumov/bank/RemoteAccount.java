package ru.ifmo.rain.naumov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Remote version {@link Account} implementation.
 * @author Simon Naumov
 */
public class RemoteAccount extends UnicastRemoteObject implements Account {
    private final String id;
    private final AtomicInteger amount;

    RemoteAccount(final String id) throws RemoteException {
        this.id = id;
        amount = new AtomicInteger(0);
    }

    /**
     * Returns account id
     * @return account id
     */
    public String getId() {
        return id;
    }

    /**
     * Returns account money amount
     * @return account money amount
     */
    public int getAmount() {
        System.out.println("Getting amount of money for account " + id);
        return amount.get();
    }

    /**
     * Updates account money amount with a specified value
     * @param amount new money amount value
     */
    public void setAmount(final int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount.set(amount);
    }
}
