package ru.ifmo.rain.naumov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * A local version of an {@link Account}
 * @author Simon Naumov
 */
public class LocalAccount implements Account, Serializable {
    private final String id;
    private int amount;

    /**
     * Creates an instance of a {@link LocalAccount} with specified id and amount
     * @param  id     account id
     * @param  amount account money amount
     */
    LocalAccount(String id, int amount) throws RemoteException {
        super();
        this.id = id;
        this.amount = amount;
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
        return amount;
    }

    /**
     * Updates money amount with a specified value
     */
    public void setAmount(int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }
}
