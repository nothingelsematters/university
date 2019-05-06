package ru.ifmo.rain.naumov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

/**
 * Client application to work with {@link Bank}
 * @author Simon Naumov
 */
public class Client {

    /**
     * Client application with the following arguments:
     * {@code [name] [surname] [passport] [account id] [change]}
     * @param  args args
     */
    public static void main(String[] args) throws RemoteException {
        if (args == null || args.length != 5) {
            printUsage();
            return;
        }

        String name = args[0];
        String surname = args[1];
        String accountId = args[3];
        long passport;
        int change;
        try {
            passport = Long.parseLong(args[2]);
            change = Integer.parseInt(args[4]);
        } catch (IllegalArgumentException ignored) {
            printUsage();
            return;
        }

        final Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        Person person = bank.createPerson(name, surname, passport);
        Account account = bank.createAccount(person, accountId);

        System.out.println(String.format("Account id: %s%nBalance: %d%nPerfoming operation...", account.getId(), account.getAmount()));
        account.setAmount(account.getAmount() + change);
        System.out.println(String.format("Money: %d", account.getAmount()));
    }

    private static void printUsage() {
        System.out.println("Usage: [name] [surname] [passport] [account id] [change]");
    }
}
