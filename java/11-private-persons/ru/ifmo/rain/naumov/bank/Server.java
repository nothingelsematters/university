package ru.ifmo.rain.naumov.bank;

import java.rmi.*;
import java.rmi.server.*;
import java.net.*;

/**
 * Server application to work with {@link Bank}
 * @author Simon Naumov
 */
public class Server {
    private final static int PORT = 8888;

    /** Start server application  */
    public static void main(final String... args) {
        final Bank bank;
        try {
            bank = new RemoteBank(PORT);
        } catch (RemoteException ignored) {
            System.out.println("Remote problems...");
            System.exit(1);
            return;
        }

        try {
            Naming.rebind("//localhost/bank", bank);
        } catch (final RemoteException e) {
            System.out.println("Cannot export object: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        } catch (final MalformedURLException e) {
            System.out.println("Malformed URL");
            System.exit(1);
        }
        System.out.println("Server started");
    }
}
