package ru.ifmo.rain.naumov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.rmi.server.UnicastRemoteObject;
import java.util.Collections;
import java.util.Set;
import java.util.Map;

/**
 * Remote version {@link Bank} implementation
 * @author Simon Naumov
 */
public class RemoteBank extends UnicastRemoteObject implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<Long, Person> persons = new ConcurrentHashMap<>();
    private final ConcurrentMap<Long, Set<String>> accountsByPassport = new ConcurrentHashMap<>();

    /**
     * Creates an instance of {@link RemoteBank}
     * @param port port to procedure
     */
    public RemoteBank(final int port) throws RemoteException {
        super(port);
        this.port = port;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Person createPerson(String name, String surname, long passportNumber) throws RemoteException {
        if (name == null || surname == null) {
            return null;
        }
        if (persons.containsKey(passportNumber)) {
            return persons.get(passportNumber);
        }
        System.out.println(String.format("Creating person with name: %s %s, and passport id: %d", name, surname, passportNumber));

        Person person = new RemotePerson(name, surname, passportNumber, port);
        persons.put(passportNumber, person);
        accountsByPassport.put(passportNumber, Collections.newSetFromMap(new ConcurrentHashMap<>()));
        return person;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Person getLocalPerson(long passport) throws RemoteException {
        Person person = persons.get(passport);
        if (person == null) {
            return null;
        }

        Map<String, LocalAccount> accounts = new ConcurrentHashMap<>();
        Set<String> accountIds = getPersonAccounts(person);

        try {
            for (String accountId : accountIds) {
                Account remoteAccount = getAccount(person, accountId);
                accounts.put(accountId, new LocalAccount(remoteAccount.getId(), remoteAccount.getAmount()));
            }
        } catch (RemoteException e) {
            System.out.println("Failed to create local accounts " + e);
            java.util.Arrays.stream(e.getStackTrace()).forEach(System.out::println);
            return null;
        }

        return new LocalPerson(person.getName(), person.getSurname(), person.getPassportId(), accounts);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Person getRemotePerson(long passport) throws RemoteException {
        return persons.get(passport);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getPersonAccounts(Person person) throws RemoteException {
        if (person == null) {
            return null;
        }

        System.out.println(String.format("Retrieving accounts of a person with %d passport number", person.getPassportId()));
        if (person instanceof LocalPerson) {
            return ((LocalPerson) person).getAccounts();
        }
        return accountsByPassport.get(person.getPassportId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Account createAccount(Person person, String id) throws RemoteException {
        if (person == null || id == null) {
            return null;
        }

        long passportId = person.getPassportId();
        String accountId = accountId(passportId, id);

        if (accounts.containsKey(accountId)) {
            return accounts.get(accountId);
        }

        System.out.println(String.format("Creating account %s", accountId));
        final Account account = new RemoteAccount(id);
        accounts.put(accountId, account);
        accountsByPassport.putIfAbsent(passportId, Collections.newSetFromMap(new ConcurrentHashMap<>()));
        accountsByPassport.get(passportId).add(id);

        return account;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Account getAccount(Person person, String id) throws RemoteException {
        if (person == null || id == null) {
            return null;
        }

        String accountId = accountId(person.getPassportId(), id);
        Account account = accounts.get(accountId);

        if (account == null) {
            return null;
        }

        System.out.println(String.format("Retrieving account %s $%d", accountId, account.getAmount()));
        if (person instanceof LocalPerson) {
            return ((LocalPerson) person).getAccount(id);
        }
        return account;
    }

    private static String accountId(long passportId, String id) {
        return String.format("%d:%s", passportId, id);
    }
}
