package ru.ifmo.rain.naumov.bank;

import org.junit.Test;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.runners.MethodSorters;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.Rule;
import org.junit.runner.Description;
import org.junit.rules.TestWatcher;
import org.junit.rules.TestRule;
import org.junit.runner.notification.Failure;
import static org.junit.Assert.*;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.net.MalformedURLException;
import java.util.function.Consumer;
import java.util.Random;
import java.util.Set;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class BankTest {
    private static final long start = System.currentTimeMillis();
    private static final int PORT = 8888;
    private static final int TEST_AMOUNT = 10;
    private static final Random random = new Random();
    private static Bank bank;

    @Before
    public void beforeClass() {
        try {
            Naming.rebind("//localhost/bank", new RemoteBank(PORT));
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (RemoteException | NotBoundException | MalformedURLException e) {
            System.out.println("FAILURE: " + e);
            java.util.Arrays.stream(e.getStackTrace()).forEach(System.out::println);
            System.exit(1);
        }

        System.out.println("Bank created");
    }

    @Rule
    public TestRule watcher = watcher(description -> System.err.println("=== Running " + description.getMethodName()));

    protected static TestWatcher watcher(final Consumer<Description> watcher) {
        return new TestWatcher() {
            @Override
            protected void starting(final Description description) {
                watcher.accept(description);
            }
        };
    }

    @Test
    public void test01_getPerson() throws RemoteException {
        for (int i = 0; i < TEST_AMOUNT; i++) {
            final String name = String.valueOf(random.nextInt());
            final String surname = String.valueOf(random.nextInt());
            final long passport = i;


            bank.createPerson(name, surname, passport);
            Person remotePerson = bank.getRemotePerson(passport);
            assertEquals(name, remotePerson.getName());
            assertEquals(surname, remotePerson.getSurname());
            assertEquals(passport, remotePerson.getPassportId());

            Person localPerson = bank.getLocalPerson(passport);
            assertEquals(name, localPerson.getName());
            assertEquals(surname, localPerson.getSurname());
            assertEquals(passport, localPerson.getPassportId());
        }

        assertNull(bank.getLocalPerson(-1));
        assertNull(bank.getRemotePerson(-1));
    }

    @Test
    public void test02_existing() throws RemoteException {
        for (int i = 0; i < TEST_AMOUNT; i++) {
            final String name = String.valueOf(random.nextInt());
            final String surname = String.valueOf(random.nextInt());
            final long passport = i;

            assertFalse(personExists(name, surname, passport));
            assertNotNull(bank.createPerson(name, surname, passport));
            assertTrue(personExists(name, surname, passport));
        }
    }

    @Test
    public void test03_getAccountIds() throws RemoteException {
        for (int i = 0; i < TEST_AMOUNT; ++i) {
            final String name = String.valueOf(random.nextInt());
            final String surname = String.valueOf(random.nextInt());
            final long passport = i;

            assertNotNull(bank.createPerson(name, surname, passport));

            int operations = random.nextInt(TEST_AMOUNT);
            int doneOperations = 0;
            Person person = bank.getRemotePerson(passport);

            for (int j = 0; j < operations; j++) {
                if (!personExists(person.getName(), person.getSurname(), person.getPassportId())) {
                    bank.createAccount(person, String.valueOf(random.nextInt()));
                    doneOperations++;
                }
            }
            Set<String> ids = bank.getPersonAccounts(person);
            assertNotNull(ids);
            assertEquals(doneOperations, ids.size());
        }
    }

    @Test
    public void test04_localThenRemote() throws RemoteException {
        final String name = String.valueOf(random.nextInt());
        final String surname = String.valueOf(random.nextInt());
        final long passport = 0;

        assertNotNull(bank.createPerson(name, surname, passport));
        Person remotePerson = bank.getRemotePerson(passport);
        assertNotNull(remotePerson);

        assertNotNull(bank.createAccount(remotePerson,  name));
        Person localPerson = bank.getLocalPerson(passport);
        assertNotNull(localPerson);

        Account localAccount = bank.getAccount(localPerson, name);
        localAccount.setAmount(localAccount.getAmount() + 100);

        Account remoteAccount = bank.getAccount(remotePerson, name);
        assertEquals(100, localAccount.getAmount());
        assertEquals(0, remoteAccount.getAmount());
    }

    @Test
    public void test05_remoteThenLocal() throws RemoteException {
        final String name = String.valueOf(random.nextInt());
        final String surname = String.valueOf(random.nextInt());
        final long passport = 0;

        assertNotNull(bank.createPerson(name, surname, passport));
        Person remotePerson = bank.getRemotePerson(passport);
        assertNotNull(remotePerson);

        assertNotNull(bank.createAccount(remotePerson,  name));
        Account remoteAccount = bank.getAccount(remotePerson, name);
        Person localPerson = bank.getLocalPerson(passport);
        assertNotNull(localPerson);

        remoteAccount.setAmount(remoteAccount.getAmount() + TEST_AMOUNT);

        Person localPersonAfter = bank.getLocalPerson(passport);
        assertNotNull(localPersonAfter);

        Account localAccount = bank.getAccount(localPerson, name);
        Account localAccountAfter = bank.getAccount(localPersonAfter, name);
        assertEquals(localAccountAfter.getAmount(), remoteAccount.getAmount());
        assertEquals(localAccount.getAmount() + TEST_AMOUNT, localAccountAfter.getAmount());
    }

    private static boolean personExists(final String name, final String surname, final long passport) throws RemoteException {
        Person person = bank.getRemotePerson(passport);
        if (person == null || !person.getName().equals(name) || !person.getSurname().equals(surname)) {
            return false;
        }
        return true;
    }

    public static void main(String[] args) {
        final Result result = new JUnitCore().run(BankTest.class);
        if (!result.wasSuccessful()) {
            for (final Failure failure : result.getFailures()) {
                System.err.printf("Test %s failed: %s%n", failure.getDescription().getMethodName(), failure.getMessage());
                if (failure.getException() != null) {
                    failure.getException().printStackTrace();
                }
            }
            System.exit(1);
        } else {
            System.out.println("=".repeat(28));
            final long time = System.currentTimeMillis() - start;
            System.out.printf("SUCCESS in %dms %n", time);
            System.exit(0);
        }
    }
}
