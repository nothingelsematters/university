package ru.ifmo.rain.naumov.bank;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;


/**
 * Serializable version {@lonk Person} implementation
 * @author Simon Naumov
 */
public class LocalPerson implements Person, Serializable {
    private final String name;
    private final String surname;
    private final long passportId;
    private final Map<String, LocalAccount> accounts;

    LocalPerson(final String name, final String surname, final long passportId, final Map<String, LocalAccount> accounts) {
        super();
        this.name = name;
        this.surname = surname;
        this.passportId = passportId;
        this.accounts = accounts;
    }

    /** Returns person's name. */
    @Override
    public String getName() {
        return name;
    }

    /** Returns person's surname. */
    @Override
    public String getSurname() {
        return surname;
    }

    /** Returns person's passport id. */
    @Override
    public long getPassportId() {
        return passportId;
    }

    /** Returns person's accounts. */
    Set<String> getAccounts() {
        return accounts.keySet();
    }

    /**
     * Returns person's accout by an account id.
     * @param accountId id of an ccount to return
     */
    Account getAccount(String accountId) {
        return accounts.get(accountId);
    }
}
