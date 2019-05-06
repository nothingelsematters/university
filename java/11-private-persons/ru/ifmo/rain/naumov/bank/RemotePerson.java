package ru.ifmo.rain.naumov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;


/**
 * Remote {@link Person} version implementation
 * @author Simon Naumov
 */
public class RemotePerson extends UnicastRemoteObject implements Person {
    private final String name;
    private final String surname;
    private final long passportId;

    /**
     * Creates an instance of {@link RemotePerson} with given values
     * @param  name            person's name
     * @param  surname         person's surname
     * @param  pasportId       person's passport number
     * @param  port            port to process
     */
    public RemotePerson(final String name, final String surname, final long passportId, final int port) throws RemoteException {
        super(port);
        this.name = name;
        this.surname = surname;
        this.passportId = passportId;
    }

    /** Returns person's name */
    @Override
    public String getName() throws RemoteException {
        return name;
    }

    /** Returns person's surname */
    @Override
    public String getSurname() throws RemoteException {
        return surname;
    }

    /** Returns person's passport nuber */
    @Override
    public long getPassportId() throws RemoteException {
        return passportId;
    }
}
