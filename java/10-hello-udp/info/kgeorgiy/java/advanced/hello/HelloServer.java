package info.kgeorgiy.java.advanced.hello;

/**
 * Test interface for {@link HelloServerTest}.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface HelloServer extends AutoCloseable {
    /**
     * Starts a new Hello server.
     *
     * @param port server port.
     * @param threads number of working threads.
     */
    void start(int port, int threads);

    /**
     * Stops server and deallocates all resources.
     */
    @Override
    void close();
}
