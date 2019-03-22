package info.kgeorgiy.java.advanced.mapper;

import java.util.List;
import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface ParallelMapper extends AutoCloseable {
    /**
     * Maps function {@code f} over specified {@code args}.
     * Mapping for each element performs in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException;

    /** Stops all threads. All unfinished mappings leave in undefined state. */
    @Override
    void close();
}
