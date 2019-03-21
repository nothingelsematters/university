package info.kgeorgiy.java.advanced.concurrent;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * List iterative parallelism support.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface ListIP extends ScalarIP {
    /**
     * Join values to string.
     *
     * @param threads number or concurrent threads.
     * @param values values to join.
     *
     * @return list of joined result of {@link #toString()} call on each value.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    String join(int threads, List<?> values) throws InterruptedException;

    /**
     * Filters values by predicate.
     *
     * @param threads number or concurrent threads.
     * @param values values to filter.
     * @param predicate filter predicate.
     *
     * @return list of values satisfying given predicated. Order of values is preserved.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException;

    /**
     * Mas values.
     *
     * @param threads number or concurrent threads.
     * @param values values to filter.
     * @param f mapper function.
     *
     * @return list of values mapped by given function.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f) throws InterruptedException;
}
