package ru.ifmo.rain.naumov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;

import java.util.List;
import java.util.ArrayList;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.Comparator;
import java.lang.Math;
import java.lang.Thread;
import java.lang.InterruptedException;
import java.lang.IllegalArgumentException;
import java.util.Collections;
import java.util.stream.Stream;
import java.util.stream.Collectors;


public class IterativeParallelism implements ListIP {
    private <T, R> R performTask(int threads, List<? extends T> values,
                                 final Function<Stream<? extends T>, ? extends R> task,
                                 final Function<Stream<? extends R>, ? extends R> joinResults) throws InterruptedException {
        if (threads <= 0) {
            throw new IllegalArgumentException("Threads quantity must be greater than 0");
        }
        
        int realThreads = Math.max(1, Math.min(values.size(), threads));
        int part = values.size() / realThreads;
        int rest = values.size() % realThreads;
        
        final List<Stream<? extends T>> taskDistribution = new ArrayList<>();
        for (int i = 0, upperBound = 0; i < realThreads; i++) {
            int lowerBound = upperBound;
            upperBound += part + (rest-- >= 1 ? 1 : 0);
            taskDistribution.add(values.subList(lowerBound, upperBound).stream());
        }
        
        List<Thread> workers = new ArrayList<>();
        final List<R> results = new ArrayList<>(Collections.nCopies(realThreads, null));
        for (int i = 0; i < realThreads; i++) {
            final int copy = i;
            workers.add(new Thread(() -> results.set(copy, task.apply(taskDistribution.get(copy)))));
            workers.get(i).start();
        }
        
        for (int i = 0; i < realThreads; i++) {
            workers.get(i).join();
        }
        return joinResults.apply(results.stream());
    }
    
    private <T> void nonEmptyAsert(List<? extends T> values) {
        if (values.size() == 0) {
            throw new IllegalArgumentException("Can't perform an action on an empty list");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        nonEmptyAsert(values);
        return performTask(threads, values, stream -> stream.max(comparator).get(), stream -> stream.max(comparator).get());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return performTask(threads, values, stream -> stream.allMatch(predicate), 
                           stream -> stream.allMatch(Boolean::booleanValue));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !all(threads, values, Predicate.not(predicate));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return performTask(threads, values, stream -> stream.map(Object::toString).collect(Collectors.joining()), 
                stream -> stream.collect(Collectors.joining()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> List<T> filter(final int threads, final List<? extends T> values,
                              final Predicate<? super T> predicate) throws InterruptedException {
        return performTask(threads, values, stream -> stream.filter(predicate).collect(Collectors.toList()), 
                stream -> stream.flatMap(List::stream).collect(Collectors.toList()));

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T, U> List<U> map(final int threads, final List<? extends T> values,
                              final Function<? super T, ? extends U> function) throws InterruptedException {
        return performTask(threads, values, stream -> stream.map(function).collect(Collectors.toList()), 
                stream -> stream.flatMap(List::stream).collect(Collectors.toList()));
    }


}
