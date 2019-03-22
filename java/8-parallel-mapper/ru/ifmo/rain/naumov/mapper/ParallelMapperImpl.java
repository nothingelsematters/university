package ru.ifmo.rain.naumov.mapper;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;
import static ru.ifmo.rain.naumov.concurrent.Utils.*;

import java.util.List;
import java.util.function.Function;
import java.lang.Thread;
import java.lang.Runnable;
import java.lang.InterruptedException;
import java.util.Queue;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;


/**
 * @author Simon Naumov
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> workers;
    private final Queue<Runnable> tasks;
    
    private class ResultList<T> {
        private final List<T> list;
        private int done = 0;
        
        ResultList(int capacity) {
            list = new ArrayList<>(Collections.nCopies(capacity, null));
        }
        
        void set(int index, T element) {
            list.set(index, element);
            synchronized (this) {
                if (++done == list.size()) {
                    notify();
                }
            }
        }
        
        synchronized List<T> getList() throws InterruptedException {
            while (done < list.size()) {
                wait();
            }
            return list;
        }
    }
    
    private void performTask() throws InterruptedException {
        Runnable task;
        synchronized (tasks) {
            while (tasks.isEmpty()) {
                tasks.wait();
            }
            task = tasks.poll();
            tasks.notifyAll();
        }
        task.run();
    }
    
    /**
     * Constructs a new instance of ParallelMapperImpl with the given quantity of threads 
     * @param threads quantity of threads
     */
    public ParallelMapperImpl(int threads) {
        assertThreadQuantity(threads);
        tasks = new ArrayDeque<>();
        workers = new ArrayList<>(threads);
        
        for (int i = 0; i < threads; i++) {
            launchThread(workers, () -> {
                try {
                    while (!Thread.interrupted()) {
                        performTask();
                    }
                } catch (InterruptedException ignored) {
                } finally {
                    Thread.currentThread().interrupt();
                }
            });
        }
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        ResultList<R> resultList = new ResultList<>(args.size());
        
        for (int i = 0; i < args.size(); i++) {
            final int index = i;
            synchronized (tasks) {
                tasks.add(() -> resultList.set(index, f.apply(args.get(index))));
                tasks.notifyAll();
            }
        }

        return resultList.getList();
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        workers.forEach(Thread::interrupt);
        for (Thread worker: workers) {
            try {
                worker.join();
            } catch (InterruptedException ingnored) {}
        }
    }
}