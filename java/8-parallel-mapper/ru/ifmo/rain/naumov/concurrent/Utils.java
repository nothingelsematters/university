package ru.ifmo.rain.naumov.concurrent;

import java.util.List;
import java.lang.Thread;
import java.lang.Runnable;
import java.lang.InterruptedException;

public class Utils {
    public static void assertThreadQuantity(int threads) throws IllegalArgumentException {
        if (threads <= 0) {
            throw new IllegalArgumentException("Threads quantity must be greater than 0");
        }
    }
    
    public static void launchThread(List<Thread> threads, Runnable task) {
        Thread thread = new Thread(task);
        threads.add(thread);
        thread.start();
    }
}