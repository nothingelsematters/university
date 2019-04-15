package ru.ifmo.rain.naumov.crawler;

import info.kgeorgiy.java.advanced.crawler.Result;
import info.kgeorgiy.java.advanced.crawler.Crawler;
import info.kgeorgiy.java.advanced.crawler.Document;
import info.kgeorgiy.java.advanced.crawler.URLUtils;
import info.kgeorgiy.java.advanced.crawler.Downloader;
import info.kgeorgiy.java.advanced.crawler.CachingDownloader;


import java.io.IOException;
import java.lang.Runnable;
import java.lang.IllegalArgumentException;
import java.net.MalformedURLException;
import java.util.Set;
import java.util.Map;
import java.util.Queue;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.concurrent.Phaser;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Crawler interface implementation
 *
 * @author Simon Naumov
 */
public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final ExecutorService downloadService;
    private final ExecutorService extractService;
    private final int perHost;
    private final ConcurrentHashMap<String, HostBarrier> hostLimitation;

    private class HostBarrier {
        private int amount;
        private final Queue<Runnable> scheduled;

        HostBarrier() {
            amount = 0;
            scheduled = new ArrayDeque<>();
        }

        private synchronized void add(final Runnable runnable) {
            if (amount < perHost) {
                ++amount;
                downloadService.submit(runnable);
            } else {
                scheduled.add(runnable);
            }
        }

        private synchronized void next() {
            final Runnable runnable = scheduled.poll();
            if (runnable == null) {
                --amount;
            } else {
                downloadService.submit(runnable);
            }
        }
    }

    /**
     * Creates a new instance of WebCrawler
     * @param downloader     used downloading tool
     * @param downloaders    simultaneous downloading limit
     * @param extractors     simultaneous extracting limit
     * @param perHost        one host simultaneous downloading limit
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        downloadService = Executors.newFixedThreadPool(downloaders);
        extractService = Executors.newFixedThreadPool(extractors);
        this.downloader = downloader;
        this.perHost = perHost;
        hostLimitation = new ConcurrentHashMap<>();
    }

    private void recursionStep(final String url, final int depth, final Set<String> processed, final Map<String, IOException> failed, final Phaser phaser) {
        String host;
        try {
            host = URLUtils.getHost(url);
        } catch(MalformedURLException e) {
            failed.put(url, e);
            return;
        }

        HostBarrier hostLimiter = hostLimitation.computeIfAbsent(host, x -> new HostBarrier());
        phaser.register();
        hostLimiter.add(() -> {
            try {
                final Document document = downloader.download(url);
                if (depth > 1) {
                    phaser.register();
                    extractService.submit(() -> {
                        try {
                            document.extractLinks().stream().filter(processed::add)
                                .forEach(link -> recursionStep(link, depth - 1, processed, failed, phaser));
                        } catch (IOException ignored) {
                            // ...
                        } finally {
                            phaser.arrive();
                        }
                    });
                }
            } catch (IOException e) {
                failed.put(url, e);
            } finally {
                phaser.arrive();
                hostLimiter.next();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Result download(String url, int depth) {
        final Set<String> processed = Collections.newSetFromMap(new HashMap<String, Boolean>());
        final Map<String, IOException> failed = new HashMap<>();

        final Phaser phaser = new Phaser(1);
        processed.add(url);
        recursionStep(url, depth, processed, failed, phaser);
        phaser.arriveAndAwaitAdvance();

        processed.removeAll(failed.keySet());
        return new Result(new ArrayList<>(processed), failed);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        downloadService.shutdown();
        extractService.shutdown();
    }

    /**
     * Allows command line usage:
     * {@code WebCrawler url [depth [downloads [extractors [perHost]]]]}
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        if (args == null || args.length < 1) {
            printUsage();
            return;
        }

        for (String a : args) {
            if (a == null) {
                printUsage();
                return;
            }
        }

        String url = args[0];
        int downloadersQueried = 4;
        int extractorsQueried = 4;
        int perHostQueried = 8;
        int depthQueried = 1;

        try {
            switch (args.length) {
                case 5: perHostQueried = getIntegerArgument(args, 4);
                case 4: extractorsQueried = getIntegerArgument(args, 3);
                case 3: downloadersQueried = getIntegerArgument(args, 2);
                case 2: depthQueried = getIntegerArgument(args, 1);
            }
        } catch(IllegalArgumentException ignored) {
            printUsage();
            return;
        }

        try (Crawler crawler = new WebCrawler(new CachingDownloader(), downloadersQueried, extractorsQueried, perHostQueried)) {
            Result result = crawler.download(url, depthQueried);
            System.out.println(String.format("%d pages downloaded, %d errors occurred", result.getDownloaded().size(), result.getErrors().size()));

        } catch (IOException ignored) {
            System.out.println("An error occurred while downloading");
        }
    }

    private static int getIntegerArgument(String[] args, int pos) {
        int value = Integer.parseInt(args[pos]);
        if (value < 1) {
            throw new IllegalArgumentException();
        }
        return value;
    }

    private static void printUsage() {
        System.out.println("Usage: WebCrawler url [depth [downloads [extractors [perHost]]]]\n" +
            "where depth, downloads, extractors and perHost are integer numbers greater than zero");
    }
}
