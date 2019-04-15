package info.kgeorgiy.java.advanced.crawler;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ReplayDownloader implements Downloader {
    private final ConcurrentMap<String, Page> pages;
    private final ConcurrentMap<String, Boolean> downloaded = new ConcurrentHashMap<>();
    private final AtomicInteger errors = new AtomicInteger();
    private final int depth;
    private final int downloadDelay;
    private final int extractDelay;

    public ReplayDownloader(final String url, final int depth, final int downloadDelay, final int extractDelay) throws IOException {
        pages = load(getFileName(url));
        this.depth = depth;
        this.downloadDelay = downloadDelay;
        this.extractDelay = extractDelay;
    }

    public static String getFileName(final String url) throws MalformedURLException {
        return URLUtils.getHost(url) + ".ser";
    }

    @SuppressWarnings("unchecked")
    private ConcurrentMap<String, Page> load(final String fileName) throws IOException {
        try (ObjectInput os = new ObjectInputStream(new GZIPInputStream(ReplayDownloader.class.getResourceAsStream(fileName)))) {
            try {
                return (ConcurrentMap<String, Page>) os.readObject();
            } catch (final ClassNotFoundException e) {
                throw new AssertionError(e);
            }
        }
    }

    @Override
    public Document download(final String url) throws IOException {
        final Page page = pages.get(url);
        if (page == null) {
            throw new AssertionError("Unknown page " + url);
        }
        if (page.depth >= depth) {
            throw new AssertionError("Page too deep " + url);
        }
        if (downloaded.putIfAbsent(url, true) != null) {
            throw new AssertionError("Duplicate download of " + url);
        }
        if (downloaded.size() % 100 == 0) {
            System.out.format("    %d of %d pages downloaded, %d errors\n", downloaded.size(), pages.size(), errors.get());
        }
        sleep(downloadDelay);
        if (page.exception != null) {
            errors.incrementAndGet();
            throw page.exception;
        }
        return () -> {
            sleep(extractDelay);
            return page.links;
        };
    }

    private void sleep(final int max) {
        try {
            Thread.sleep(ThreadLocalRandom.current().nextInt(max) + 1);
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public Result expected(final int depth) {
        return new Result(
                getEntryStream(depth)
                        .filter(e -> e.getValue().exception == null)
                        .map(Map.Entry::getKey)
                        .collect(Collectors.toList()),
                getEntryStream(depth)
                        .filter(e -> e.getValue().exception != null)
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().exception))
        );
    }

    private Stream<Map.Entry<String, Page>> getEntryStream(final int depth) {
        return pages.entrySet().stream()
        .filter(e -> e.getValue().depth < depth);
    }

    public static class Page implements Serializable {
        public final int depth;
        public final List<String> links;
        public final IOException exception;

        public Page(final int depth, final List<String> links, final IOException exception) {
            this.depth = depth;
            this.links = links;
            this.exception = exception;
        }
    }
}
