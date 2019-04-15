package info.kgeorgiy.java.advanced.crawler;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class CheckingDownloader implements Downloader {
    public final Downloader downloader;
    private final AtomicInteger downloaders;
    private final AtomicInteger extractors;
    private final AtomicReference<String> error = new AtomicReference<>();
    private final ConcurrentMap<String, AtomicInteger> hosts = new ConcurrentHashMap<>();
    private final int perHost;
    private final ConcurrentMap<String, String> downloaded = new ConcurrentHashMap<>();

    public CheckingDownloader(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.downloaders = new AtomicInteger(downloaders);
        this.extractors = new AtomicInteger(extractors);
        this.perHost = perHost;
    }

    @Override
    public Document download(final String url) throws IOException {
        if (downloaded.putIfAbsent(url, url) != null) {
            error("Duplicate download of " + url);
        }
        final Document document = check(
                getHost(url),
                "Too many downloaders for " + URLUtils.getHost(url),
                () -> check(downloaders, "Too many downloaders", () -> downloader.download(url))
        );
        return () -> check(extractors, "Too many extractors", document::extractLinks);
    }

    private AtomicInteger getHost(final String url) throws MalformedURLException {
        final String host = URLUtils.getHost(url);
        final AtomicInteger existing = hosts.get(host);
        if (existing != null) {
            return existing;
        }
        final AtomicInteger created = new AtomicInteger(perHost);
        final AtomicInteger found = hosts.putIfAbsent(host, created);
        return found == null ? created : found;
    }

    private <T> T check(final AtomicInteger permissions, final String message, final Callable<T> callable) throws IOException {
        final String existingError = error.get();
        if (existingError != null) {
            throw new AssertionError(existingError);
        }
        if (permissions.decrementAndGet() < 0) {
            error(message);
        }
        try {
            return callable.call();
        } catch (final RuntimeException e) {
            throw e;
        } catch (final Exception e) {
            throw (IOException) e;
        } finally {
            permissions.incrementAndGet();
        }
    }

    private void error(final String message) {
        error.set(message);
        throw new AssertionError(message);
    }

    public String getError() {
        return error.get();
    }
}
