package info.kgeorgiy.java.advanced.crawler;

import java.io.IOException;

/**
 * Downloads {@link info.kgeorgiy.java.advanced.crawler.Document documents}.
 *
 * @see CachingDownloader
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Downloader {
    /**
     * Downloads {@link info.kgeorgiy.java.advanced.crawler.Document} by
     * <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     *
     * @param url URL to download.
     * @return downloaded document.
     * @throws IOException if an error occurred.
     */
    Document download(final String url) throws IOException;
}
