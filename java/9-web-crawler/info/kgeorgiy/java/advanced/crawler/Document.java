package info.kgeorgiy.java.advanced.crawler;

import java.io.IOException;
import java.util.List;

/**
 * Downloaded document.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
*/
public interface Document {
    /**
     * Extracts links from downloaded document. All extracted links are absolute URLs.
     *
     * @return list of extracted links or empty list, if this document is not a valid HTML page.
     *
     * @throws IOException if an error occurred.
     */
    List<String> extractLinks() throws IOException;
}
