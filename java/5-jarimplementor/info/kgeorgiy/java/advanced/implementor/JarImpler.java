package info.kgeorgiy.java.advanced.implementor;

import java.nio.file.Path;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface JarImpler extends Impler {
    /**
     * Produces <tt>.jar</tt> file implementing class or interface specified by provided <tt>token</tt>.
     * <p>
     * Generated class classes name should be same as classes name of the type token with <tt>Impl</tt> suffix
     * added.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <tt>.jar</tt> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    void implementJar(Class<?> token, Path jarFile) throws ImplerException;
}
