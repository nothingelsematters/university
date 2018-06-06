package expression;

import base.TestCounter;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract strictfp class BaseTest {
    public final Random random = new Random(7240958270458L);

    protected TestCounter counter = new TestCounter();

    protected BaseTest() {
        Locale.setDefault(Locale.US);

        checkAssert(getClass());
    }

    public void assertTrue(final String message, final boolean condition) {
        assert condition : message;
    }

    public void assertEquals(final String message, final int actual, final int expected) {
        assertTrue(String.format("%s: Expected %d, found %d", message, expected, actual), actual == expected);
    }

    public void assertEquals(final String message, final Object actual, final Object expected) {
        assertTrue(String.format("%s: Expected \"%s\", found \"%s\"", message, expected, actual), actual != null && actual.equals(expected) || expected == null);
    }

    public void assertEquals(final String message, final double precision, final double actual, final double expected) {
        assertTrue(
                String.format("%s: Expected %.12f, found %.12f", message, expected, actual),
                Math.abs(actual - expected) < precision ||
                        Math.abs(actual - expected) < precision * Math.abs(actual) ||
                        (Double.isNaN(actual) || Double.isInfinite(actual)) &&
                        (Double.isNaN(expected) || Double.isInfinite(expected))
        );
    }

    private void checkAssert(final Class<?> c) {
        if (!c.desiredAssertionStatus()) {
            throw new AssertionError("You should enable assertions by running 'java -ea " + c.getName() + "'");
        }
    }

    public static String repeat(final String s, final int n) {
        return Stream.generate(() -> s).limit(n).collect(Collectors.joining());
    }

    public <T> T random(final List<T> variants) {
        return variants.get(random.nextInt(variants.size()));
    }

    @SafeVarargs
    public final <T> T random(final T... variants) {
        return random(Arrays.asList(variants));
    }

    @SafeVarargs
    public static <T> T random(final Random random, final T... variants) {
        return random(random, Arrays.asList(variants));
    }

    public static <T> T random(final Random random, final List<T> variants) {
        return variants.get(random.nextInt(variants.size()));
    }

    public int randomInt(final int n) {
        return random.nextInt(n);
    }

    public void run() {
        test();
        counter.printStatus(getClass());
    }

    protected abstract void test();

    @SafeVarargs
    public static <T> List<T> list(final T... items) {
        return new ArrayList<>(Arrays.asList(items));
    }

    public static void addRange(final List<Integer> values, final int d, final int c) {
        for (int i = -d; i <= d; i++) {
            values.add(c + i);
        }
    }

    public static final class Op<T> {
        public final String name;
        public final T f;

        private Op(final String name, final T f) {
            this.name = name;
            this.f = f;
        }
    }

    public static <T> Op<T> op(final String name, final T f) {
        return new Op<>(name, f);
    }
}
