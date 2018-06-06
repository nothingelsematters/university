package expression.exceptions;

import java.util.Arrays;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ExceptionsPowLog10Test extends ExceptionsTest {
    public static final Reason NEG_LOG = new Reason("Logarithm of negative value");

    protected ExceptionsPowLog10Test() {
        unary.add(op("log10", this::log10));
        unary.add(op("pow10", this::pow10));

        tests.addAll(Arrays.asList(
                op("log10 11", (x, y, z) -> 1),
                op("log10 -4", (x, y, z) -> error(NEG_LOG)),
                op("pow10 4", (x, y, z) -> 10_000),
                op("pow10 8", (x, y, z) -> 100_000_000),
                op("pow10 x * y", (x, y, z) -> pow10(x) * y),
                op("pow10(x * y * z)", (x, y, z) -> pow10(x * y * z))
        ));
        parsingTest.addAll(Arrays.asList(
                op("hello", "hello"),
                op("log10", "log10"),
                op("log10()", "log10()"),
                op("log10(1, 2)", "log10(1, 2)"),
                op("log(1)", "log(1)")
        ));
    }

    private long pow10(final long a) {
        return 0 <= a && a <= 9 ? (long) Math.pow(10, a) : error(OVERFLOW);
    }

    private long log10(final long a) {
        return a <= 0 ? error(NEG_LOG) : (long) Math.log10(a);
    }

    public static void main(final String[] args) {
        new ExceptionsPowLog10Test().run();
    }
}