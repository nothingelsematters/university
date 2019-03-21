package expression.parser;

import java.util.Arrays;

/**
 * @author Georgiy Korneev
 */
public class ParserBitwiseTest extends ParserTest {
    protected ParserBitwiseTest() {
        levels.add(0, list(op("&", (a, b) -> a & b)));
        levels.add(0, list(op("^", (a, b) -> a ^ b)));
        levels.add(0, list(op("|", (a, b) -> a | b)));

        tests.addAll(Arrays.asList(
                op("6 & 1 + 2", (x, y, z) -> 2L),
                op("6 ^ 1 + 2", (x, y, z) -> 5L),
                op("6 | 1 + 2", (x, y, z) -> 7L),
                op("x & y", (x, y, z) -> x & y),
                op("x | y", (x, y, z) -> x | y),
                op("x ^ y", (x, y, z) -> x ^ y),
                op("x | z & y", (x, y, z) -> x | z & y),
                op("x ^ z & y", (x, y, z) -> x ^ z & y),
                op("x | z ^ y", (x, y, z) -> x | z ^ y),
                op("x & y + z", (x, y, z) -> x & y + z),
                op("x ^ y - z", (x, y, z) -> x ^ y - z),
                op("x | y + z", (x, y, z) -> x | y + z),
                op("(- - - x^1883669513)|(- x^1681810605)", (x, y, z) -> (- - - x^1883669513)|(- x^1681810605))
        ));
    }

    public static void main(final String[] args) {
        new ParserBitwiseTest().run();
    }
}
