package expression;

public class Denial implements TripleExpression {
    private TripleExpression value;
    public Denial(TripleExpression v) {
        value = v;
    }
    public int evaluate(int x, int y, int z) {
        return -value.evaluate(x, y, z);
    }
}
