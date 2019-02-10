package expression;

public class BitOr extends AbstractOperations {
    public BitOr(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) {
        return a | b;
    }
}
