package expression;

public class BitXor extends AbstractOperations {
    public BitXor(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) {
        return a ^ b;
    }
}
