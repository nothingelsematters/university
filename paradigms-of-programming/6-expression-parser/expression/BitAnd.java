package expression;

public class BitAnd extends AbstractOperations {
    public BitAnd(TripleExpression a, TripleExpression b) {
        super(a, b);
    }
    protected int operationImpl(int a, int b) {
        return a & b;
    }
}
