package expression;

public class Multiply extends AbstractOperations {
    public Multiply(TripleExpression a, TripleExpression b) {
        super(a, b);
    }
    protected int operationImpl(int a, int b) {
        return a * b;
    }
}
