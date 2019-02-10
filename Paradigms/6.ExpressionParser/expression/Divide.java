package expression;

public class Divide extends AbstractOperations {
    public Divide(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) {
        if (b == 0){
            throw new IllegalArgumentException();
        }
        return a / b;
    }
}
