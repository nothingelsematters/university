package expression;

import expression.exceptions.OverflowException;

public class CheckedAdd extends AbstractOperations {
    public CheckedAdd(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) throws OverflowException {
        if (b > 0 ? a > Integer.MAX_VALUE - b : a < Integer.MIN_VALUE - b) {
            throw new OverflowException(a + " + " + b);
        }
        return a + b;
    }
}
