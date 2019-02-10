package expression;

import expression.exceptions.OverflowException;

public class CheckedSubtract extends AbstractOperations {
    public CheckedSubtract(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) throws OverflowException {
        if (b > 0 ? a < Integer.MIN_VALUE + b : a > Integer.MAX_VALUE + b) {
            throw new OverflowException(a + " - " + b);
        }
        return a - b;
    }
}
