package expression;

import expression.exceptions.OverflowException;

public class CheckedMultiply extends AbstractOperations {
    public CheckedMultiply(TripleExpression a, TripleExpression b) {
        super(a, b);
    }
    protected int operationImpl(int a, int b) throws  OverflowException {
        if ((b != 0) && ((a > 0 && b > 0 && a > Integer.MAX_VALUE / b) ||
                (a < 0 && b < 0 && a < Integer.MAX_VALUE / b) ||
                (a > 0 && b < 0 && b < Integer.MIN_VALUE / a) ||
                (a < 0 && b > 0 && a < Integer.MIN_VALUE / b))) {
            throw new OverflowException(a + " * " + b);
        }
        return a * b;
    }
}
