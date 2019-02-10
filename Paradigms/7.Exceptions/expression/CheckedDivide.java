package expression;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.EvaluatingException;
import expression.exceptions.OverflowException;

public class CheckedDivide extends AbstractOperations {
    public CheckedDivide(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operationImpl(int a, int b) throws EvaluatingException {
        if (b == 0) {
            throw new DivisionByZeroException(a);
        }
        if (a == Integer.MIN_VALUE && b == -1) {
            throw new OverflowException(a + " / " + b);
        }
        return a / b;
    }
}
