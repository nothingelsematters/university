package expression;

import expression.exceptions.OverflowException;

public class CheckedNegate extends AbstractUnaryOperations {
    public CheckedNegate(TripleExpression in) {
        super(in);
    }
    public int unaryOperationImpl(int a) throws OverflowException {
        if (a == Integer.MIN_VALUE) {
            throw new OverflowException("-" + a);
        }
        return -a;
    }
}
