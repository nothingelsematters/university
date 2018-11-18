package expression;

import expression.exceptions.OverflowException;

public class CheckedPow10 extends AbstractUnaryOperations {
    public CheckedPow10(TripleExpression in) {
        super(in);
    }
    public int unaryOperationImpl(int a) throws OverflowException{
        if ((a < 0) || (a > 9)) {
                throw new OverflowException("pow10 " + a);
        }
        int result = 1;
        for (int i = 0; i < a; ++i) {
            result *= 10;
        }
        return result;
    }
}
