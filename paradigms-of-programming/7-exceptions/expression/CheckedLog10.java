package expression;

import expression.exceptions.InvalidLogarithmException;

public class CheckedLog10 extends AbstractUnaryOperations {
    public CheckedLog10(TripleExpression in) {
        super(in);
    }
    public int unaryOperationImpl(int a) throws InvalidLogarithmException{
        if (a <= 0) {
            throw new InvalidLogarithmException(a);
        }
        int result;
        for (result = 0; a >= 10; ++result) {
            a /= 10;
        }
        return result;
    }
}
