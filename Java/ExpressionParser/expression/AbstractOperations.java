package expression;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.EvaluatingException;
import expression.exceptions.OverflowException;

public abstract class AbstractOperations implements TripleExpression {
    public TripleExpression l, r;

    public AbstractOperations(TripleExpression a, TripleExpression b) {
        l = a;
        r = b;
    }
    public int evaluate(int x, int y, int z) throws EvaluatingException {
        return operationImpl(l.evaluate(x, y, z), r.evaluate(x, y, z));
    }
    protected abstract int operationImpl(int a, int b) throws EvaluatingException;
}
