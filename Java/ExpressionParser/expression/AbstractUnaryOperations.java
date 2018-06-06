package expression;

import expression.exceptions.EvaluatingException;

public abstract class AbstractUnaryOperations implements TripleExpression {
    public TripleExpression operand;

    public AbstractUnaryOperations(TripleExpression in) {
        operand = in;
    }
    public int evaluate(int x, int y, int z) throws EvaluatingException {
        return unaryOperationImpl(operand.evaluate(x, y, z));
    }
    protected abstract int unaryOperationImpl(int a) throws EvaluatingException;
}
