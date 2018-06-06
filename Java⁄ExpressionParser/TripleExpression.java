package expression;

import expression.exceptions.EvaluatingException;

public interface TripleExpression {
    int evaluate(int x, int y, int z) throws EvaluatingException;
}
