package expression.parser;

import expression.TripleExpression;
import expression.exceptions.ParsingException;

public interface Parser {
    TripleExpression parse(String expression) throws ParsingException;
}
