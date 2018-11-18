package expression.parser;

import expression.*;
import expression.exceptions.ParsingException;
import expression.exceptions.WrongSymbolException;

public class ExpressionParser implements Parser {
    private Tokenizer key;

    private TripleExpression negatePowLog10Counter() throws ParsingException {
        TripleExpression temp;
        switch (key.currentState) {
            case Const:
                temp = new Const(key.numberValue);
                break;
            case Variable:
                temp = new Variable(key.variableName);
                break;
            case OpenBracket:
                key.nextToken();
                temp = parsingExpression();
                if (key.currentState != Token.CloseBracket) {
                    throw new WrongSymbolException("the end of the expression", "close bracket",
                            key.stringToParse.substring(key.stringToParse.lastIndexOf('('),
                                    key.stringToParse.length()));
                }
                break;
            case CloseBracket:
                throw new WrongSymbolException("close bracket", "argument",
                        key.stringToParse.substring(Math.max(key.stringPosition - 5, 0),
                                Math.min(key.stringPosition + 5, key.stringToParse.length())));
            default:
                throw new WrongSymbolException("operation", "argument",
                        key.stringToParse.substring(Math.max(key.stringPosition - 5, 0),
                                Math.min(key.stringPosition + 5, key.stringToParse.length())));
        }
        key.nextToken();
        return temp;

    }

    private TripleExpression multiplyAndDivideCounter() throws ParsingException {
        Token perfectState = key.currentState;
        switch (perfectState) {
            case Log10:
                key.nextToken();
                return new CheckedLog10(multiplyAndDivideCounter());
            case Pow10:
                key.nextToken();
                return new CheckedPow10(multiplyAndDivideCounter());
            case Subtract:
                key.nextToken();
                return new CheckedNegate(multiplyAndDivideCounter());
            default:
                return negatePowLog10Counter();
        }
    }

    private TripleExpression addAndSubtractCounter() throws ParsingException {
        TripleExpression part = multiplyAndDivideCounter();
        while ((key.currentState == Token.Multiply) || (key.currentState == Token.Divide)) {
            Token perfectState = key.currentState;
            key.nextToken();
            TripleExpression acting = multiplyAndDivideCounter();
            switch (perfectState) {
                case Multiply:
                    part = new CheckedMultiply(part, acting);
                    break;
                case Divide:
                    part = new CheckedDivide(part, acting);
            }
        }
        return part;
    }

    private TripleExpression parsingExpression() throws ParsingException {
        TripleExpression part = addAndSubtractCounter();
        while ((key.currentState == Token.Add) || (key.currentState == Token.Subtract)) {
            Token perfectState = key.currentState;
            key.nextToken();
            TripleExpression acting = addAndSubtractCounter();
            switch (perfectState) {
                case Add:
                    part = new CheckedAdd(part, acting);
                    break;
                case Subtract:
                    part = new CheckedSubtract(part, acting);
            }
        }
        return part;
    }

    public TripleExpression parse(String expression) throws ParsingException {
        key = new Tokenizer(expression);
        key.nextToken();
        TripleExpression result = parsingExpression();
        if (key.currentState != Token.End) {
            throw new WrongSymbolException("close bracket", "open bracket with it",
                    key.stringToParse.substring(Math.max(key.stringToParse.lastIndexOf('('), 0),
                           Math.min(key.stringToParse.length(), key.stringPosition + 5)));
        }
        return result;
    }
}
