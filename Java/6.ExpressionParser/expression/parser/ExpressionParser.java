package expression.parser;

import expression.*;

public class ExpressionParser implements Parser {
    private int stringPosition;
    private int numberValue;
    private char variableName;
    private Tokens currentState;
    private String stringToParse;

    private void nextPart() {
        Tokens perfectState = currentState;
        while ((stringPosition < stringToParse.length()) &&
                ((stringToParse.charAt(stringPosition) == ' ') ||
                        (stringToParse.charAt(stringPosition) == '\t'))) {
            ++stringPosition;
        }
        if (stringPosition >= stringToParse.length()) {
            currentState = Tokens.End;
            return;
        }
        switch (stringToParse.charAt(stringPosition)) {
            case '+':
                currentState = Tokens.Add;
                break;
            case '*':
                currentState = Tokens.Multiply;
                break;
            case '/':
                currentState = Tokens.Divide;
                break;
            case '&':
                currentState = Tokens.BitAnd;
                break;
            case '^':
                currentState = Tokens.BitXor;
                break;
            case '|':
                currentState = Tokens.BitOr;
                break;
            case '(':
                currentState = Tokens.OpenBracket;
                break;
            case ')':
                currentState = Tokens.CloseBracket;
                break;
            case '-':
                currentState = Tokens.Subtract;
        }
        if (Character.isAlphabetic(stringToParse.charAt(stringPosition))) {
            variableName = stringToParse.charAt(stringPosition);
            currentState = Tokens.Variable;
        } else if ((Character.isDigit(stringToParse.charAt(stringPosition))) ||
                ((currentState == Tokens.Subtract) && !((perfectState == Tokens.Variable) || (perfectState == Tokens.Const)) &&
                        (Character.isDigit(stringToParse.charAt(stringPosition + 1))))) {
            int numberBeginning = stringPosition++;
            while ((stringPosition < stringToParse.length()) &&
                    (Character.isDigit(stringToParse.charAt(stringPosition)))) {
                ++stringPosition;
            }
            numberValue = Integer.parseInt(stringToParse.substring(numberBeginning, stringPosition));
            currentState = Tokens.Const;
            return;
        }
        ++stringPosition;
    }

    private TripleExpression singleMinusCounter() {
        TripleExpression temp;
        switch (currentState) {
            case Const:
                temp = new Const(numberValue);
                break;
            case Variable:
                temp = new Variable(variableName);
                break;
            case OpenBracket:
                nextPart();
                temp = parsingExpression();
                break;
            default:
                throw new IllegalArgumentException();
        }
        nextPart();
        return temp;
    }

    private TripleExpression multiplyAndDivideCounter() {
        if (currentState == Tokens.Subtract) {
            nextPart();
            return new Denial(multiplyAndDivideCounter());
        } else {
            return singleMinusCounter();
        }
    }

    private TripleExpression addAndSubtractCounter() {
        TripleExpression part = multiplyAndDivideCounter();
        while ((currentState == Tokens.Multiply) || (currentState == Tokens.Divide)) {
            Tokens perfectState = currentState;
            nextPart();
            TripleExpression acting = multiplyAndDivideCounter();
            part = ((perfectState == Tokens.Multiply) ? new Multiply(part, acting) : new Divide(part, acting));
        }
        return part;
    }

    private TripleExpression bitAndCounter() {
        TripleExpression part = addAndSubtractCounter();
        while ((currentState == Tokens.Add) || (currentState == Tokens.Subtract)) {
            Tokens perfectState = currentState;
            nextPart();
            TripleExpression acting = addAndSubtractCounter();
            part = ((perfectState == Tokens.Add) ? new Add(part, acting) : new Subtract(part, acting));
        }
        return part;
    }

    private TripleExpression bitXorCounter() {
        TripleExpression part = bitAndCounter();
        while (currentState == Tokens.BitAnd) {
            nextPart();
            part = new BitAnd(part, bitAndCounter());
        }
        return part;
    }

    private TripleExpression bitOrCounter() {
        TripleExpression part = bitXorCounter();
        while (currentState == Tokens.BitXor) {
            nextPart();
            part = new BitXor(part, bitXorCounter());
        }
        return part;
    }

    private TripleExpression parsingExpression() {
        TripleExpression part = bitOrCounter();
        while (currentState == Tokens.BitOr) {
            nextPart();
            part = new BitOr(part, bitOrCounter());
        }
        return part;
    }

    public TripleExpression parse(String expression) {
        stringPosition = 0;
        stringToParse = expression;
        nextPart();
        return parsingExpression();
    }
}
