package expression.parser;

import expression.exceptions.*;

public class Tokenizer {
    public int stringPosition;
    public final String stringToParse;
    public int numberValue;
    public String variableName;
    public Token currentState;

    public Tokenizer(String expression) {
        stringToParse = expression;
        stringPosition = 0;
    }

    private void checkAlphabetOperation(String oper) throws WrongSymbolException{
        if ((stringToParse.length() < stringPosition + 5) ||
                (!stringToParse.substring(stringPosition, stringPosition + 5).equals(oper))) {
            throw new WrongSymbolException(stringToParse.charAt(stringPosition),
                    stringToParse.substring(Math.max(stringPosition - 5, 0),
                            Math.min(stringPosition + 5, stringToParse.length())));
        };
        stringPosition += 4;
    }

    public void nextToken() throws WrongSymbolException, OverflowException {
        Token perfectState = currentState;
        while ((stringPosition < stringToParse.length()) &&
                ((stringToParse.charAt(stringPosition) == ' ') ||
                        (stringToParse.charAt(stringPosition) == '\t'))) {
            ++stringPosition;
        }
        if (stringPosition >= stringToParse.length()) {
            currentState = Token.End;
            return;
        } else if (Character.isAlphabetic(stringToParse.charAt(stringPosition))) {
            switch (stringToParse.charAt(stringPosition)) {
                case 'x':
                case 'y':
                case 'z':
                    variableName = stringToParse.substring(stringPosition, stringPosition + 1);
                    currentState = Token.Variable;
                    break;
                case 'l':
                    checkAlphabetOperation("log10");
                    currentState = Token.Log10;
                    break;
                case 'p':
                    checkAlphabetOperation("pow10");
                    currentState = Token.Pow10;
                    break;
                default :
                    throw new WrongSymbolException(stringToParse.charAt(stringPosition),
                            stringToParse.substring(Math.max(stringPosition - 5, 0),
                                    Math.min(stringPosition + 5, stringToParse.length())));
            }
            ++stringPosition;
            return;
        } else if ((Character.isDigit(stringToParse.charAt(stringPosition))) ||
                ((stringToParse.charAt(stringPosition) == '-') && !((perfectState == Token.Variable) ||
                        (perfectState == Token.Const) || (perfectState == Token.CloseBracket)) &&
                        (Character.isDigit(stringToParse.charAt(stringPosition + 1))))) {
            int numberBeginning = stringPosition++;
            while ((stringPosition < stringToParse.length()) &&
                    (Character.isDigit(stringToParse.charAt(stringPosition)))) {
                ++stringPosition;
            }
            try {
                numberValue = Integer.parseInt(stringToParse.substring(numberBeginning, stringPosition));
            } catch (NumberFormatException e) {
                throw new OverflowException("read number : " + stringToParse.substring(numberBeginning, stringPosition));
            }
            currentState = Token.Const;
            return;
        }
        switch (stringToParse.charAt(stringPosition)) {
            case '+':
                currentState = Token.Add;
                break;
            case '*':
                currentState = Token.Multiply;
                break;
            case '/':
                currentState = Token.Divide;
                break;
            case '(':
                currentState = Token.OpenBracket;
                break;
            case ')':
                currentState = Token.CloseBracket;
                break;
            case '-':
                currentState = Token.Subtract;
                break;
            default:
                throw new WrongSymbolException(stringToParse.charAt(stringPosition),
                        stringToParse.substring(Math.max(stringPosition - 5, 0),
                                Math.min(stringPosition + 5, stringToParse.length())));
        }
        ++stringPosition;
    }
}
