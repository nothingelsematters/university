package expression.exceptions;

public class WrongSymbolException extends ExpressionException {
    public WrongSymbolException(char c, String s) {
        super("Wrong symbol '" + c + '\'' + " found at : \"" + s + '\"');
    }
    public WrongSymbolException(String s, String d, String e) {
        super("Unexpected symbol: found " + s + ", required " + d + " at : \"" + e + "\"");
    }
}
