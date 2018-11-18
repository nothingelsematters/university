package expression.exceptions;

public class InvalidLogarithmException extends EvaluatingException {
    public InvalidLogarithmException(int a) {
        super("Invalid Logarithm : log10 (" + a + ')');
    }
}
