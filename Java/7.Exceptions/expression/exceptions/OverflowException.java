package expression.exceptions;

public class OverflowException extends EvaluatingException {
    public OverflowException(String message) {
        super("Overflow trying: " + message);
    }
}
