package expression.exceptions;

public class DivisionByZeroException extends EvaluatingException {
        public DivisionByZeroException(int a) {
                super("Division by zero : " + a + " / 0");
        }
}
