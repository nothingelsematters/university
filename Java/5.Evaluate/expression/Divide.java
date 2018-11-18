package expression;
import java.lang.ArithmeticException;

public class Divide extends AbstractOperations {
      public Divide(IntegerAndDoubleExpression a, IntegerAndDoubleExpression b){
          super(a, b);
      }
      protected double operationImpl(double a, double b) {
              return a / b;
      }
      protected int operationImpl(int a, int b) {
              if (b == 0) {
                  throw new ArithmeticException();
              }
              return a / b;
      }
}
