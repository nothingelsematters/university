package expression;

public class Multiply extends AbstractOperations {
      public Multiply(IntegerAndDoubleExpression a, IntegerAndDoubleExpression b){
        super(a, b);
      }
      protected double operationImpl(double a, double b) {
              return a * b;
      }
      protected int operationImpl(int a, int b) {
              return a * b;
      }
}
