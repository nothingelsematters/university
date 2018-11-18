package expression;

public class Add extends AbstractOperations {
      public Add(IntegerAndDoubleExpression a, IntegerAndDoubleExpression b){
          super(a, b);
      }
      protected double operationImpl(double a, double b) {
              return a + b;
      }
      protected int operationImpl(int a, int b) {
              return a + b;
      }
}
