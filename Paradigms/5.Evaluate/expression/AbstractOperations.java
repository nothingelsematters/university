package expression;

public abstract class AbstractOperations implements IntegerAndDoubleExpression {
      public IntegerAndDoubleExpression l, r;
      public AbstractOperations(){}
      public AbstractOperations (IntegerAndDoubleExpression a, IntegerAndDoubleExpression b) {
            l = a;
            r = b;
      }
      public double evaluate (double x) {
            return operationImpl (l.evaluate(x), r.evaluate(x));
      }
      protected abstract double operationImpl(double a, double b);
      public int evaluate (int x) {
            return operationImpl (l.evaluate(x), r.evaluate(x));
      }
      protected abstract int operationImpl(int a, int b);
}
