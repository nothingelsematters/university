package expression;

public class Const implements IntegerAndDoubleExpression {
      private Number value;
      public Const(double v) {
            value = v;
      }
      public Const(int v) {
            value = v;
      }
      public double evaluate(double x) {
            return value.doubleValue();
      }
      public int evaluate(int x) {
            return value.intValue();
      }
}
