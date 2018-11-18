package expression;

public class Variable implements IntegerAndDoubleExpression {
      Variable(){};
      public Variable(String s){};
      public double evaluate(double x){
            return x;
      }
      public int evaluate(int x){
            return x;
      }
}
