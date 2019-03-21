package expression;

public class Subtract extends AbstractOperations {
      public Subtract(TripleExpression a, TripleExpression b){
          super(a, b);
      }
      protected int operationImpl(int a, int b) {
              return a - b;
      }
}
