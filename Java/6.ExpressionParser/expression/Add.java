package expression;

public class Add extends AbstractOperations {
      public Add(TripleExpression a, TripleExpression b){
          super(a, b);
      }
      protected int operationImpl(int a, int b) {
              return a + b;
      }
}
