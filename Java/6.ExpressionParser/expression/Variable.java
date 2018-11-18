package expression;

public class Variable implements TripleExpression {
    private char value;
    public Variable() {}
    public Variable(char s) {
        value = s;
    }
    public int evaluate(int x, int y, int z) {
        switch (value) {
            case 'x':
                return x;
            case 'y':
                return y;
            case 'z':
                return z;
            default:
                throw new IllegalArgumentException();
        }
    }
}
