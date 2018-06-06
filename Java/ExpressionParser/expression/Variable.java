package expression;

public class Variable implements TripleExpression {
    private String value;
    public Variable() {}
    public Variable(String s) {
        value = s;
    }
    public int evaluate(int x, int y, int z) {
        switch (value) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new IllegalArgumentException();
        }
    }
}
