package runtimy;

public class Variable {
    private Value value;

    public Variable(Value value) {
        this.value = value;
    }

    public Variable() {}

    public Value get() {
        return value;
    }

    public void set(Value value) {
        this.value = value;
    }

}
