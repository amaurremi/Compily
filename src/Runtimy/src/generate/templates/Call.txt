package runtimy;

public abstract class Call {

    final String name;

    protected Call(String name) {
        this.name = name;
    }

    public abstract Value apply(Value... params);

    public final void checkParams(int n, Value... params) {
        if (params.length != n) {
            throw new MRTError("Call to " + name + ": parameter count mismatch: actual " + params.length + " != formal " + n);
        }
    }
}
