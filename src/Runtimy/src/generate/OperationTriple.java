package generate;

public class OperationTriple {
    final Type firstType;
    final Type secondType;
    final Operation operation;

    public OperationTriple(Type firstType, Operation operation, Type secondType) {
        this.firstType = firstType;
        this.secondType = secondType;
        this.operation = operation;
    }

    public boolean equals(Object o) {
        if(o instanceof OperationTriple) {
            OperationTriple ot = (OperationTriple)o;
            return ot.firstType.equals(firstType) && ot.secondType.equals(secondType) && ot.operation.equals(operation);
        } else {
            return false;
        }
    }

    public int hashCode() {
        int hash = 1;
        hash = 31 * hash + firstType.hashCode();
        hash = 31 * hash + secondType.hashCode();
        hash = 31 * hash + operation.hashCode();
        return hash;
    }

}
