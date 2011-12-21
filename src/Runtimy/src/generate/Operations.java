package generate;

import java.util.ArrayList;
import java.util.List;

public enum Operations {
    ARITHMETIC(Operation.SUBTRACT, Operation.MULTIPLY, Operation.DIVIDE),
    BOOLEAN_NUMERAL(Operation.NOT_EQ, Operation.GEQ, Operation.LEQ, Operation.GE, Operation.LE),
    BOOLEAN_STRING(Operation.NOT_EQ),
    BOOLEAN(Operation.NOT_EQ, Operation.AND, Operation.OR),
    ADD(Operation.ADD);

    final Operation[] operations;

    Operations(Operation... operations) {
        this.operations = operations;
    }

    static List<Operation> allExceptFunctionObjectOperations() {
        ArrayList<Operation> operations = Operation.listValues();
        operations.remove(Operation.APPLY);
        operations.remove(Operation.GET_VALUE);
        operations.remove(Operation.PUT_VALUE);
        operations.remove(Operation.EQ);
        operations.remove(Operation.SET);
        operations.remove(Operation.GET);
        operations.remove(Operation.IS_TRUE);
        return operations;
    }

}
