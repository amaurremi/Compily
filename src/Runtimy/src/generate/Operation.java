package generate;

import java.util.ArrayList;
import java.util.Arrays;

public enum Operation {
    ADD("add", "+"),
    SUBTRACT("subtract", "-"),
    MULTIPLY("multiply", "*"),
    DIVIDE("divide", "/"),
    APPLY("applyToArguments", "apply function/procedure to arguments", null),
    GET_VALUE("getValue", "get map value", null),
    PUT_VALUE("putValue", "put map value", null),
    AND("and", "conjunction", "&&"),
    OR("or", "disjunction", "||"),
    EQ("equal", "equal to", null),
    NOT_EQ("notEqual", "not equal to", "!="),
    LE("less", "less than", "<"),
    GE("greater", "greater than", ">"),
    LEQ("lessOrEq", "less than or equal to", "<="),
    GEQ("greaterOrEq", "greater than or equal to", ">="),
    SET("set", "set value", null),
    GET("get", "get value", null),
    IS_TRUE("isTrue", "is true", null);

    public String getOperationFunction() {
        return operationFunction;
    }                                                          

    public String getOperationString() {
        return operationString;
    }

    public String getOperator() {
        return operator;
    }

    private String operationFunction;
    private String operationString;
    private String operator;

    Operation(String operationFunction, String operationString, String operationSign) {
        this.operationFunction = operationFunction;
        this.operationString = operationString;
        this.operator = operationSign;
    }

    Operation(String operationFunction, String operationSign) {
        this.operationFunction = operationFunction;
        this.operationString = operationFunction;
        this.operator = operationSign;
    }

    static ArrayList<Operation> listValues() {
        ArrayList<Operation> list = new ArrayList<Operation>();
        list.addAll(Arrays.asList(Operation.values()));
        return list;
    }

}
