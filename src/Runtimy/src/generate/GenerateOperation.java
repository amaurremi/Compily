package generate;

import java.util.Map;

abstract class GenerateOperation {
    static void arithmeticBooleanConcat(Map<OperationTriple, String> operationMap, Operations operations,
                                        Type returnType, Type type) {
        arithmeticBooleanConcat(operationMap, operations, returnType, type, type, "val.value", "value");
    }

    static void arithmeticBooleanConcat(Map<OperationTriple, String> operationMap, Operations operations,
                                        Type returnType, Type thisType, Type otherType) {
        String otherValue;
        String thisValue;
        if (operations.equals(Operations.ARITHMETIC) || operations.equals(Operations.BOOLEAN_NUMERAL)
                || operations.equals(Operations.ADD) && returnType.equals(Type.DOUBLE)) {
            // assuming a value can initially be either Double or String
            otherValue = "Double.valueOf(val.value)";
            thisValue = "Double.valueOf(value)";
        } else if (operations.equals(Operations.BOOLEAN)) {
            // assuming a value can initially be either Boolean or String
            otherValue = "Boolean.valueOf(val.value)";
            thisValue = "Boolean.valueOf(value)";
        } else if (operations.equals(Operations.BOOLEAN_STRING) || operations.equals(Operations.ADD)) {
            otherValue = "val.value.toString()";
            thisValue = "value.toString()";
        } else {
            return;
        }
        arithmeticBooleanConcat(operationMap, operations, returnType, thisType, otherType, otherValue, thisValue);
    }

    private static void arithmeticBooleanConcat(Map<OperationTriple, String> operationMap, Operations operations,
                                                Type returnType, Type thisType, Type otherType, String otherValue,
                                                String thisValue) {
        String otherTypeString = otherType.getTypeString();
        String returnTypeString = returnType.getTypeString();
        for (Operation operation : operations.operations) {
            operationMap.put(new OperationTriple(thisType, operation, otherType),
                    "public " + returnTypeString + " " + operation.getOperationFunction() + otherTypeString + "(" +
                            otherTypeString + " val) {\n" +
                   "\t return new " + returnTypeString + "(" + otherValue + " " + operation.getOperator() + " " +
                            thisValue + ");\n" +
                   "}");
        }
    }

}
