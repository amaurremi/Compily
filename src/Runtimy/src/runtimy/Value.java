package runtimy;

import generate.Operation;
import generate.Type;

public abstract class Value {
    public void incompatibleValueException(Operation operation, Type type1, Type type2) {
        throw new MRTError("Operation '" + operation.getOperationString() +
                        "' incompatible with types '" + type1.getTypeString() + "' and '" + type2.getTypeString() + "'");
    }

    public void incompatibleValueException(Operation operation, Type type) {
        throw new MRTError("Operation '" + operation.getOperationString() +
                        "' incompatible with type '" + type.getTypeString() + "'");
    }

    public void incompatibleValueException(String message) {
        throw new MRTError(message);
    }

    public Value get() {
        return this;
    }

    public abstract void putValue(Value key, Value value);

    public abstract boolean isTrue();

public BooleanVal equal(Value val) {
       return new BooleanVal(new Boolean(this.equals(val)));
}
public abstract Value apply(Value... values);
public abstract Value getValue(Value value);
    public abstract Value add(Value val);
            public abstract Value addDoubleVal(DoubleVal val);
            public abstract Value addStringVal(StringVal val);
            public abstract Value addFunctionVal(FunctionVal val);
            public abstract Value addObjectVal(ObjectVal val);
            public abstract Value addBooleanVal(BooleanVal val);
        public abstract Value subtract(Value val);
            public abstract Value subtractDoubleVal(DoubleVal val);
            public abstract Value subtractStringVal(StringVal val);
            public abstract Value subtractFunctionVal(FunctionVal val);
            public abstract Value subtractObjectVal(ObjectVal val);
            public abstract Value subtractBooleanVal(BooleanVal val);
        public abstract Value multiply(Value val);
            public abstract Value multiplyDoubleVal(DoubleVal val);
            public abstract Value multiplyStringVal(StringVal val);
            public abstract Value multiplyFunctionVal(FunctionVal val);
            public abstract Value multiplyObjectVal(ObjectVal val);
            public abstract Value multiplyBooleanVal(BooleanVal val);
        public abstract Value divide(Value val);
            public abstract Value divideDoubleVal(DoubleVal val);
            public abstract Value divideStringVal(StringVal val);
            public abstract Value divideFunctionVal(FunctionVal val);
            public abstract Value divideObjectVal(ObjectVal val);
            public abstract Value divideBooleanVal(BooleanVal val);
        public abstract Value and(Value val);
            public abstract Value andDoubleVal(DoubleVal val);
            public abstract Value andStringVal(StringVal val);
            public abstract Value andFunctionVal(FunctionVal val);
            public abstract Value andObjectVal(ObjectVal val);
            public abstract Value andBooleanVal(BooleanVal val);
        public abstract Value or(Value val);
            public abstract Value orDoubleVal(DoubleVal val);
            public abstract Value orStringVal(StringVal val);
            public abstract Value orFunctionVal(FunctionVal val);
            public abstract Value orObjectVal(ObjectVal val);
            public abstract Value orBooleanVal(BooleanVal val);
        public abstract Value notEqual(Value val);
            public abstract Value notEqualDoubleVal(DoubleVal val);
            public abstract Value notEqualStringVal(StringVal val);
            public abstract Value notEqualFunctionVal(FunctionVal val);
            public abstract Value notEqualObjectVal(ObjectVal val);
            public abstract Value notEqualBooleanVal(BooleanVal val);
        public abstract Value less(Value val);
            public abstract Value lessDoubleVal(DoubleVal val);
            public abstract Value lessStringVal(StringVal val);
            public abstract Value lessFunctionVal(FunctionVal val);
            public abstract Value lessObjectVal(ObjectVal val);
            public abstract Value lessBooleanVal(BooleanVal val);
        public abstract Value greater(Value val);
            public abstract Value greaterDoubleVal(DoubleVal val);
            public abstract Value greaterStringVal(StringVal val);
            public abstract Value greaterFunctionVal(FunctionVal val);
            public abstract Value greaterObjectVal(ObjectVal val);
            public abstract Value greaterBooleanVal(BooleanVal val);
        public abstract Value lessOrEq(Value val);
            public abstract Value lessOrEqDoubleVal(DoubleVal val);
            public abstract Value lessOrEqStringVal(StringVal val);
            public abstract Value lessOrEqFunctionVal(FunctionVal val);
            public abstract Value lessOrEqObjectVal(ObjectVal val);
            public abstract Value lessOrEqBooleanVal(BooleanVal val);
        public abstract Value greaterOrEq(Value val);
            public abstract Value greaterOrEqDoubleVal(DoubleVal val);
            public abstract Value greaterOrEqStringVal(StringVal val);
            public abstract Value greaterOrEqFunctionVal(FunctionVal val);
            public abstract Value greaterOrEqObjectVal(ObjectVal val);
            public abstract Value greaterOrEqBooleanVal(BooleanVal val);
    }
