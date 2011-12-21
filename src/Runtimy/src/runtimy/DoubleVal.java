package runtimy;

import generate.Type;
import generate.Operation;

public class DoubleVal extends Value {
    public final Double value;

    public DoubleVal(double value) {
        this.value = value;
    }

    public String toString() {
        return value.toString();
    }

    public int hashCode() {
        return 31 * value.hashCode();
    }

    public boolean equals(Object object) {
        if (object instanceof DoubleVal) {
             return ((DoubleVal)object).value.equals(value);
        } else {
            return false;
        }
    }

    public Value apply(Value... values) {
        incompatibleValueException(Operation.APPLY, Type.DOUBLE);
        return null;
    }

    public Value getValue(Value value) {
        incompatibleValueException(Operation.GET_VALUE, Type.DOUBLE);
        return null;
    }

    public boolean isTrue() {
        return false;
    }

    public void putValue(Value key, Value value) {
        incompatibleValueException(Operation.PUT_VALUE, Type.DOUBLE);
    }

            public Value add(Value val) {
            return val.addDoubleVal(this);
        }

            public Value subtract(Value val) {
            return val.subtractDoubleVal(this);
        }

            public Value multiply(Value val) {
            return val.multiplyDoubleVal(this);
        }

            public Value divide(Value val) {
            return val.divideDoubleVal(this);
        }

            public Value and(Value val) {
            return val.andDoubleVal(this);
        }

            public Value or(Value val) {
            return val.orDoubleVal(this);
        }

            public Value notEqual(Value val) {
            return val.notEqualDoubleVal(this);
        }

            public Value less(Value val) {
            return val.lessDoubleVal(this);
        }

            public Value greater(Value val) {
            return val.greaterDoubleVal(this);
        }

            public Value lessOrEq(Value val) {
            return val.lessOrEqDoubleVal(this);
        }

            public Value greaterOrEq(Value val) {
            return val.greaterOrEqDoubleVal(this);
        }

    
            public BooleanVal greaterOrEqDoubleVal(DoubleVal val) {
	 return new BooleanVal(val.value >= value);
}

            public DoubleVal subtractStringVal(StringVal val) {
	 return new DoubleVal(Double.valueOf(val.value) - Double.valueOf(value));
}

            public DoubleVal divideStringVal(StringVal val) {
	 return new DoubleVal(Double.valueOf(val.value) / Double.valueOf(value));
}

            public StringVal addBooleanVal(BooleanVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public DoubleVal addDoubleVal(DoubleVal val) {
	 return new DoubleVal(Double.valueOf(val.value) + Double.valueOf(value));
}

            public DoubleVal subtractDoubleVal(DoubleVal val) {
	 return new DoubleVal(val.value - value);
}

            public DoubleVal multiplyStringVal(StringVal val) {
	 return new DoubleVal(Double.valueOf(val.value) * Double.valueOf(value));
}

            public DoubleVal divideDoubleVal(DoubleVal val) {
	 return new DoubleVal(val.value / value);
}

            public BooleanVal greaterDoubleVal(DoubleVal val) {
	 return new BooleanVal(val.value > value);
}

            public BooleanVal notEqualDoubleVal(DoubleVal val) {
	 return new BooleanVal(val.value != value);
}

            public StringVal addStringVal(StringVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public BooleanVal lessDoubleVal(DoubleVal val) {
	 return new BooleanVal(val.value < value);
}

            public DoubleVal multiplyDoubleVal(DoubleVal val) {
	 return new DoubleVal(val.value * value);
}

            public BooleanVal lessOrEqDoubleVal(DoubleVal val) {
	 return new BooleanVal(val.value <= value);
}

            public Value andDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.DOUBLE, Type.DOUBLE);
	 return null;
}

            public Value orDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.DOUBLE, Type.DOUBLE);
	 return null;
}

            public Value andStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value orStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value notEqualStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value lessStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value greaterStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value lessOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value greaterOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.DOUBLE, Type.STRING);
	 return null;
}

            public Value addFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value subtractFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value multiplyFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value divideFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value andFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value orFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value notEqualFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value lessFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value greaterFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value lessOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value greaterOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.DOUBLE, Type.FUNCTION);
	 return null;
}

            public Value addObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value subtractObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value multiplyObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value divideObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value andObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value orObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value notEqualObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value lessObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value greaterObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value lessOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value greaterOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.DOUBLE, Type.OBJECT);
	 return null;
}

            public Value subtractBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value multiplyBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value divideBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value andBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value orBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value notEqualBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value lessBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value greaterBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value lessOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

            public Value greaterOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.DOUBLE, Type.BOOLEAN);
	 return null;
}

    }
