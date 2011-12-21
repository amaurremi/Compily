package runtimy;

import generate.Type;
import generate.Operation;

public class BooleanVal extends Value {
    public final Boolean value;

    public BooleanVal(boolean value) {
        this.value = value;
    }

    public String toString() {
        return value.toString();
    }

    public int hashCode() {
        return 31 * value.hashCode();
    }

    public boolean equals(Object object) {
        if (object instanceof BooleanVal) {
             return ((BooleanVal)object).value.equals(value);
        } else {
            return false;
        }
    }

    public Value apply(Value... values) {
        incompatibleValueException(Operation.APPLY, Type.BOOLEAN);
        return null;
    }

    public Value getValue(Value value) {
        incompatibleValueException(Operation.GET_VALUE, Type.BOOLEAN);
        return null;
    }

    public void putValue(Value key, Value value) {
        incompatibleValueException(Operation.PUT_VALUE, Type.BOOLEAN);
    }

    public boolean isTrue() {
        return value;
    }

            public Value add(Value val) {
            return val.addBooleanVal(this);
        }

            public Value subtract(Value val) {
            return val.subtractBooleanVal(this);
        }

            public Value multiply(Value val) {
            return val.multiplyBooleanVal(this);
        }

            public Value divide(Value val) {
            return val.divideBooleanVal(this);
        }

            public Value and(Value val) {
            return val.andBooleanVal(this);
        }

            public Value or(Value val) {
            return val.orBooleanVal(this);
        }

            public Value notEqual(Value val) {
            return val.notEqualBooleanVal(this);
        }

            public Value less(Value val) {
            return val.lessBooleanVal(this);
        }

            public Value greater(Value val) {
            return val.greaterBooleanVal(this);
        }

            public Value lessOrEq(Value val) {
            return val.lessOrEqBooleanVal(this);
        }

            public Value greaterOrEq(Value val) {
            return val.greaterOrEqBooleanVal(this);
        }

    
            public BooleanVal orStringVal(StringVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) || Boolean.valueOf(value));
}

            public StringVal addStringVal(StringVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public BooleanVal andStringVal(StringVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) && Boolean.valueOf(value));
}

            public BooleanVal notEqualBooleanVal(BooleanVal val) {
	 return new BooleanVal(val.value != value);
}

            public BooleanVal orBooleanVal(BooleanVal val) {
	 return new BooleanVal(val.value || value);
}

            public BooleanVal notEqualStringVal(StringVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) != Boolean.valueOf(value));
}

            public StringVal addDoubleVal(DoubleVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public StringVal addBooleanVal(BooleanVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public BooleanVal andBooleanVal(BooleanVal val) {
	 return new BooleanVal(val.value && value);
}

            public Value subtractDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value multiplyDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value divideDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value andDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value orDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value notEqualDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value lessDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value greaterDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value lessOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value greaterOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.BOOLEAN, Type.DOUBLE);
	 return null;
}

            public Value subtractStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value multiplyStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value divideStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value lessStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value greaterStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value lessOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value greaterOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.BOOLEAN, Type.STRING);
	 return null;
}

            public Value addFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value subtractFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value multiplyFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value divideFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value andFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value orFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value notEqualFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value lessFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value greaterFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value lessOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value greaterOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.BOOLEAN, Type.FUNCTION);
	 return null;
}

            public Value addObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value subtractObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value multiplyObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value divideObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value andObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value orObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value notEqualObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value lessObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value greaterObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value lessOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value greaterOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.BOOLEAN, Type.OBJECT);
	 return null;
}

            public Value subtractBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value multiplyBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value divideBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value lessBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value greaterBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value lessOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

            public Value greaterOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.BOOLEAN, Type.BOOLEAN);
	 return null;
}

    }
