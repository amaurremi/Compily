package runtimy;

import generate.Type;
import generate.Operation;

public class StringVal extends Value {
    public final String value;

    public StringVal(String value) {
        this.value = value;
    }

    public String toString() {
        return value;
    }

    public int hashCode() {
        return 31 * value.hashCode();
    }

    public boolean equals(Object object) {
        if (object instanceof StringVal) {
             return ((StringVal)object).value.equals(value);
        } else {
            return false;
        }
    }
    
    public boolean isTrue() {
        return false;
    }

    public Value apply(Value... values) {
        incompatibleValueException(Operation.APPLY, Type.STRING);
        return null;
    }

    public Value getValue(Value value) {
        incompatibleValueException(Operation.GET_VALUE, Type.STRING);
        return null;
    }


    public void putValue(Value key, Value value) {
        incompatibleValueException(Operation.PUT_VALUE, Type.STRING);
    }

            public Value add(Value val) {
            return val.addStringVal(this);
        }

            public Value subtract(Value val) {
            return val.subtractStringVal(this);
        }

            public Value multiply(Value val) {
            return val.multiplyStringVal(this);
        }

            public Value divide(Value val) {
            return val.divideStringVal(this);
        }

            public Value and(Value val) {
            return val.andStringVal(this);
        }

            public Value or(Value val) {
            return val.orStringVal(this);
        }

            public Value notEqual(Value val) {
            return val.notEqualStringVal(this);
        }

            public Value less(Value val) {
            return val.lessStringVal(this);
        }

            public Value greater(Value val) {
            return val.greaterStringVal(this);
        }

            public Value lessOrEq(Value val) {
            return val.lessOrEqStringVal(this);
        }

            public Value greaterOrEq(Value val) {
            return val.greaterOrEqStringVal(this);
        }

    
            public DoubleVal multiplyDoubleVal(DoubleVal val) {
	 return new DoubleVal(Double.valueOf(val.value) * Double.valueOf(value));
}

            public BooleanVal notEqualStringVal(StringVal val) {
	 return new BooleanVal(val.value.toString() != value.toString());
}

            public DoubleVal subtractDoubleVal(DoubleVal val) {
	 return new DoubleVal(Double.valueOf(val.value) - Double.valueOf(value));
}

            public BooleanVal notEqualBooleanVal(BooleanVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) != Boolean.valueOf(value));
}

            public BooleanVal andBooleanVal(BooleanVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) && Boolean.valueOf(value));
}

            public BooleanVal orBooleanVal(BooleanVal val) {
	 return new BooleanVal(Boolean.valueOf(val.value) || Boolean.valueOf(value));
}

            public StringVal addDoubleVal(DoubleVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public DoubleVal divideDoubleVal(DoubleVal val) {
	 return new DoubleVal(Double.valueOf(val.value) / Double.valueOf(value));
}

            public StringVal addStringVal(StringVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public StringVal addBooleanVal(BooleanVal val) {
	 return new StringVal(val.value.toString() + value.toString());
}

            public Value andDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value orDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value notEqualDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value lessDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value greaterDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value lessOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value greaterOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.STRING, Type.DOUBLE);
	 return null;
}

            public Value subtractStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.STRING, Type.STRING);
	 return null;
}

            public Value multiplyStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.STRING, Type.STRING);
	 return null;
}

            public Value divideStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.STRING, Type.STRING);
	 return null;
}

            public Value andStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.STRING, Type.STRING);
	 return null;
}

            public Value orStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.STRING, Type.STRING);
	 return null;
}

            public Value lessStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.STRING, Type.STRING);
	 return null;
}

            public Value greaterStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.STRING, Type.STRING);
	 return null;
}

            public Value lessOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.STRING, Type.STRING);
	 return null;
}

            public Value greaterOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.STRING, Type.STRING);
	 return null;
}

            public Value addFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value subtractFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value multiplyFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value divideFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value andFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value orFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value notEqualFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value lessFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value greaterFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value lessOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value greaterOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.STRING, Type.FUNCTION);
	 return null;
}

            public Value addObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value subtractObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value multiplyObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value divideObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value andObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value orObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value notEqualObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value lessObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value greaterObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value lessOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value greaterOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.STRING, Type.OBJECT);
	 return null;
}

            public Value subtractBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value multiplyBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value divideBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value lessBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value greaterBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value lessOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.STRING, Type.BOOLEAN);
	 return null;
}

            public Value greaterOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.STRING, Type.BOOLEAN);
	 return null;
}

    }
