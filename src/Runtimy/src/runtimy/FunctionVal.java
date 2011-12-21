package runtimy;

import generate.Type;
import generate.Operation;

public class FunctionVal extends Value {
    Call call;
    
    public FunctionVal(Call call) {
        this.call = call;
    }

    public Value apply(Value... values) {
        return call.apply(values);
    }

    public Value getValue(Value value) {
        incompatibleValueException(Operation.GET_VALUE, Type.FUNCTION);
        return null;
    }


    public void putValue(Value key, Value value) {
        incompatibleValueException(Operation.PUT_VALUE, Type.FUNCTION);
    }

    public boolean isTrue() {
        return false;
    }

            public Value add(Value val) {
            return val.addFunctionVal(this);
        }

            public Value subtract(Value val) {
            return val.subtractFunctionVal(this);
        }

            public Value multiply(Value val) {
            return val.multiplyFunctionVal(this);
        }

            public Value divide(Value val) {
            return val.divideFunctionVal(this);
        }

            public Value and(Value val) {
            return val.andFunctionVal(this);
        }

            public Value or(Value val) {
            return val.orFunctionVal(this);
        }

            public Value notEqual(Value val) {
            return val.notEqualFunctionVal(this);
        }

            public Value less(Value val) {
            return val.lessFunctionVal(this);
        }

            public Value greater(Value val) {
            return val.greaterFunctionVal(this);
        }

            public Value lessOrEq(Value val) {
            return val.lessOrEqFunctionVal(this);
        }

            public Value greaterOrEq(Value val) {
            return val.greaterOrEqFunctionVal(this);
        }

    
            public Value addDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value subtractDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value multiplyDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value divideDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value andDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value orDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value notEqualDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value lessDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value greaterDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value lessOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value greaterOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.FUNCTION, Type.DOUBLE);
	 return null;
}

            public Value addStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value subtractStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value multiplyStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value divideStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value andStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value orStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value notEqualStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value lessStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value greaterStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value lessOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value greaterOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.FUNCTION, Type.STRING);
	 return null;
}

            public Value addFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value subtractFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value multiplyFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value divideFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value andFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value orFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value notEqualFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value lessFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value greaterFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value lessOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value greaterOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.FUNCTION, Type.FUNCTION);
	 return null;
}

            public Value addObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value subtractObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value multiplyObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value divideObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value andObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value orObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value notEqualObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value lessObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value greaterObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value lessOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value greaterOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.FUNCTION, Type.OBJECT);
	 return null;
}

            public Value addBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value subtractBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value multiplyBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value divideBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value andBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value orBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value notEqualBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value lessBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value greaterBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value lessOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

            public Value greaterOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.FUNCTION, Type.BOOLEAN);
	 return null;
}

    }
