package runtimy;

import generate.Type;
import generate.Operation;

import java.util.HashMap;
import java.util.Map;

public class ObjectVal extends Value {
    private final Map<Value, Value> map;

    public ObjectVal() {
        map = new HashMap<Value, Value>();
    }

    public ObjectVal(Map<Value, Value> map) {
        this.map = map;
    }

    public ObjectVal field(Value key, Value value) {
        map.put(key, value);
        return new ObjectVal(map);
    }

    public Value getValue(Value key) {
        return map.get(key);
    }
    
    public boolean isTrue() {
        return false;
    }

    public void putValue(Value key, Value value) {
        map.put(key, value);
    }

    public Value apply(Value... values) {
        incompatibleValueException(Operation.APPLY, Type.OBJECT);
        return null;
    }
    
            public Value add(Value val) {
            return val.addObjectVal(this);
        }

            public Value subtract(Value val) {
            return val.subtractObjectVal(this);
        }

            public Value multiply(Value val) {
            return val.multiplyObjectVal(this);
        }

            public Value divide(Value val) {
            return val.divideObjectVal(this);
        }

            public Value and(Value val) {
            return val.andObjectVal(this);
        }

            public Value or(Value val) {
            return val.orObjectVal(this);
        }

            public Value notEqual(Value val) {
            return val.notEqualObjectVal(this);
        }

            public Value less(Value val) {
            return val.lessObjectVal(this);
        }

            public Value greater(Value val) {
            return val.greaterObjectVal(this);
        }

            public Value lessOrEq(Value val) {
            return val.lessOrEqObjectVal(this);
        }

            public Value greaterOrEq(Value val) {
            return val.greaterOrEqObjectVal(this);
        }

    
            public Value addDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value subtractDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value multiplyDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value divideDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value andDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value orDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value notEqualDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value lessDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value greaterDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value lessOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value greaterOrEqDoubleVal(DoubleVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.OBJECT, Type.DOUBLE);
	 return null;
}

            public Value addStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value subtractStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value multiplyStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value divideStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value andStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value orStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value notEqualStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value lessStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value greaterStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value lessOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value greaterOrEqStringVal(StringVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.OBJECT, Type.STRING);
	 return null;
}

            public Value addFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value subtractFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value multiplyFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value divideFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value andFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value orFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value notEqualFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value lessFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value greaterFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value lessOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value greaterOrEqFunctionVal(FunctionVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.OBJECT, Type.FUNCTION);
	 return null;
}

            public Value addObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value subtractObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value multiplyObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value divideObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value andObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value orObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value notEqualObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value lessObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value greaterObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value lessOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value greaterOrEqObjectVal(ObjectVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.OBJECT, Type.OBJECT);
	 return null;
}

            public Value addBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.ADD, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value subtractBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.SUBTRACT, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value multiplyBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.MULTIPLY, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value divideBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.DIVIDE, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value andBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.AND, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value orBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.OR, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value notEqualBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.NOT_EQ, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value lessBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LE, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value greaterBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GE, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value lessOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.LEQ, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

            public Value greaterOrEqBooleanVal(BooleanVal val) throws MRTError {
	 incompatibleValueException(Operation.GEQ, Type.OBJECT, Type.BOOLEAN);
	 return null;
}

    }
