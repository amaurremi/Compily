import runtimy.*;

public class Example {
	public static void main(String[] args) {
		final Variable print0 = new Variable();
		print0.set(new FunctionVal(new Call("print0") {
			public Value apply(Value... variables) {
				checkParams(1, variables);
				final Variable string2 = new Variable(variables[0]);
				System.out.println(string2.get());
				return null;
			}
		}));
		
		final Variable main = new Variable();
		main.set(new FunctionVal(new Call("main") {
			public Value apply(Value... variables) {
				checkParams(0, variables);
				{
					final Variable x3 = new Variable(new DoubleVal(2.0).subtract(new DoubleVal(1.0)).subtract(new DoubleVal(3.0)));
					final Variable y4 = new Variable(new ObjectVal()
						.field(new DoubleVal(2.0), new DoubleVal(3.0))
						.field(new DoubleVal(3.0), new DoubleVal(4.0))
					);
					print0.get().apply(y4.get().getValue(new DoubleVal(2.0)).multiply(x3.get()));
				}
				return null;
			}
		}));
		
		main.get().apply();
	}

}
