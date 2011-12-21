package generate;

import java.io.*;
import java.util.*;

import org.apache.velocity.app.Velocity;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;

public class Generate {

    public static void main(String[] args) throws Exception {
        Map<OperationTriple, String> operationMap = new HashMap<OperationTriple, String>();
        createOperationMap(operationMap);
        File generated = new File("src/runtimy");
        generated.mkdir();
        File fromHaskell = new File(generated.getParentFile().getParentFile(), "generated");
        fromHaskell.mkdir();

        generateValueClass(new File(generated, "Value.java"));
        generateValueExtensionClass(operationMap, Type.BOOLEAN, "BooleanTmpl.vm");
        generateValueExtensionClass(operationMap, Type.DOUBLE, "DoubleTmpl.vm");
        generateValueExtensionClass(operationMap, Type.STRING, "StringTmpl.vm");
        generateValueExtensionClass(operationMap, Type.OBJECT, "ObjectTmpl.vm");
        generateValueExtensionClass(operationMap, Type.FUNCTION, "FunctionTmpl.vm");
        generateClass("IVE.txt", "MRTError.java");
        generateClass("Variable.txt", "Variable.java");
        generateClass("Call.txt", "Call.java");
    }

    private static void generateClass(String templateName, String outputName) throws IOException {
        Scanner input = new Scanner(new FileReader("src/generate/templates/" + templateName));
        BufferedWriter output = new BufferedWriter(new FileWriter("src/runtimy/" + outputName));
        while (input.hasNextLine()) {
            output.write(input.nextLine());
            output.newLine();
        }
        input.close();
        output.close();
    }

    private static void generateValueExtensionClass(Map<OperationTriple, String> operationMap, Type thisType,
                                                    String templatePath) throws IOException {
        List<String> finallyAppliedFunctions = new ArrayList<String>();
        Type[] types = Type.values();
        List<Operation> operations = Operations.allExceptFunctionObjectOperations();
        for (OperationTriple triple : operationMap.keySet()) {
            if (triple.firstType == thisType) {
                finallyAppliedFunctions.add(operationMap.get(triple));
            }
        }
        for (Type rightType : types) {
            for (Operation operation : operations) {
                    if (!operationMap.containsKey(new OperationTriple(thisType, operation, rightType))) {
                        finallyAppliedFunctions.add(
                            "public Value " + operation.getOperationFunction() + rightType.getTypeString() +
                                    "(" + rightType.getTypeString() + " val) throws MRTError {\n" +
                            "\t incompatibleValueException(Operation." + operation + ", Type." + thisType + ", Type."
                                    + rightType + ");\n" +
                            "\t return null;\n" +
                            "}"
                        );
                    }
            }
        }
        Template template = Velocity.getTemplate("src/generate/templates/" + templatePath);
        VelocityContext context = new VelocityContext();
        context.put("finallyAppliedFunctions", finallyAppliedFunctions);
        context.put("type", thisType);
        context.put("types", Type.values());
        context.put("allExceptFunctionObjectOperations", Operations.allExceptFunctionObjectOperations());
        mergeContext(template, context, thisType.getTypeString() + ".java");
    }

    private static void mergeContext(Template template, VelocityContext context, String writerPath) throws IOException {
        Writer writer = new FileWriter("src/runtimy/" + writerPath);
        template.merge(context, writer);
        writer.flush();
        writer.close();
    }

    private static void generateValueClass(File writerFilePath) throws IOException {
        VelocityContext context = new VelocityContext();
        context.put("allExceptFunctionObjectOperations", Operations.allExceptFunctionObjectOperations());
        context.put("types", Type.values());
        Writer writer = new FileWriter(writerFilePath);
        Template template = Velocity.getTemplate("src/generate/templates/ValueTmpl.vm");
        template.merge(context, writer);
        writer.close();
    }

    private static void createOperationMap(Map<OperationTriple, String> operationMap) {
        Type[] types = new Type[]{Type.DOUBLE, Type.STRING, Type.BOOLEAN};
        for (Type type1 : types) {
            for (Type type2 : types) {
                GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.ADD, Type.STRING, type1, type2);
            }
        }

        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.ADD, Type.DOUBLE, Type.DOUBLE, Type.DOUBLE);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.ARITHMETIC, Type.DOUBLE, Type.DOUBLE);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.ARITHMETIC, Type.DOUBLE, Type.STRING,
                Type.DOUBLE);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.ARITHMETIC, Type.DOUBLE, Type.DOUBLE,
                Type.STRING);

        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.BOOLEAN, Type.BOOLEAN, Type.BOOLEAN);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.BOOLEAN, Type.BOOLEAN, Type.STRING,
                Type.BOOLEAN);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.BOOLEAN, Type.BOOLEAN, Type.BOOLEAN,
                Type.STRING);

        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.BOOLEAN_STRING, Type.BOOLEAN, Type.STRING,
                Type.STRING);
        GenerateOperation.arithmeticBooleanConcat(operationMap, Operations.BOOLEAN_NUMERAL, Type.BOOLEAN, Type.DOUBLE);
    }

}
