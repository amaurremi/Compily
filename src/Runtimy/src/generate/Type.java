package generate;

public enum Type {
    DOUBLE("Double", "DoubleVal"),
    STRING("String", "StringVal"),
    FUNCTION(null, "FunctionVal"),
    OBJECT(null, "ObjectVal"),
    BOOLEAN("Boolean", "BooleanVal");

    private String primitiveType;
    private String typeString;

    Type(String primitiveType, String typeString) {
        this.primitiveType = primitiveType;
        this.typeString = typeString;
    }

    public String getPrimitiveType() {
        return primitiveType;
    }

    public String getTypeString() {
        return typeString;
    }

}
