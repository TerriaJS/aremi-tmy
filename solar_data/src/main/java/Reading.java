public class Reading {

    String varName;
    double value;
    String quality;
    int count;
    int fillCount = 0;
    Value v;

    public Reading(String varName, double value, String quality, int count, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = count;
        this.v = (isValid) ? Value.Valid : Value.Invalid;
    }

    public Reading(String varName, double value, String quality, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = (isValid) ? 1 : 0;
        this.v = (isValid) ? Value.Valid : Value.Invalid;
    }

    public Reading(String varName, double value, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.count = (isValid) ? 1 : 0;
        this.v = (isValid) ? Value.Valid : Value.Invalid;
    }

    // checks if the value of this reading is valid
    public boolean isValid() {
        return v == Value.Filled || v == Value.Valid;
    }
}
