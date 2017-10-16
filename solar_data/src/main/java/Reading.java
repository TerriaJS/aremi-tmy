public class Reading {

    String varName;
    double value;
    String quality;
    int count;
    boolean isValid;

    public Reading(String varName, double value, String quality, int count, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = count;
        this.isValid = isValid;
    }

    public Reading(String varName, double value, String quality, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = -1;
        this.isValid = isValid;
    }

    public Reading(String varName, double value, boolean isValid) {
        this.varName = varName;
        this.value = value;
        this.isValid = isValid;
    }
}
