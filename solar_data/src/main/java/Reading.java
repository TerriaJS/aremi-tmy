public class Reading {

    String varName;
    double value;
    String quality;
    int count;
    boolean isValid;

    public Reading(String varName, double value, String quality, int count) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = count;
        this.isValid = this.value > 0;
    }

    public Reading(String varName, double value, String quality) {
        this.varName = varName;
        this.value = value;
        this.quality = quality;
        this.count = -1;
        this.isValid = this.value > 0;
    }
}
