public class Reading {

    double value;
    String quality;
    int count;

    public Reading(double value, String quality, int count) {
        this.value = value;
        this.quality = quality;
        this.count = count;
    }

    public Reading(double value, String quality) {
        this.value = value;
        this.quality = quality;
        count = -1;
    }

    public String[] combineValues() {
        if (count == -1) return new String[] {Double.toString(value), quality};
        else return new String[] {Double.toString(value), quality, Integer.toString(count)};
    }
}
