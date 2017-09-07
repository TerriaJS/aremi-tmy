public class AveragedReading {

    double value;
    String quality;
    int count;

    public AveragedReading(double value, String quality, int count) {
        this.value = value;
        this.quality = quality;
        this.count = count;
    }

    public AveragedReading(double value, String quality) {
        this.value = value;
        this.quality = quality;
        count = -1;
    }
}
