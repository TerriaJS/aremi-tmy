import java.time.LocalDateTime;
import java.util.List;

public class FillGaps {

    public static double[] linearInterpolate(double from, double to, int gapsCount) {
        double[] res = new double[gapsCount + 2];

        // the first and last value should be from and to
        res[0] = from;
        res[res.length - 1] = to;

        // calculate the gradient over the two points
        double gradient = (to - from) / (gapsCount + 1);

        for (int i = 1; i < res.length - 1; i++) {
            res[i] = res[i-1] + gradient;

        }

        return res;
    }

    public static void fillMissingTimeStamp(List<WeatherData> list) {
        LocalDateTime currDateTIme = list.get(0).dateTime;
        for (int i = 1; i < list.size(); i++) {
            currDateTIme = currDateTIme.plusMinutes(30);
            if (!list.get(i).dateTime.equals(currDateTIme)) {
                // if there is a gap, we skipped one half-hourly reading, so continue on to the next half hour
                currDateTIme = currDateTIme.plusMinutes(30);
                System.out.println("At index " + i + ", we found a timestamp gap");
            }
        }
    }
}
