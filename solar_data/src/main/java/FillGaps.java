import java.time.LocalDateTime;
import java.util.List;

public class FillGaps {

    public static final int PRECIP = 0;
    public static final int WBTEMP = 1;
    public static final int DPTEMP = 2;
    public static final int AIRTEMP = 3;
    public static final int HUMIDITY = 4;
    public static final int VAP = 5;
    public static final int SATVAP = 6;
    public static final int WINDSPD = 7;
    public static final int WINDDIR = 8;
    public static final int WINDGUST = 9;
    public static final int SEALVL = 10;


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

    // only works for ActualWD for now
    public static void fillMissingTimeStamp(String station, List<WeatherData> list) {
        LocalDateTime currDateTIme = list.get(0).dateTime;
        for (int i = 1; i < list.size(); i++) {
            currDateTIme = currDateTIme.plusMinutes(30);
            if (!list.get(i).dateTime.equals(currDateTIme)) {
                // we found a gap, meaning we skipped one half-hourly reading

                // create a weather data object and add it to the list
                String[] weatherReadings = new String[30];
                weatherReadings[0] = "hm";
                weatherReadings[1] = Integer.toString(Integer.parseInt(station));
                weatherReadings[2] = Integer.toString(currDateTIme.getYear());
                weatherReadings[3] = Integer.toString(currDateTIme.getMonthValue());
                weatherReadings[4] = Integer.toString(currDateTIme.getDayOfMonth());
                weatherReadings[5] = Integer.toString(currDateTIme.getHour());
                weatherReadings[6] = Integer.toString(currDateTIme.getMinute());
                // std time
                weatherReadings[7] = Integer.toString(currDateTIme.getYear());
                weatherReadings[8] = Integer.toString(currDateTIme.getMonthValue());
                weatherReadings[9] = Integer.toString(currDateTIme.getDayOfMonth());
                weatherReadings[10] = Integer.toString(currDateTIme.getHour());
                weatherReadings[11] = Integer.toString(currDateTIme.getMinute());

                weatherReadings[29] = "#";

                list.add(i, new ActualWD(currDateTIme, weatherReadings));

                System.out.println("At index " + i + ", we found a timestamp gap");
            }
        }
    }

    static int[] counter = new int[11];

    public static void fillShortGap() {

    }

    public static void fillLongGap() {

    }

    public static void handleGap(int gapIndex, int arrayIndex, Reading whichVariable) {
        if (!whichVariable.isValid) {
            counter[arrayIndex]++;
        } else {
            if (counter[arrayIndex] > 0) {
                // do linear interpolation if gap less than 5 hours
                if (counter[arrayIndex] <= 10) {
                    System.out.println("At index " + gapIndex + " we interpolated this gap of length " + (counter[arrayIndex] / 2.0) + " for " + whichVariable.varName);
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[arrayIndex] <= 48) {
                    System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[arrayIndex] / 2.0) + " for " + whichVariable.varName);
                }

                // no rule specified in sandia method for gaps this big
                else {
                    System.out.println("At index " + gapIndex + " we can't take care of this gap of length " + (counter[arrayIndex] / 2.0) + " for " + whichVariable.varName);
                }
                counter[arrayIndex] = 0;
            }
        }
    }

    public static void findGaps(List<WeatherData> list) {
        // initialise an array as counter, size is the number of variables
        // if we find a gap for a variable, add one to the array
        // otherwise (no gaps), if the array value > 0
            // if array value <= 10: linear interpolate and fill in the gaps
                // how to fill in the gaps:
                    // take the gap count and the array returned by linear interpolation
                    // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
            // if array value <= 48: take the averages of the previous 24 hours and next 24 hours
            // otherwise, leave it empty and reset counter back to 0

        for (int i = 0; i < list.size(); i++) {
            handleGap(i, PRECIP, list.get(i).precip);
            handleGap(i, WBTEMP, list.get(i).wbTemp);
            handleGap(i, DPTEMP, list.get(i).dpTemp);
            handleGap(i, AIRTEMP, list.get(i).airTemp);
            handleGap(i, HUMIDITY, list.get(i).humidity);
            handleGap(i, VAP, list.get(i).vapPressure);
            handleGap(i, SATVAP, list.get(i).satVapPressure);
            handleGap(i, WINDSPD, list.get(i).windSpeed);
            handleGap(i, WINDDIR, list.get(i).windDir);
            handleGap(i, WINDGUST, list.get(i).windGust);
            handleGap(i, SEALVL, list.get(i).seaLvlPressure);
        }


    }
}
