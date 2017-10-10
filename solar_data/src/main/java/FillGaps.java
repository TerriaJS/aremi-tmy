import java.time.LocalDateTime;
import java.util.List;

// only works for weather data for now
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
    public static void fillMissingTimeStamp(String station) {
        LocalDateTime currDateTIme = Main.wds.get(0).dateTime;
        for (int i = 1; i < Main.wds.size(); i++) {
            currDateTIme = currDateTIme.plusMinutes(30);
            if (!Main.wds.get(i).dateTime.equals(currDateTIme)) {
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

                Main.wds.add(i, new ActualWD(currDateTIme, weatherReadings));

                System.out.println("At index " + i + ", we found a timestamp gap and took care of it");
            }
        }
    }

    static int[] counter = new int[11];

    public static void fillShortGap(int from, int to, int gapSize, int whichVariable) {
        double[] values;
        switch (whichVariable) {
            case PRECIP:
                values = linearInterpolate(Main.wds.get(from).precip.value, Main.wds.get(to).precip.value, gapSize);
                break;
            case WBTEMP:
                values = linearInterpolate(Main.wds.get(from).wbTemp.value, Main.wds.get(to).wbTemp.value, gapSize);
                break;
            case DPTEMP:
                values = linearInterpolate(Main.wds.get(from).dpTemp.value, Main.wds.get(to).dpTemp.value, gapSize);
                break;
            case AIRTEMP:
                values = linearInterpolate(Main.wds.get(from).airTemp.value, Main.wds.get(to).airTemp.value, gapSize);
                break;
            case HUMIDITY:
                values = linearInterpolate(Main.wds.get(from).humidity.value, Main.wds.get(to).humidity.value, gapSize);
                break;
            case VAP:
                values = linearInterpolate(Main.wds.get(from).vapPressure.value, Main.wds.get(to).vapPressure.value, gapSize);
                break;
            case SATVAP:
                values = linearInterpolate(Main.wds.get(from).satVapPressure.value, Main.wds.get(to).satVapPressure.value, gapSize);
                break;
            case WINDSPD:
                values = linearInterpolate(Main.wds.get(from).windSpeed.value, Main.wds.get(to).windSpeed.value, gapSize);
                break;
            case WINDDIR:
                values = linearInterpolate(Main.wds.get(from).windDir.value, Main.wds.get(to).windDir.value, gapSize);
                break;
            case WINDGUST:
                values = linearInterpolate(Main.wds.get(from).windGust.value, Main.wds.get(to).windGust.value, gapSize);
                break;
            case SEALVL:
                values = linearInterpolate(Main.wds.get(from).seaLvlPressure.value, Main.wds.get(to).seaLvlPressure.value, gapSize);
                break;
            default:
                values = new double[11];
        }
        //double[] values = linearInterpolate(from, to, gapSize);
        // how to fill in the gaps:
        // take the gap count and the array returned by linear interpolation
        // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
        for (int i = 1; i <= gapSize; i++) {
            switch (whichVariable) {
                case PRECIP:
                    Main.wds.get(from + i).precip.value = values[i];
                    Main.wds.get(from + i).precip.isValid = true;
                    break;
                case WBTEMP:
                    Main.wds.get(from + i).wbTemp.value = values[i];
                    Main.wds.get(from + i).wbTemp.isValid = true;
                    break;
                case DPTEMP:
                    Main.wds.get(from + i).dpTemp.value = values[i];
                    Main.wds.get(from + i).dpTemp.isValid = true;
                    break;
                case AIRTEMP:
                    Main.wds.get(from + i).airTemp.value = values[i];
                    Main.wds.get(from + i).airTemp.isValid = true;
                    break;
                case HUMIDITY:
                    Main.wds.get(from + i).humidity.value = values[i];
                    Main.wds.get(from + i).humidity.isValid = true;
                    break;
                case VAP:
                    Main.wds.get(from + i).vapPressure.value = values[i];
                    Main.wds.get(from + i).vapPressure.isValid = true;
                    break;
                case SATVAP:
                    Main.wds.get(from + i).satVapPressure.value = values[i];
                    Main.wds.get(from + i).satVapPressure.isValid = true;
                    break;
                case WINDSPD:
                    Main.wds.get(from + i).windSpeed.value = values[i];
                    Main.wds.get(from + i).windSpeed.isValid = true;
                    break;
                case WINDDIR:
                    Main.wds.get(from + i).windDir.value = values[i];
                    Main.wds.get(from + i).windDir.isValid = true;
                    break;
                case WINDGUST:
                    Main.wds.get(from + i).windGust.value = values[i];
                    Main.wds.get(from + i).windGust.isValid = true;
                    break;
                case SEALVL:
                    Main.wds.get(from + i).seaLvlPressure.value = values[i];
                    Main.wds.get(from + i).seaLvlPressure.isValid = true;
                    break;
            }
        }
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

                    // from = gapIndex - gapSize - 1
                    // to = gapIndex
                    // gapSize = counter[arrayIndex]
                    fillShortGap(gapIndex - counter[arrayIndex] - 1, gapIndex, counter[arrayIndex], arrayIndex);
                    System.out.println("At index " + gapIndex + " we interpolated this gap of length " + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[arrayIndex] <= 48) {
                    System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // no rule specified in sandia method for gaps this big
                else {
                    System.out.println("At index " + gapIndex + " we can't take care of this gap of length " + (counter[arrayIndex]) + " for " + whichVariable.varName);
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
