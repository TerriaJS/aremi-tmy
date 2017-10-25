import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

// only works for weather data for now
// TODO: make it work for other datasets
public class FillGapsWeather {

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

    private static int[] counter = new int[11];

    // only works for ActualWD for now
    public static void fillMissingTimeStamp() {
        LocalDateTime currDateTIme = Main.wds.get(0).dateTime;
        for (int i = 1; i < Main.wds.size(); i++) {
            currDateTIme = currDateTIme.plusMinutes(30);
            if (!Main.wds.get(i).dateTime.equals(currDateTIme)) {
                // we found a gap, meaning we skipped one half-hourly reading

                // create a weather data object and add it to the list
                Main.wds.add(i, new ActualWD(currDateTIme, null));

//                System.out.println("At index " + i + ", we found a timestamp gap and took care of it");
            }
        }
    }

    public static void fillShortGap(int from, int to, int gapSize, int whichVariable) {
        if (from < 0) return; // this gap is at the beginning of the file and there's no way of interpolating the data

        Reading prevReading = getReading(from, whichVariable);
        Reading nextReading = getReading(to, whichVariable);

        double[] values = FillGaps.linearInterpolate(prevReading.value, nextReading.value, gapSize);

        //double[] values = linearInterpolate(from, to, gapSize);
        // how to fill in the gaps:
        // take the gap count and the array returned by linear interpolation
        // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
        for (int i = 1; i <= gapSize; i++) {
            Reading currReading = getReading(from + i, whichVariable);
            currReading.value = values[i];
            currReading.isValid = true;

        }
    }

    // map which variable to the attributes in WeatherData
    public static Reading getReading(int index, int whichVariable) {
        switch (whichVariable) {
            case PRECIP:
                return Main.wds.get(index).precip;
            case WBTEMP:
                return Main.wds.get(index).wbTemp;
            case DPTEMP:
                return Main.wds.get(index).dpTemp;
            case AIRTEMP:
                return Main.wds.get(index).airTemp;
            case HUMIDITY:
                return Main.wds.get(index).humidity;
            case VAP:
                return Main.wds.get(index).vapPressure;
            case SATVAP:
                return Main.wds.get(index).satVapPressure;
            case WINDSPD:
                return Main.wds.get(index).windSpeed;
            case WINDDIR:
                return Main.wds.get(index).windDir;
            case WINDGUST:
                return Main.wds.get(index).windGust;
            case SEALVL:
                return Main.wds.get(index).seaLvlPressure;
            default:
                return null;
        }
    }

    public static void fillLongGap(int gapIndex, int gapSize, int whichVariable) {
        // gapIndex - gapSize is the start of the gap
        // gapIndex - 1 is the end of the gap

        // if (gapIndex - gapSize < 0) return; // this gap is at the beginning of the file and there's no way of filling in this gap
        try {
            for (int i = gapIndex - gapSize; i < gapIndex; i++) {
                Reading prev = getReading(i - 48, whichVariable); //Main.wds.get(gapIndex - 48).precip;
                Reading next = getReading(i + 48, whichVariable); //Main.wds.get(gapIndex + 48).precip;
                if (prev.isValid && next.isValid) {
                    Reading curr = getReading(i, whichVariable); // Main.wds.get(i).precip;
                    curr.value = (prev.value + next.value) / 2;
                    curr.isValid = true;
//                System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + gapSize + " for " + curr.varName);
                }
            }
        } catch (IndexOutOfBoundsException e) {
//            System.out.println("Cannot fill the long gap at index " + gapIndex + " because either the previous or next day is out of bounds");
        }


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
//                    System.out.println("At index " + gapIndex + " we interpolated this gap of length " + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[arrayIndex] <= 48) {
                    // just leave precipitation as it is
                    if (arrayIndex != PRECIP)
                        fillLongGap(gapIndex, counter[arrayIndex],arrayIndex);
                    // System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // no rule specified in sandia method for gaps this big
                else {
//                    System.out.println("At index " + gapIndex + " we can't take care of this gap of length " + (counter[arrayIndex]) + " for " + whichVariable.varName);
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

    public static List<WeatherData> averageValues(String station) {
//        System.out.println("Now averaging the data");
        LocalDateTime currDateTIme = Main.wds.get(0).dateTime;
        List<WeatherData> res = new ArrayList<>();
        double[] sums = new double[11];
        int[] readingsCount = new int[11];
        boolean[] validAverages = new boolean[11];
        for (int i = 0; i < Main.wds.size(); i++) {
            WeatherData w = Main.wds.get(i);

            // check for all the time values in that hour, once we move to the next hour, average them
            if (currDateTIme.getHour() != w.hrsStd) {
                // create a new WeatherData object with the averaged value, with the current dateTime
                // add to the list res
                // refresh the sums array back to 0
                // set readings count back to 0
                WeatherData wd = new ActualWD(currDateTIme, null);
//                System.out.println(wd);
                for (int j = 0; j < 11; j++) {
                    Reading reading = wd.getReading(j);
                    // if it's precipitation, then add the values
                    // TODO: check for other variables that may need to be added rather than averaged
                    reading.value = (j == 0) ? sums[j] : FillGaps.average(sums[j], readingsCount[j]);
                    reading.isValid = true;
                    sums[j] = 0;
                    readingsCount[j] = 0;
                    validAverages[j] = false;
                }
                res.add(wd);
                currDateTIme = currDateTIme.plusHours(1);
            }

            for (int j = 0; j < 11; j++) {
                Reading r = getReading(i, j);
                if (r.isValid) {
                    validAverages[j] = true;
                    sums[j] += r.value;
                    readingsCount[j]++;
                }
            }
        }

        return res;
    }
}
