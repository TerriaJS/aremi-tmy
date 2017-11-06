import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

// only works for weather data for now
// TODO: make it work for other datasets
public class FillGapsWeather {
//
//    public static final int PRECIP = 0;
//    public static final int AIRTEMP = 1;
//    public static final int WBTEMP = 2;
//    public static final int DPTEMP = 3;
//    public static final int HUMIDITY = 4;
//    public static final int VAP = 5;
//    public static final int SATVAP = 6;
//    public static final int WINDSPD = 7;
//    public static final int WINDDIR = 8;
//    public static final int WINDGUST = 9;
//    public static final int SEALVL = 10;

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
        if (from < 0) {
            System.out.println("Can't do linear interpolation because the gap is at the beginning of the file");
            return; // this gap is at the beginning of the file and there's no way of interpolating the data
        }

        Reading prevReading = Main.wds.get(from).getReading(whichVariable);
        Reading nextReading = Main.wds.get(to).getReading(whichVariable);

        double[] values = FillGaps.linearInterpolate(prevReading.value, nextReading.value, gapSize);

        //double[] values = linearInterpolate(from, to, gapSize);
        // how to fill in the gaps:
        // take the gap count and the array returned by linear interpolation
        // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
        for (int i = 1; i <= gapSize; i++) {
            Reading currReading = Main.wds.get(from + i).getReading(whichVariable);
            currReading.value = values[i];
            currReading.v = Value.Filled;
            currReading.fillCount++;
        }
    }

//    // map which variable to the attributes in WeatherData
//    public static Reading getReading(int index, int whichVariable) {
//        switch (whichVariable) {
//            case PRECIP:
//                return Main.wds.get(index).precip;
//            case WBTEMP:
//                return Main.wds.get(index).wbTemp;
//            case DPTEMP:
//                return Main.wds.get(index).dpTemp;
//            case AIRTEMP:
//                return Main.wds.get(index).airTemp;
//            case HUMIDITY:
//                return Main.wds.get(index).humidity;
//            case VAP:
//                return Main.wds.get(index).vapPressure;
//            case SATVAP:
//                return Main.wds.get(index).satVapPressure;
//            case WINDSPD:
//                return Main.wds.get(index).windSpeed;
//            case WINDDIR:
//                return Main.wds.get(index).windDir;
//            case WINDGUST:
//                return Main.wds.get(index).windGust;
//            case SEALVL:
//                return Main.wds.get(index).seaLvlPressure;
//            default:
//                return null;
//        }
//    }

    public static void fillLongGap(int gapIndex, int gapSize, int whichVariable) {
        // gapIndex - gapSize is the start of the gap
        // gapIndex - 1 is the end of the gap

        // if (gapIndex - gapSize < 0) return; // this gap is at the beginning of the file and there's no way of filling in this gap
        try {
            for (int i = gapIndex - gapSize; i < gapIndex; i++) {
                Reading prev = Main.wds.get(i - 48).getReading(whichVariable);
                Reading next = Main.wds.get(i + 48).getReading(whichVariable);
                if (prev.isValid() && next.isValid()) {
                    Reading curr = Main.wds.get(i).getReading(whichVariable);
                    curr.value = (prev.value + next.value) / 2;
                    curr.v = Value.Filled;
                    curr.fillCount++;
//                System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + gapSize + " for " + curr.varName);

                }
            }
        } catch (IndexOutOfBoundsException e) {
//            System.out.println("Cannot fill the long gap at index " + gapIndex + " because either the previous or next day is out of bounds");
        }


    }

    public static void handleSmallGaps(int gapIndex, int arrayIndex) {
        Reading r = Main.wds.get(gapIndex).getReading(arrayIndex);
        if (!r.isValid()) {
            counter[arrayIndex]++;
        } else {
            if (counter[arrayIndex] > 0) {
                // do linear interpolation if gap less than 5 hours
                if (counter[arrayIndex] <= 10) {
                    fillShortGap(gapIndex - counter[arrayIndex] - 1, gapIndex, counter[arrayIndex], arrayIndex);
//                    System.out.println("At index " + gapIndex + " we interpolated this gap of length " + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }
                counter[arrayIndex] = 0;
            }
        }
    }

    public static void handleBigGaps(int gapIndex, int arrayIndex) {
        Reading r = Main.wds.get(gapIndex).getReading(arrayIndex);
        if (!r.isValid()) {
            counter[arrayIndex]++;
        } else {
            if (counter[arrayIndex] > 0) {
                // something is wrong if we find an uninterpolated gap of less than 5 hours in the middle of the file,
                // but it is okay if it is at the beginning of the file because we cannot interpolate at the beginning
                if (counter[arrayIndex] <= 10 && gapIndex < 10) {
                    System.out.println("Somehow we found an uninterpolated gap of less than 5 hours in the middle of the file for " + r.varName + " at index " + gapIndex + "!");
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[arrayIndex] <= 48) {
                    // just leave precipitation as it is
                    if (arrayIndex != WeatherVar.PRECIP.ordinal())
                        fillLongGap(gapIndex, counter[arrayIndex],arrayIndex);
                    // System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // don't do anything to gaps over 24 hours and just refresh the gap count
                counter[arrayIndex] = 0;
            }
        }
    }

    public static void checkForGaps(List<WeatherData> list) {
        // check every WeatherData object in the list
        // check every variable of the WeatherData object
        // record the ones that are valid and invalid data
        // linear interpolate for gaps that are less than 5 hours
        for (int i = 0; i < list.size(); i++) {
            for (int j = 0; j < 11; j++) {
                handleSmallGaps(i, j);
            }
        }

        // check every WeatherData object in the list
        // check every variable of the WeatherData object
        // record the ones that are valid and invalid data
        // linear interpolate for gaps that are less than 5 hours
        for (int i = 0; i < list.size(); i++) {
            for (int j = 0; j < 11; j++) {
                handleBigGaps(i, j);
            }
        }
    }

    public static List<WeatherData> averageValues() {
//        System.out.println("Now averaging the data");
        LocalDateTime currDateTIme = Main.wds.get(0).dateTime;
        List<WeatherData> res = new ArrayList<>();
        double[] sums = new double[11];
        int[] readingsCount = new int[11];
        int[] gapsCount = new int[11];
        for (int i = 0; i < Main.wds.size(); i++) {
            WeatherData w = Main.wds.get(i);

            // check for all the time values in that hour, once we move to the next hour, average them
            if (currDateTIme.getHour() != w.hrsStd) {
                // create a new WeatherData object with the averaged value, with the current dateTime
                // add to the list res
                // refresh the sums array back to 0
                // set readings count back to 0


                // if we are dealing with NT or SA states, we want to average them into hourly at the 30 minute mark so that it matches
                // the solar data offset (since their timezone is offset by 9h30min)
                LocalDateTime newdt;
                if (Main.stateName.equals("NT") || Main.stateName.equals("SA"))
                    newdt = LocalDateTime.of(currDateTIme.getYear(), currDateTIme.getMonth(), currDateTIme.getDayOfMonth(), currDateTIme.getHour(), 30, currDateTIme.getSecond());
                // otherwise any other state can be averaged into hourly at the 00 minute mark
                else
                    newdt = LocalDateTime.of(currDateTIme.getYear(), currDateTIme.getMonth(), currDateTIme.getDayOfMonth(), currDateTIme.getHour(), 0, currDateTIme.getSecond());
                WeatherData wd = new ActualWD(newdt, null);

                for (int j = 0; j < 11; j++) {
                    Reading reading = wd.getReading(j);
                    // if it's precipitation, then add the values
                    reading.value = (j == 0) ? sums[j] : FillGaps.average(sums[j], readingsCount[j] + gapsCount[j]);
                    if (readingsCount[j] == 0 && gapsCount[j] == 0) reading.v = Value.Invalid;
                    else reading.v = Value.Valid;
                    reading.count = readingsCount[j];
                    reading.fillCount = gapsCount[j];
                    sums[j] = 0;
                    readingsCount[j] = 0;
                    gapsCount[j] = 0;

                }
                res.add(wd);
                currDateTIme = currDateTIme.plusHours(1);
            }

            for (int j = 0; j < 11; j++) {
                Reading r = Main.wds.get(i).getReading(j);
                if (r.isValid()) {
                    sums[j] += r.value;

                    // if the value is an actual value recorded by BoM, then add to the given reading count
                    // if the value is a filled value, then add to the gap count

                    switch (r.v) {
                        case Valid:
                            readingsCount[j]++;
                            break;
                        case Filled:
                            gapsCount[j]++;
                            break;
                        case Invalid:
                            break;
                    }
                }
            }
        }

        return res;
    }
}
