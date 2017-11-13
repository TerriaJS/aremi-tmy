import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

// only works for weather data for now
// TODO: make it work for other datasets
public class FillGapsWeather {

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
            }
        }
    }

    public static void fillShortGap(int from, int to, int gapSize, WeatherVar wv) {
        if (from < 0) {
            System.out.println("Can't do linear interpolation because the gap is at the beginning of the file");
            return; // this gap is at the beginning of the file and there's no way of interpolating the data
        }

        Reading prevReading = Main.wds.get(from).getReading(wv);
        Reading nextReading = Main.wds.get(to).getReading(wv);

        double[] values = FillGaps.linearInterpolate(prevReading.value, nextReading.value, gapSize);

        // how to fill in the gaps:
        // take the gap count and the array returned by linear interpolation
        // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
        for (int i = 1; i <= gapSize; i++) {
            Reading currReading = Main.wds.get(from + i).getReading(wv);
            currReading.value = values[i];
            currReading.v = Value.Filled;
            currReading.fillCount++;
        }
    }

    public static void fillLongGap(int gapIndex, int gapSize, WeatherVar wv) {
        // gapIndex - gapSize is the start of the gap
        // gapIndex - 1 is the end of the gap

        // if (gapIndex - gapSize < 0) return; // this gap is at the beginning of the file and there's no way of filling in this gap
        try {
            for (int i = gapIndex - gapSize; i < gapIndex; i++) {
                Reading prev = Main.wds.get(i - 48).getReading(wv);
                Reading next = Main.wds.get(i + 48).getReading(wv);
                if (prev.isValid() && next.isValid()) {
                    Reading curr = Main.wds.get(i).getReading(wv);
                    curr.value = (prev.value + next.value) / 2;
                    curr.v = Value.Filled;
                    curr.fillCount++;

                }
            }
        } catch (IndexOutOfBoundsException e) {

        }


    }

    public static void handleSmallGaps(int gapIndex, WeatherVar wv) {
        Reading r = Main.wds.get(gapIndex).getReading(wv);
        if (!r.isValid()) {
            counter[wv.ordinal()]++;
        } else {
            if (counter[wv.ordinal()] > 0) {
                // do linear interpolation if gap less than 5 hours
                if (counter[wv.ordinal()] <= 10) {
                    fillShortGap(gapIndex - counter[wv.ordinal()] - 1, gapIndex, counter[wv.ordinal()], wv);
//                    System.out.println("At index " + gapIndex + " we interpolated this gap of length " + (counter[wv]) + " for " + whichVariable.varName);
                }
                counter[wv.ordinal()] = 0;
            }
        }
    }

    public static void handleBigGaps(int gapIndex, WeatherVar wv) {
        Reading r = Main.wds.get(gapIndex).getReading(wv);
        if (!r.isValid()) {
            counter[wv.ordinal()]++;
        } else {
            if (counter[wv.ordinal()] > 0) {
                // something is wrong if we find an uninterpolated gap of less than 5 hours in the middle of the file,
                // but it is okay if it is at the beginning of the file because we cannot interpolate at the beginning
                if (counter[wv.ordinal()] <= 10 && gapIndex < 10) {
                    System.out.println("Somehow we found an uninterpolated gap of less than 5 hours in the middle of the file for " + r.varName + " at index " + gapIndex + "!");
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[wv.ordinal()] <= 48) {
                    // just leave precipitation as it is
                    if (wv != WeatherVar.PRECIP)
                        fillLongGap(gapIndex, counter[wv.ordinal()],wv);
                    // System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[wv]) + " for " + whichVariable.varName);
                }

                // don't do anything to gaps over 24 hours and just refresh the gap count
                counter[wv.ordinal()] = 0;
            }
        }
    }

    public static void checkForGaps(List<WeatherData> list) {
        // check every WeatherData object in the list
        // check every variable of the WeatherData object
        // record the ones that are valid and invalid data
        // linear interpolate for gaps that are less than 5 hours
        for (int i = 0; i < list.size(); i++) {
            for (WeatherVar wv : WeatherVar.values()) {
                if (list.get(i).containsVar(wv))
                    handleSmallGaps(i, wv);
            }
        }

        // check every WeatherData object in the list
        // check every variable of the WeatherData object
        // record the ones that are valid and invalid data
        // linear interpolate for gaps that are less than 5 hours
        for (int i = 0; i < list.size(); i++) {
            for (WeatherVar wv : WeatherVar.values()) {
                if (list.get(i).containsVar(wv))
                    handleBigGaps(i, wv);
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
//                if (Main.stateName.equals("NT") || Main.stateName.equals("SA"))
//                    newdt = LocalDateTime.of(currDateTIme.getYear(), currDateTIme.getMonth(), currDateTIme.getDayOfMonth(), currDateTIme.getHour(), 30, currDateTIme.getSecond());
//                // otherwise any other state can be averaged into hourly at the 00 minute mark
//                else
                newdt = LocalDateTime.of(currDateTIme.getYear(), currDateTIme.getMonth(), currDateTIme.getDayOfMonth(), currDateTIme.getHour(), 0, currDateTIme.getSecond());
                WeatherData wd = new ActualWD(newdt, null);

                for (int j = 0; j < 11; j++) {
                    Reading reading = wd.getReading(WeatherVar.values()[j]);
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
                Reading r = Main.wds.get(i).getReading(WeatherVar.values()[j]);
                if (r != null && r.isValid()) {
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
