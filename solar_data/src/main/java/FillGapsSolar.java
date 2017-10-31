import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class FillGapsSolar {

    private static final int DNI = 0;
    private static final int GHI = 1;

    private static final int[] counter = new int[2];

    public static void fillMissingTimeStamp() {
        LocalDateTime currDateTIme = Main.sds.get(0).dateTime;
        for (int i = 1; i < Main.sds.size(); i++) {
            currDateTIme = currDateTIme.plusHours(1);
            if (!Main.sds.get(i).dateTime.equals(currDateTIme)) {
                // we found a gap, meaning we skipped one half-hourly reading

                // create a weather data object and add it to the list
                String[] solarReadings = new String[3];
                solarReadings[0] = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(currDateTIme);

                Main.sds.add(i, new SolarData(currDateTIme, solarReadings));

//                System.out.println("At index " + i + ", we found a timestamp gap and took care of it");
            }
        }
    }

    // map which variable to the attributes in WeatherData
    public static Reading getReading(int index, int whichVariable) {
        switch (whichVariable) {
            case DNI:
                return Main.sds.get(index).dni;
            case GHI:
                return Main.sds.get(index).ghi;
            default:
                return null;
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
            currReading.v = Value.Filled;
            currReading.fillCount++;

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
                if (prev.isValid() && next.isValid()) {
                    Reading curr = getReading(i, whichVariable); // Main.wds.get(i).precip;
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
        Reading r = getReading(gapIndex, arrayIndex);
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
        Reading r = getReading(gapIndex, arrayIndex);
        if (!r.isValid()) {
            counter[arrayIndex]++;
        } else {
            if (counter[arrayIndex] > 0) {
                // do linear interpolation if gap less than 5 hours
                if (counter[arrayIndex] <= 10) {
                    System.out.println("Somehow we found an uninterpolated gap of less than 5 hours!");
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[arrayIndex] <= 48) {
                    fillLongGap(gapIndex, counter[arrayIndex],arrayIndex);
                    // System.out.println("At index " + gapIndex + " we took averages of prev and next day for this gap of length "  + (counter[arrayIndex]) + " for " + whichVariable.varName);
                }

                // don't do anything to gaps over 24 hours and just refresh the gap count
                counter[arrayIndex] = 0;
            }
        }
    }

    public static void findGaps(List<SolarData> list) {
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
            handleSmallGaps(i, DNI);
            handleSmallGaps(i, GHI);
        }

        for (int i = 0; i < list.size(); i++) {
            handleBigGaps(i, DNI);
            handleBigGaps(i, GHI);
        }
    }
}
