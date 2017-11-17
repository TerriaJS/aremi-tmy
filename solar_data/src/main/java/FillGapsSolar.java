import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class FillGapsSolar {

//    private static final int DNI = 0;
//    private static final int GHI = 1;

    private static final int[] counter = new int[SolarVar.values().length];

    public static void fillMissingTimeStamp() {
        try {
            LocalDateTime currDateTIme = Main.sds.get(0).dateTime;
            for (int i = 1; i < Main.sds.size(); i++) {
                currDateTIme = currDateTIme.plusHours(1);
                if (!Main.sds.get(i).dateTime.equals(currDateTIme)) {
                    // we found a gap, meaning we skipped one half-hourly reading

                    // create a weather data object and add it to the list
                    String[] solarReadings = new String[3];
                    solarReadings[0] = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(currDateTIme);

                    Main.sds.add(i, new SolarData(currDateTIme, solarReadings));

                }
            }
        } catch (IndexOutOfBoundsException e) {
            System.out.println("Index out of bounds - the list is empty");
        }
    }



    public static void fillShortGap(int from, int to, int gapSize, SolarVar sv) {
        if (from < 0) return; // this gap is at the beginning of the file and there's no way of interpolating the data

        Reading prevReading = Main.sds.get(from).getReading(sv);
        Reading nextReading = Main.sds.get(to).getReading(sv);

        double[] values = FillGaps.linearInterpolate(prevReading.value, nextReading.value, gapSize);

        // how to fill in the gaps:
        // take the gap count and the array returned by linear interpolation
        // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
        for (int i = 1; i <= gapSize; i++) {
            Reading currReading = Main.sds.get(from + i).getReading(sv);
            currReading.value = values[i];
            currReading.v = Value.Filled;
            currReading.fillCount++;

        }
    }

    public static void fillLongGap(int gapIndex, int gapSize, SolarVar sv) {
        // gapIndex - gapSize is the start of the gap
        // gapIndex - 1 is the end of the gap

        // if (gapIndex - gapSize < 0) return; // this gap is at the beginning of the file and there's no way of filling in this gap
        try {
            for (int i = gapIndex - gapSize; i < gapIndex; i++) {
                Reading prev = Main.sds.get(i - 48).getReading(sv); //Main.wds.get(gapIndex - 48).precip;
                Reading next = Main.sds.get(i + 48).getReading(sv); //Main.wds.get(gapIndex + 48).precip;
                if (prev.isValid() && next.isValid()) {
                    Reading curr = Main.sds.get(i).getReading(sv); // Main.wds.get(i).precip;
                    curr.value = (prev.value + next.value) / 2;
                    curr.v = Value.Filled;
                    curr.fillCount++;
                }
            }
        } catch (IndexOutOfBoundsException e) {
//            System.out.println("Cannot fill the long gap at index " + gapIndex + " because either the previous or next day is out of bounds");
        }


    }

    public static void handleSmallGaps(int gapIndex, SolarVar sv) {
        Reading r = Main.sds.get(gapIndex).getReading(sv);
        if (!r.isValid()) {
            counter[sv.ordinal()]++;
        } else {
            if (counter[sv.ordinal()] > 0) {
                // do linear interpolation if gap less than 5 hours
                if (counter[sv.ordinal()] <= 10) {
                    fillShortGap(gapIndex - counter[sv.ordinal()] - 1, gapIndex, counter[sv.ordinal()], sv);
                }
                counter[sv.ordinal()] = 0;
            }
        }
    }

    public static void handleBigGaps(int gapIndex, SolarVar sv) {
        Reading r = Main.sds.get(gapIndex).getReading(sv);
        if (!r.isValid()) {
            counter[sv.ordinal()]++;
        } else {
            if (counter[sv.ordinal()] > 0) {
                if (counter[sv.ordinal()] <= 10) {
                    System.out.println("Somehow we found an uninterpolated gap of less than 5 hours!");
                }

                // take average from previous and next day if gap less than 24 hours
                else if (counter[sv.ordinal()] <= 48) {
                    fillLongGap(gapIndex, counter[sv.ordinal()], sv);
                }

                // don't do anything to gaps over 24 hours and just refresh the gap count
                counter[sv.ordinal()] = 0;
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
            for (SolarVar sv : SolarVar.values())
                handleSmallGaps(i, sv);
        }

        for (int i = 0; i < list.size(); i++) {
            for (SolarVar sv : SolarVar.values())
                handleBigGaps(i, sv);
        }
    }
}
