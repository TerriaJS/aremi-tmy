import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class FillGapsSolar {

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

                System.out.println("At index " + i + ", we found a timestamp gap and took care of it");
            }
        }
    }
}
