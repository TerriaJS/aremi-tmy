import java.time.format.DateTimeFormatter;

public class CombinedData {

    String[] dataString;
    SolarData sd;
    WeatherData wd;



    // only works for ActualWD and ActualWDBrief for now
    // TODO: make it work for other kinds of weather data (AveragedWD)
    public CombinedData(SolarData sd, WeatherData wd) {
        this.sd = sd;
        this.wd = wd;

        dataString = new String[41];

        dataString[0] = wd.stationId;
        dataString[1] = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(wd.dateTime);

        // go through the values from solar data String[] and copy it over to the new dataString
        // keep track of the counts and fill counts
        for (int i = 0; i < 2; i++) {
            dataString[3 * i + 2] = sd.dataString[i + 1];
            dataString[3 * i + 3] = Integer.toString(sd.getReading(SolarVar.values()[i]).count);
            dataString[3 * i + 4] = Integer.toString(sd.getReading(SolarVar.values()[i]).fillCount);
        }

        // go through the values from weather data String[] and copy it over to the new dataString
        // keep track of the counts and fill counts
        for (int i = 0; i < 11; i++) {
            dataString[3 * i + 8] = wd.dataString[(i * 2) + 12];
            dataString[3 * i + 9] = Integer.toString(wd.getReading(WeatherVar.values()[i]).count);
            dataString[3 * i + 10] = Integer.toString(wd.getReading(WeatherVar.values()[i]).fillCount);
        }
    }
}
