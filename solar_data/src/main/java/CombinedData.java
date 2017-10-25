import java.time.format.DateTimeFormatter;

public class CombinedData {

    String[] dataString;
    SolarData sd;
    WeatherData wd;



    // only works for ActualWD for now
    // TODO: make it work for other kinds of weather data (the different classes)
    public CombinedData(SolarData sd, WeatherData wd) {
        this.sd = sd;
        this.wd = wd;

        dataString = new String[15];

        dataString[0] = wd.stationId;
        dataString[1] = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(wd.dateTime);

        // take the DNI and GHI values
        dataString[2] = sd.dataString[1];
        dataString[3] = sd.dataString[2];

        // go through the values from weather data String[] and copy it over to the new dataString
        for (int i = 0; i < 11; i++) {
            dataString[i + 4] = wd.dataString[(i * 2) + 12];
        }
    }
}
