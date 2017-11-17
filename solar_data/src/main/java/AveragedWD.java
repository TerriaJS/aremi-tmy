import java.text.DecimalFormat;
import java.time.LocalDateTime;

public class AveragedWD extends WeatherData {

    public AveragedWD(LocalDateTime dt, String[] data) {

        super(dt, data);

        boolean isGap;

        isGap = checkParsable(dataString[12]) && checkQuality(dataString[13]);
        airTemp = new Reading("Air temperature", isGap ? Double.parseDouble(dataString[12]) : 0, dataString[13], Integer.parseInt(dataString[14]), isGap);

        isGap = checkParsable(dataString[15]) && checkQuality(dataString[16]);
        humidity = new Reading("Humidity", isGap ? Double.parseDouble(dataString[15]) : 0, dataString[16], isGap);

        isGap = checkParsable(dataString[17]) && checkQuality(dataString[18]);
        windSpeed = new Reading("Wind speed", isGap ? Double.parseDouble(dataString[17]) : 0, dataString[18], Integer.parseInt(dataString[19]), isGap);

        isGap = checkParsable(dataString[20]) && checkQuality(dataString[21]);
        windGust = new Reading("Wind gust", isGap ? Double.parseDouble(dataString[20]) : 0, dataString[21], Integer.parseInt(dataString[22]), isGap);

        isGap = checkParsable(dataString[23]) && checkQuality(dataString[24]);
        windDir = new Reading("Wind direction", isGap ? Double.parseDouble(dataString[23]) : 0, dataString[24], Integer.parseInt(dataString[25]), isGap);

    }

    // map which the enum variable to the attributes in WeatherData
    public Reading getReading(WeatherVar whichVariable) {
        switch (whichVariable) {
            case AIRTEMP:
                return this.airTemp;
            case HUMIDITY:
                return this.humidity;
            case WINDSPD:
                return this.windSpeed;
            case WINDDIR:
                return this.windDir;
            case WINDGUST:
                return this.windGust;
            default:
                return null;
        }
    }

    // checks if the given enum variable is a variable of this dataset
    public boolean containsVar(WeatherVar variable) {
        return variable == WeatherVar.AIRTEMP ||
                variable == WeatherVar.HUMIDITY ||
                variable == WeatherVar.WINDSPD ||
                variable == WeatherVar.WINDDIR ||
                variable == WeatherVar.WINDGUST;
    }

    // since updating of values take place in the class attributes rather than the string
    // this function is used to update the String[] with the updated values
    public String[] combineValues() {
        alignCommonVariables();
        DecimalFormat df = new DecimalFormat("#.#");
        dataString[12] = rightAlign(df.format(airTemp.value), 6);
        dataString[13] = rightAlign(airTemp.quality, 1);
        dataString[14] = rightAlign(Integer.toString(airTemp.count), 3);

        dataString[15] = rightAlign(df.format(humidity.value), 6);
        dataString[16] = rightAlign(humidity.quality, 1);

        dataString[17] = rightAlign(df.format(windSpeed.value), 6);
        dataString[18] = rightAlign(windSpeed.quality, 1);
        dataString[19] = rightAlign(Integer.toString(windSpeed.count), 3);

        dataString[20] = rightAlign(df.format(windGust.value), 6);
        dataString[21] = rightAlign(windGust.quality, 1);
        dataString[22] = rightAlign(Integer.toString(windGust.count), 3);

        dataString[23] = rightAlign(df.format(windDir.value), 6);
        dataString[24] = rightAlign(windDir.quality, 1);
        dataString[25] = rightAlign(Integer.toString(windDir.count), 3);

        return dataString;
    }
}
