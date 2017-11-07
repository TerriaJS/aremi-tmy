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

    public void averageValues(WeatherData toCombine) {
        this.airTemp.value = (this.airTemp.value + toCombine.airTemp.value) / 2;
        this.airTemp.count += toCombine.airTemp.count;

        this.humidity.value = (this.humidity.value + toCombine.humidity.value) / 2; // humidity doesn't have count

        this.windSpeed.value = (this.windSpeed.value + toCombine.windSpeed.value) / 2;
        this.windSpeed.count += toCombine.windSpeed.count;

        this.windDir.value = (this.windDir.value + toCombine.windDir.value) / 2;
        this.windDir.count += toCombine.windDir.count;

        this.windGust.value = (this.windGust.value + toCombine.windGust.value) / 2;
        this.windGust.count += toCombine.windGust.count;
    }

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
