import java.text.DecimalFormat;
import java.time.LocalDateTime;

public class ActualWDBrief extends WeatherData {

    public ActualWDBrief(LocalDateTime dt, String[] data) {
        super(dt, data);

        boolean isGap;

        // Precipitation since 9am local time in mm
        isGap = checkParsable(dataString[12]) && checkQuality(dataString[13]);
        precip = new Reading("Precipitation", checkParsable(dataString[12]) ? Double.parseDouble(dataString[12]) : 0, dataString[13], isGap);

        // Air Temperature in degrees C
        isGap = checkParsable(dataString[14]) && checkQuality(dataString[15]);
        airTemp = new Reading("Air temperature", checkParsable(dataString[14]) ? Double.parseDouble(dataString[14]) : 0, dataString[15], isGap);

        // Dew point temperature in degrees C
        isGap = checkParsable(dataString[16]) && checkQuality(dataString[17]);
        dpTemp = new Reading("Dew point temperature", checkParsable(dataString[16]) ? Double.parseDouble(dataString[16]) : 0, dataString[17], isGap);

        // Relative humidity in percentage %
        isGap = checkParsable(dataString[18]) && checkQuality(dataString[19]);
        humidity = new Reading("Humidity", checkParsable(dataString[18]) ? Double.parseDouble(dataString[18]) : 0, dataString[19], isGap);

        // Wind speed in km/h
        isGap = checkParsable(dataString[20]) && checkQuality(dataString[21]);
        windSpeed = new Reading("Wind speed", checkParsable(dataString[20]) ? Double.parseDouble(dataString[20]) : 0, dataString[21], isGap);

        // Wind direction in degrees
        isGap = checkParsable(dataString[22]) && checkQuality(dataString[23]);
        windDir = new Reading("Wind direction", checkParsable(dataString[22]) ? Double.parseDouble(dataString[22]) : 0, dataString[23], isGap);

        // Speed of maximum wind gust in last 10 minutes in  km/h
        isGap = checkParsable(dataString[24]) && checkQuality(dataString[25]);
        windGust = new Reading("Wind gust", checkParsable(dataString[24]) ? Double.parseDouble(dataString[24]) : 0, dataString[25], isGap);

        // Mean sea level pressure in hPa
        isGap = checkParsable(dataString[26]) && checkQuality(dataString[27]);
        seaLvlPressure = new Reading("Sea level pressure", checkParsable(dataString[26]) ? Double.parseDouble(dataString[26]) : 0, dataString[27], isGap);
    }

    public void averageValues(WeatherData toCombine) {
        if (toCombine instanceof ActualWD) {
            this.airTemp.value = (this.airTemp.value + toCombine.airTemp.value) / 2;
            this.humidity.value = (this.humidity.value + toCombine.humidity.value) / 2;
            this.windSpeed.value = (this.windSpeed.value + toCombine.windSpeed.value) / 2;
            this.windDir.value = (this.windDir.value + toCombine.windDir.value) / 2;
            this.windGust.value = (this.windGust.value + toCombine.windGust.value) / 2;
            this.precip.value = (this.precip.value + toCombine.precip.value) / 2;
            this.dpTemp.value = (this.dpTemp.value + toCombine.dpTemp.value) / 2;
            this.seaLvlPressure.value = (this.seaLvlPressure.value + toCombine.seaLvlPressure.value) / 2;
        }
    }

    public Reading getReading(WeatherVar whichVariable) {
        switch (whichVariable) {
            case PRECIP:
                return this.precip;
            case DPTEMP:
                return this.dpTemp;
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
            case SEALVL:
                return this.seaLvlPressure;
            default:
                return null;
        }
    }

    public boolean containsVar(WeatherVar variable) {
        return variable == WeatherVar.PRECIP ||
                variable == WeatherVar.DPTEMP ||
                variable == WeatherVar.AIRTEMP ||
                variable == WeatherVar.HUMIDITY ||
                variable == WeatherVar.WINDSPD ||
                variable == WeatherVar.WINDDIR ||
                variable == WeatherVar.WINDGUST ||
                variable == WeatherVar.SEALVL;
    }

    public String[] combineValues() {
        alignCommonVariables();
        DecimalFormat df = new DecimalFormat("#.#");
        dataString[12] = rightAlign(df.format(precip.value), 6);
        dataString[13] = rightAlign(precip.quality, 1);

        dataString[14] = rightAlign(df.format(airTemp.value), 6);
        dataString[15] = rightAlign(airTemp.quality, 1);

        dataString[16] = rightAlign(df.format(dpTemp.value), 6);
        dataString[17] = rightAlign(dpTemp.quality, 1);

        dataString[18] = rightAlign(df.format(humidity.value), 6);
        dataString[19] = rightAlign(humidity.quality, 1);

        dataString[20] = rightAlign(df.format(windSpeed.value), 6);
        dataString[21] = rightAlign(windSpeed.quality, 1);

        dataString[22] = rightAlign(df.format(windDir.value), 6);
        dataString[23] = rightAlign(windDir.quality, 1);

        dataString[24] = rightAlign(df.format(windGust.value), 6);
        dataString[25] = rightAlign(windGust.quality, 1);

        dataString[26] = rightAlign(df.format(seaLvlPressure.value), 6);
        dataString[27] = rightAlign(seaLvlPressure.quality, 1);

        dataString[28] = rightAlign(dataString[28], 1);

        return dataString;
    }
}
