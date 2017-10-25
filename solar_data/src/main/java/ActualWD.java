import java.time.LocalDateTime;

public class ActualWD extends WeatherData {

    public ActualWD(LocalDateTime dt, String[] data) {
        super(dt, data);

        boolean isGap;

        // Precipitation since 9am local time in mm
        isGap = checkParsable(dataString[12]) && checkQuality(dataString[13]);
        precip = new Reading("Precipitation", isGap ? Double.parseDouble(dataString[12]) : 0, dataString[13], isGap);

        // Air Temperature in degrees C
        isGap = checkParsable(dataString[14]) && checkQuality(dataString[15]);
        airTemp = new Reading("Air temperature", isGap ? Double.parseDouble(dataString[14]) : 0, dataString[15], isGap);

        // Wet bulb temperature in degrees C
        isGap = checkParsable(dataString[16]) && checkQuality(dataString[17]);
        wbTemp = new Reading("Wet bulb temperature", isGap ? Double.parseDouble(dataString[16]) : 0, dataString[17], isGap);

        // Dew point temperature in degrees C
        isGap = checkParsable(dataString[18]) && checkQuality(dataString[19]);
        dpTemp = new Reading("Dew point temperature", isGap ? Double.parseDouble(dataString[18]) : 0, dataString[19], isGap);

        // Relative humidity in percentage %
        isGap = checkParsable(dataString[20]) && checkQuality(dataString[21]);
        humidity = new Reading("Humidity", isGap ? Double.parseDouble(dataString[20]) : 0, dataString[21], isGap);

        // Vapour pressure in hPa
        isGap = checkParsable(dataString[22]) && checkQuality(dataString[23]);
        vapPressure = new Reading("Vapour pressure", isGap ? Double.parseDouble(dataString[22]) : 0, dataString[23], isGap);

        // Saturated vapour pressure in hPa
        isGap = checkParsable(dataString[24]) && checkQuality(dataString[25]);
        satVapPressure = new Reading("Saturated vapour pressure", isGap ? Double.parseDouble(dataString[24]) : 0, dataString[25],isGap);

        // Wind speed in km/h
        isGap = checkParsable(dataString[26]) && checkQuality(dataString[27]);
        windSpeed = new Reading("Wind speed", isGap ? Double.parseDouble(dataString[26]) : 0, dataString[27], isGap);

        // Wind direction in degrees
        isGap = checkParsable(dataString[28]) && checkQuality(dataString[29]);
        windDir = new Reading("Wind direction", isGap ? Double.parseDouble(dataString[28]) : 0, dataString[29], isGap);

        // Speed of maximum wind gust in last 10 minutes in  km/h
        isGap = checkParsable(dataString[30]) && checkQuality(dataString[31]);
        windGust = new Reading("Wind gust", isGap ? Double.parseDouble(dataString[30]) : 0, dataString[31], isGap);

        // Mean sea level pressure in hPa
        isGap = checkParsable(dataString[32]) && checkQuality(dataString[33]);
        seaLvlPressure = new Reading("Sea level pressure", isGap ? Double.parseDouble(dataString[32]) : 0, dataString[33], isGap);
    }

    public void averageValues(WeatherData toCombine) {
        if (toCombine instanceof ActualWD) {
            this.airTemp.value = (this.airTemp.value + toCombine.airTemp.value) / 2;
            this.humidity.value = (this.humidity.value + toCombine.humidity.value) / 2;
            this.windSpeed.value = (this.windSpeed.value + toCombine.windSpeed.value) / 2;
            this.windDir.value = (this.windDir.value + toCombine.windDir.value) / 2;
            this.windGust.value = (this.windGust.value + toCombine.windGust.value) / 2;
            this.precip.value = (this.precip.value + toCombine.precip.value) / 2;
            this.vapPressure.value = (this.vapPressure.value + toCombine.vapPressure.value) / 2;
            this.satVapPressure.value = (this.satVapPressure.value + toCombine.satVapPressure.value) / 2;
            this.wbTemp.value = (this.wbTemp.value + toCombine.wbTemp.value) / 2;
            this.dpTemp.value = (this.dpTemp.value + toCombine.dpTemp.value) / 2;
            this.seaLvlPressure.value = (this.seaLvlPressure.value + toCombine.seaLvlPressure.value) / 2;
        }
    }

    // map which variable to the attributes in WeatherData
    public Reading getReading(int whichVariable) {
        switch (whichVariable) {
            case 0:
                return this.precip;
            case 1:
                return this.wbTemp;
            case 2:
                return this.dpTemp;
            case 3:
                return this.airTemp;
            case 4:
                return this.humidity;
            case 5:
                return this.vapPressure;
            case 6:
                return this.satVapPressure;
            case 7:
                return this.windSpeed;
            case 8:
                return this.windDir;
            case 9:
                return this.windGust;
            case 10:
                return this.seaLvlPressure;
            default:
                return null;
        }
    }

    public String[] combineValues() {
        alignCommonVariables();
        //DecimalFormat df = new DecimalFormat("#.#");
        dataString[12] = parseValue(precip);
        dataString[13] = rightAlign(precip.quality, 1);

        dataString[14] = parseValue(airTemp);
        dataString[15] = rightAlign(airTemp.quality, 1);

        dataString[16] = parseValue(wbTemp);
        dataString[17] = rightAlign(wbTemp.quality, 1);

        dataString[18] = parseValue(dpTemp);
        dataString[19] = rightAlign(dpTemp.quality, 1);

        dataString[20] = parseValue(humidity);
        dataString[21] = rightAlign(humidity.quality, 1);

        dataString[22] = parseValue(vapPressure);
        dataString[23] = rightAlign(vapPressure.quality, 1);

        dataString[24] = parseValue(satVapPressure);
        dataString[25] = rightAlign(satVapPressure.quality, 1);

        dataString[26] = parseValue(windSpeed);
        dataString[27] = rightAlign(windSpeed.quality, 1);

        dataString[28] = parseValue(windDir);
        dataString[29] = rightAlign(windDir.quality, 1);

        dataString[30] = parseValue(windGust);
        dataString[31] = rightAlign(windGust.quality, 1);

        dataString[32] = parseValue(seaLvlPressure);
        dataString[33] = rightAlign(seaLvlPressure.quality, 1);

        dataString[34] = rightAlign(dataString[34], 1);

        return dataString;
    }
}
