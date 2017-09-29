import java.text.DecimalFormat;

public class ActualWD extends WeatherData {

    public ActualWD(String[] data) {
        super(data);

        // Precipitation since 9am local time in mm
        precip = new Reading(checkParsable(data[12]) ? Double.parseDouble(data[12]) : 0, data[13]);

        // Air Temperature in degrees C
        airTemp = new Reading(checkParsable(data[14]) ? Double.parseDouble(data[14]) : 0, data[15]);

        // Wet bulb temperature in degrees C
        wbTemp = new Reading(checkParsable(data[16]) ? Double.parseDouble(data[16]) : 0, data[17]);

        // Dew point temperature in degrees C
        dpTemp = new Reading(checkParsable(data[18]) ? Double.parseDouble(data[18]) : 0, data[19]);

        // Relative humidity in percentage %
        humidity = new Reading(checkParsable(data[20]) ? Double.parseDouble(data[20]) : 0, data[21]);

        // Vapour pressure in hPa
        vapPressure = new Reading(checkParsable(data[22]) ? Double.parseDouble(data[22]) : 0, data[23]);

        // Saturated vapour pressure in hPa
        satVapPressure = new Reading(checkParsable(data[24]) ? Double.parseDouble(data[24]) : 0, data[25]);

        // Wind speed in km/h
        windSpeed = new Reading(checkParsable(data[26]) ? Double.parseDouble(data[26]) : 0, data[27]);

        // Wind direction in degrees
        windDir = new Reading(checkParsable(data[28]) ? Double.parseDouble(data[28]) : 0, data[29]);

        // Speed of maximum windgust in last 10 minutes in  km/h
        windGust = new Reading(checkParsable(data[30]) ? Double.parseDouble(data[30]) : 0, data[31]);

        // Mean sea level pressure in hPa
        seaLvlPressure = new Reading(checkParsable(data[32]) ? Double.parseDouble(data[32]) : 0, data[33]);
    }

    public boolean checkQuality() {
        return (ALLOWEDQUALITY.contains(airTemp.quality)) &&
                (ALLOWEDQUALITY.contains(humidity.quality)) &&
                (ALLOWEDQUALITY.contains(windSpeed.quality)) &&
                (ALLOWEDQUALITY.contains(windGust.quality)) &&
                (ALLOWEDQUALITY.contains(windDir.quality)) &&
                (ALLOWEDQUALITY.contains(precip.quality)) &&
                (ALLOWEDQUALITY.contains(seaLvlPressure.quality)) &&
                (ALLOWEDQUALITY.contains(vapPressure.quality)) &&
                (ALLOWEDQUALITY.contains(satVapPressure.quality)) &&
                (ALLOWEDQUALITY.contains(wbTemp.quality)) &&
                (ALLOWEDQUALITY.contains(dpTemp.quality));
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

    public String[] combineValues() {
        DecimalFormat df = new DecimalFormat("#.#");
        dataString[12] = rightAlign(df.format(precip.value), 6);
        dataString[13] = rightAlign(precip.quality, 1);

        dataString[14] = rightAlign(df.format(airTemp.value), 6);
        dataString[15] = rightAlign(airTemp.quality, 1);

        dataString[16] = rightAlign(df.format(wbTemp.value), 6);
        dataString[17] = rightAlign(wbTemp.quality, 1);

        dataString[18] = rightAlign(df.format(dpTemp.value), 6);
        dataString[19] = rightAlign(dpTemp.quality, 1);

        dataString[20] = rightAlign(df.format(humidity.value), 6);
        dataString[21] = rightAlign(humidity.quality, 1);

        dataString[22] = rightAlign(df.format(vapPressure.value), 6);
        dataString[23] = rightAlign(vapPressure.quality, 1);

        dataString[24] = rightAlign(df.format(satVapPressure.value), 6);
        dataString[25] = rightAlign(satVapPressure.quality, 1);

        dataString[26] = rightAlign(df.format(windSpeed.value), 6);
        dataString[27] = rightAlign(windSpeed.quality, 1);

        dataString[28] = rightAlign(df.format(windDir.value), 6);
        dataString[29] = rightAlign(windDir.quality, 1);

        dataString[30] = rightAlign(df.format(windGust.value), 6);
        dataString[31] = rightAlign(windGust.quality, 1);

        dataString[32] = rightAlign(df.format(seaLvlPressure.value), 6);
        dataString[33] = rightAlign(seaLvlPressure.quality, 1);

        dataString[34] = rightAlign(dataString[34], 1);

        return dataString;
    }
}
