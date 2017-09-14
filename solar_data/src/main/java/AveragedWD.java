import java.text.DecimalFormat;

public class AveragedWD extends WeatherData {


    public AveragedWD(String[] data) {

        super(data);

        airTemp = new Reading((!data[12].equals("")) ? Double.parseDouble(data[12]) : 0, data[13], Integer.parseInt(data[14]));
        humidity = new Reading((!data[15].equals("")) ? Double.parseDouble(data[15]) : 0, data[16]);
        windSpeed = new Reading((!data[17].equals("")) ? Double.parseDouble(data[17]) : 0, data[18], Integer.parseInt(data[19]));
        windGust = new Reading((!data[20].equals("")) ? Double.parseDouble(data[20]) : 0, data[21], Integer.parseInt(data[22]));
        windDir = new Reading((!data[23].equals("")) ? Double.parseDouble(data[23]) : 0, data[24], Integer.parseInt(data[25]));

    }

    public boolean checkQuality() {
        return (ALLOWEDQUALITY.contains(airTemp.quality)) &&
                (ALLOWEDQUALITY.contains(humidity.quality)) &&
                (ALLOWEDQUALITY.contains(windSpeed.quality)) &&
                (ALLOWEDQUALITY.contains(windGust.quality)) &&
                (ALLOWEDQUALITY.contains(windDir.quality));
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

    public String[] combineValues() {
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

    public boolean isValidData() {
        return airTemp.isValidReading() && humidity.isValidReading() && windSpeed.isValidReading()
                && windGust.isValidReading() && windDir.isValidReading();
    }
}
