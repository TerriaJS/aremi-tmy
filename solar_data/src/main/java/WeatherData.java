import java.text.DecimalFormat;

public class WeatherData {

    String recordIdentifier, stationId;
    int year, month, day, hrs, mins, yearStd, monthStd, dayStd, hrsStd, minsStd;
    AveragedReading airTemp, humidity, windSpeed, windGust, windDir;

    private String[] dataString;

    public static final String ALLOWEDQUALITY = "YNSF";

    public WeatherData(String[] data) {
        // get rid of leading whitespaces
        for (int i = 0; i < data.length; i++) {
            data[i] = data[i].trim();
        }

        recordIdentifier = data[0];
        stationId = data[1];

        year = Integer.parseInt(data[2]);
        month = Integer.parseInt(data[3]);
        day = Integer.parseInt(data[4]);
        hrs = Integer.parseInt(data[5]);
        mins = Integer.parseInt(data[6]);
        yearStd = Integer.parseInt(data[7]);
        monthStd = Integer.parseInt(data[8]);
        dayStd = Integer.parseInt(data[9]);
        hrsStd = Integer.parseInt(data[10]);
        minsStd = Integer.parseInt(data[11]);

        airTemp = new AveragedReading((!data[12].equals("")) ? Double.parseDouble(data[12]) : 0, data[13], Integer.parseInt(data[14]));
        humidity = new AveragedReading((!data[15].equals("")) ? Double.parseDouble(data[15]) : 0, data[16]);
        windSpeed = new AveragedReading((!data[17].equals("")) ? Double.parseDouble(data[17]) : 0, data[18], Integer.parseInt(data[19]));
        windGust = new AveragedReading((!data[20].equals("")) ? Double.parseDouble(data[20]) : 0, data[21], Integer.parseInt(data[22]));
        windDir = new AveragedReading((!data[23].equals("")) ? Double.parseDouble(data[23]) : 0, data[24], Integer.parseInt(data[25]));

        this.dataString = data;
    }

    public boolean checkQuality() {
        return (ALLOWEDQUALITY.contains(airTemp.quality)) &&
                (ALLOWEDQUALITY.contains(humidity.quality)) &&
                (ALLOWEDQUALITY.contains(windSpeed.quality)) &&
                (ALLOWEDQUALITY.contains(windGust.quality)) &&
                (ALLOWEDQUALITY.contains(windDir.quality));
    }

    private String rightAlign(String str, int intendedLength) {
        return String.format("%1$" + intendedLength + "s", str);
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
}
