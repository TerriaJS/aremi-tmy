public abstract class WeatherData {

    public static final String ALLOWEDQUALITY = "YNSF";

    String recordIdentifier, stationId;
    int year, month, day, hrs, mins, yearStd, monthStd, dayStd, hrsStd, minsStd;
    Reading precip, airTemp, wbTemp, dpTemp,
            humidity, vapPressure, satVapPressure, windSpeed,
            windDir, windGust, seaLvlPressure;

    public String[] dataString;

    public WeatherData(String[] data) {
        // get rid of leading whitespaces
        for (int i = 0; i < data.length; i++) {
            data[i] = data[i].trim();
        }
        this.dataString = data;

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
    }

    public abstract boolean checkQuality();
    public abstract String[] combineValues();
    public abstract void averageValues(WeatherData toCombine);

    public String rightAlign(String str, int intendedLength) {
        return String.format("%1$" + intendedLength + "s", str);
    }


}
