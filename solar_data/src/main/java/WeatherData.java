import java.time.LocalDateTime;

public abstract class WeatherData {

    public static final String ALLOWEDQUALITY = "YNSF";

    String recordIdentifier, stationId;
    LocalDateTime dateTime;
    int year, month, day, hrs, mins, yearStd, monthStd, dayStd, hrsStd, minsStd;
    Reading precip, airTemp, wbTemp, dpTemp,
            humidity, vapPressure, satVapPressure, windSpeed,
            windDir, windGust, seaLvlPressure;

    public String[] dataString;

    public WeatherData(LocalDateTime dateTime, String[] data) {
        // get rid of leading whitespaces
        for (int i = 0; i < data.length; i++) {
            data[i] = (data[i] == null) ? "" : data[i].trim();
        }

        this.dateTime = dateTime;
        this.dataString = data;

        this.recordIdentifier = data[0];
        this.stationId = data[1];

        this.year = Integer.parseInt(data[2]);
        this.month = Integer.parseInt(data[3]);
        this.day = Integer.parseInt(data[4]);
        this.hrs = Integer.parseInt(data[5]);
        this.mins = Integer.parseInt(data[6]);
        this.yearStd = Integer.parseInt(data[7]);
        this.monthStd = Integer.parseInt(data[8]);
        this.dayStd = Integer.parseInt(data[9]);
        this.hrsStd = Integer.parseInt(data[10]);
        this.minsStd = Integer.parseInt(data[11]);
    }

    public abstract boolean checkQuality();
    public abstract String[] combineValues();
    public abstract void averageValues(WeatherData toCombine);

    public String rightAlign(String str, int intendedLength) {
        return String.format("%1$" + intendedLength + "s", str);
    }

    public boolean checkParsable(String str) {
        if (str == null || str.equals("")) return false;
        for (int i = 0; i < str.length(); i++) {
            if (!Character.isDigit(str.charAt(i)) && !(str.charAt(i) == '.')) {
                return false;
            }
        }
        return true;
    }


}
