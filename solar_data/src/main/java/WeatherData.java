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
            if (!Character.isDigit(str.charAt(i)) && !(str.charAt(i) == '.') && !(str.charAt(i) == '-')) {
                return false;
            }
        }
        return true;
    }

    public void alignCommonVariables() {
        dataString[0] = rightAlign(recordIdentifier, 1);
        dataString[1] = rightAlign(stationId, 6);

        dataString[2] = rightAlign(Integer.toString(year), 4);
        dataString[3] = rightAlign(Integer.toString(month), 2);
        dataString[4] = rightAlign(Integer.toString(day), 2);
        dataString[5] = rightAlign(Integer.toString(hrs), 2);
        dataString[6] = rightAlign(Integer.toString(mins), 2);

        dataString[7] = rightAlign(Integer.toString(yearStd), 4);
        dataString[8] = rightAlign(Integer.toString(monthStd), 2);
        dataString[9] = rightAlign(Integer.toString(dayStd), 2);
        dataString[10] = rightAlign(Integer.toString(hrsStd), 2);
        dataString[11] = rightAlign(Integer.toString(minsStd), 2);


    }


}
