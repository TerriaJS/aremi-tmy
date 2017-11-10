import java.text.DecimalFormat;
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

        if (data == null) {
            // create a String[] and fill in the values we know from the LocalDateTime object
            data = new String[36];
            data[0] = "hm";
            data[1] = Integer.toString(Integer.parseInt(Main.stnNum));
            data[2] = Integer.toString(dateTime.getYear());
            data[3] = Integer.toString(dateTime.getMonthValue());
            data[4] = Integer.toString(dateTime.getDayOfMonth());
            data[5] = Integer.toString(dateTime.getHour());
            data[6] = Integer.toString(dateTime.getMinute());
            // std time
            data[7] = Integer.toString(dateTime.getYear());
            data[8] = Integer.toString(dateTime.getMonthValue());
            data[9] = Integer.toString(dateTime.getDayOfMonth());
            data[10] = Integer.toString(dateTime.getHour());
            data[11] = Integer.toString(dateTime.getMinute());

            for (int i = 12; i < 35; i++) {
                data[i] = "";
            }

            data[35] = "#";
        } else {
            // get rid of leading whitespaces
            for (int i = 0; i < data.length; i++) {
                data[i] = (data[i] == null) ? "" : data[i].trim();
            }
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

    public abstract String[] combineValues();
    public abstract void averageValues(WeatherData toCombine);
    public abstract Reading getReading(WeatherVar whichVariable);

    public String rightAlign(String str, int intendedLength) {
        return String.format("%1$" + intendedLength + "s", str);
    }

    public boolean checkQuality(String quality) {
        return (ALLOWEDQUALITY.contains(quality));
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
        dataString[3] = String.format("%02d", month);
        dataString[4] = String.format("%02d", day);
        dataString[5] = String.format("%02d", hrs);
        dataString[6] = String.format("%02d", mins);

        dataString[7] = rightAlign(Integer.toString(yearStd), 4);
        dataString[8] = String.format("%02d", monthStd);
        dataString[9] = String.format("%02d", dayStd);
        dataString[10] = String.format("%02d", hrsStd);
        dataString[11] = String.format("%02d", minsStd);

    }

    public String parseValue(Reading reading) {
        DecimalFormat df = new DecimalFormat("#.#");
        String toParse = (reading.isValid()) ? (df.format(reading.value)) : "";
        return toParse; //rightAlign(toParse, 6);

    }


}
