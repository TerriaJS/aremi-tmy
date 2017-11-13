import java.time.*;
import java.time.format.DateTimeFormatter;

public class SolarData {

    Reading dni, ghi;
    LocalDateTime dateTime;
    String[] dataString;

    public SolarData(String[] data) {
        this.dataString = data;
        this.dateTime = LocalDateTime.parse(data[0], DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));

        boolean isGap;

        isGap = checkParsable(data[1]);
        this.dni = new Reading("Direct normal irradiance", (isGap) ? Integer.parseInt(data[1]) : 0, isGap);

        isGap = checkParsable(data[2]);
        this.ghi = new Reading("Global horizontal irradiance", (isGap) ? Integer.parseInt(data[2]) : 0, isGap);
    }

    public SolarData(LocalDateTime dt, String[] data) {
        this.dateTime = dt;
        this.dataString = data;

        boolean isGap;

        isGap = checkParsable(data[1]);
        this.dni = new Reading("Direct normal irradiance", (isGap) ? Integer.parseInt(data[1]) : 0, isGap);

        isGap = checkParsable(data[2]);
        this.ghi = new Reading("Global horizontal irradiance", (isGap) ? Integer.parseInt(data[2]) : 0, isGap);
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

    public Reading getReading(SolarVar whichVariable) {
        switch (whichVariable) {
            case DNI:
                return this.dni;
            case GHI:
                return this.ghi;
            default:
                return null;
        }
    }

    public String rightAlign(String str, int intendedLength) {
        return String.format("%1$" + intendedLength + "s", str);
    }


    public String[] combineValues() {
        dataString[1] = (dni.isValid()) ? Integer.toString((int) dni.value) : "";
        dataString[2] = (ghi.isValid()) ? Integer.toString((int) ghi.value) : "";

        return dataString;

    }
}
