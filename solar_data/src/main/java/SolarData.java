import java.time.*;
import java.time.format.DateTimeFormatter;

public class SolarData {

    int dni, ghi;
    LocalDateTime dateTime;
    String[] dataString;

    public SolarData(String[] data) {
        this.dataString = data;
        this.dateTime = LocalDateTime.parse(data[0], DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        this.dni = (checkParsable(data[1]) ? Integer.parseInt(data[1]) : 0);
        this.ghi = (checkParsable(data[2]) ? Integer.parseInt(data[2]) : 0);
    }

    public SolarData(LocalDateTime dt, String[] data) {
        this.dateTime = dt;
        this.dataString = data;

        this.dni = (checkParsable(data[1]) ? Integer.parseInt(data[1]) : 0);
        this.ghi = (checkParsable(data[2]) ? Integer.parseInt(data[2]) : 0);
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
