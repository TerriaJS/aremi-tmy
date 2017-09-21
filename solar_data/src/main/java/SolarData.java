import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;

import java.time.*;
import java.time.format.DateTimeFormatter;

public class SolarData {

    LocalDateTime ldt;
    DateTime dt;
    int dni, ghi;

    String[] dataString;

    public SolarData(String[] data) {

        this.dataString = data;

        ldt = LocalDateTime.parse(data[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
        dt = DateTime.parse(data[0], DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ"));

        System.out.println(ldt.toString());

        this.dni = (checkParsable(data[1]) ? Integer.parseInt(data[1]) : 0);
        this.ghi = (checkParsable(data[2]) ? Integer.parseInt(data[2]) : 0);
    }

    public boolean checkParsable(String str) {
        if (str.equals("")) return false;
        for (int i = 0; i < str.length(); i++) {
            if (!Character.isDigit(str.charAt(i)) && !(str.charAt(i) == '.')) {
                return false;
            }
        }
        return true;
    }
}
