import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;

import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static org.apache.commons.lang3.ArrayUtils.addAll;

public class ChangeYear {

    private final static String WRITE_TO_TMY = "BoM_observations/tmy/";

    public static void main(String[] args) throws IOException {
        String stateName = new File(args[0]).getParentFile().getName();
        String stnNum = new File(args[0]).getName().split("_")[0];

        CSVReader reader = new CSVReader(new BufferedReader(new FileReader(args[0])));

        // if the directory doesn't exist, then write the create the directory first
        // handles any potential file not found error
        if (!Main.checkValidDirectory(WRITE_TO_TMY, stateName)) return;

        CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_TMY + stateName + "/" + stnNum + "_tmy_with_uniform_year.csv"));

        String[] headers = reader.readNext(); // keep the headers as it is

        // add the original column to the existing headers
        writer.writeNext(addAll(new String[] {"original"}, headers), false);

        String[] tmyReadings;
        while ((tmyReadings = reader.readNext()) != null) {
            // updates the year for all the readings
            LocalDateTime dt = LocalDateTime.parse(tmyReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            int origYear = dt.getYear();
            dt = dt.withYear(2017);
            tmyReadings[0] = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(dt);

            // creates a new String[] object with the original year as the first column
            String[] modifiedReadings = new String[tmyReadings.length + 1];
            System.arraycopy(tmyReadings, 0, modifiedReadings, 1, tmyReadings.length);
            modifiedReadings[0] = Integer.toString(origYear);

            // write the new String[] object to file
            writer.writeNext(modifiedReadings, false);
        }

        reader.close();
        writer.close();
    }
}
