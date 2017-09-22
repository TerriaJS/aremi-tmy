import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.joda.time.format.DateTimeFormat;

import java.io.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.time.*;
import java.time.ZoneOffset;

public class Main {

    private final static String WRITE_TO_AVERAGED = "BoM_observations/Hourly-averaged-data/";
    private final static String WRITE_TO_ACTUAL = "BoM_observations/Hourly-data/";
    private final static String WRITE_TO_SOLAR = "BoM_observations/Hourly-solar-data/";

    private static String filenamePref, filenameSuff;
    private static String parentDir;
    private static String stateName;

    private final static String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
    private final static String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";

    private final static ZoneOffset AEST = ZoneOffset.ofHours(10);
    private final static ZoneOffset ACST = ZoneOffset.ofHoursMinutes(9, 30);
    private final static ZoneOffset AWST = ZoneOffset.ofHours(8);

    private static Map<String, ZoneId> TIME_ZONE_LOOKUP;

    public static void main(String[] args) throws IOException {

        // populate the lookup table with states and their corresponding offsets from UTC
        TIME_ZONE_LOOKUP = new HashMap<>();
        TIME_ZONE_LOOKUP.put("NSW", ZoneId.of("Australia/NSW"));
        TIME_ZONE_LOOKUP.put("VIC", ZoneId.of("Australia/Victoria"));
        TIME_ZONE_LOOKUP.put("TAS", ZoneId.of("Australia/Tasmania"));
        TIME_ZONE_LOOKUP.put("QLD", ZoneId.of("Australia/Queensland"));
        TIME_ZONE_LOOKUP.put("WA", ZoneId.of("Australia/West"));
        TIME_ZONE_LOOKUP.put("SA", ZoneId.of("Australia/South"));
        TIME_ZONE_LOOKUP.put("NT", ZoneId.of("Australia/North"));

        File f = new File(args[0]);
        parentDir = f.getParent(); // get path of the parent to read the station files

        // find out which state we are working with to know which directory to write into

        CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader(args[0])));

        String[] line;

        while ((line = stationsReader.readNext()) != null) {
            String stnNum = line[1].trim();
            String latitude = line[6].trim();
            String longitude = line[7].trim();
            stateName = line[9].trim();

            // need the exact format of the way the files are named,
            String fileName = f.getName();
            String[] splitName = fileName.split(Pattern.quote("StnDet"));
            filenamePref = splitName[0];
            filenameSuff = splitName[1];

            //averageHalfHourlyData(stnNum);
            //halfHourlyData(stnNum);
            combineSolarValues(stnNum, latitude, longitude);
        }

        stationsReader.close();

    }

    private static void averageHalfHourlyData(String station) throws IOException {

        System.out.println("Working on " + station);

        CSVReader reader = new CSVReader(new BufferedReader(new FileReader(parentDir + "/" + filenamePref + "Data_" + station + filenameSuff)));
        CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_AVERAGED + "/" + stateName + "/" + station + "_averaged.csv"));

        String[] headers = reader.readNext(); // keep the headers as it is
        writer.writeNext(headers, false);

        String[] weatherReadings;
        List<AveragedWD> wds = new ArrayList<AveragedWD>();
        while ((weatherReadings = reader.readNext()) != null) {

            wds.add(new AveragedWD(weatherReadings));

//            WeatherData w1 = new AveragedWD(weatherReadings);
//            if (w1.checkQuality()) {
//                if (w1.mins == 0 && (weatherReadings = reader.readNext()) != null) {
//                    WeatherData w2 = new AveragedWD(weatherReadings);
//                    if (w2.checkQuality()) w1.averageValues(w2);
//                }
//                writer.writeNext(w1.combineValues(), false);
//            }
//
        }

        for (AveragedWD wd : wds) {
            writer.writeNext(wd.combineValues(), false);
        }
        reader.close();
        writer.close();

    }

    private static void halfHourlyData(String station) throws IOException {

        System.out.println("Working on " + station);

        CSVReader reader = new CSVReader(new BufferedReader(new FileReader(parentDir + "/" + filenamePref + "Data_" + station + filenameSuff)));
        CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_ACTUAL + "/" + stateName + "/" + station + "_averaged.csv"));

        String[] headers = reader.readNext(); // keep the headers as it is
        writer.writeNext(headers, false);

        String[] weatherReadings;
        while ((weatherReadings = reader.readNext()) != null) {
            WeatherData w1 = new ActualWD(weatherReadings);
            if (w1.checkQuality()) {
                if (w1.mins == 0 && (weatherReadings = reader.readNext()) != null) {
                    WeatherData w2 = new ActualWD(weatherReadings);
                    if (w2.checkQuality()) w1.averageValues(w2);
                }
                writer.writeNext(w1.combineValues(), false);
            }


        }
        reader.close();
        writer.close();
    }

    private static void combineSolarValues(String station, String latitude, String longitude) throws IOException {

        final String[] targetHeader = {"Local standard time", "DNI value", "GHI value"};

        System.out.println("Working on station " + station);

        HttpResponse dniResponse = httpGetter(DNI, latitude, longitude);
        HttpResponse ghiResponse = httpGetter(GHI, latitude, longitude);

        // Check for HTTP response code: 200 = success
        int dniStatusCode = dniResponse.getStatusLine().getStatusCode();
        int ghiStatusCode = ghiResponse.getStatusLine().getStatusCode();
        if (dniStatusCode != 200 || ghiStatusCode != 200) {
            System.out.println("Failed to get data from station " + station + ". HTTP error code: " + dniStatusCode + ", " + ghiStatusCode);
            return;
        }

        // Read the DNI and GHI csv files
        CSVReader dniReader = new CSVReader(new BufferedReader(new InputStreamReader(dniResponse.getEntity().getContent())));
        CSVReader ghiReader = new CSVReader(new BufferedReader(new InputStreamReader(ghiResponse.getEntity().getContent())));

        dniReader.readNext(); // skip the headers
        ghiReader.readNext(); // skip the headers

        String[] dniReadings, ghiReadings;

        // Save data to a csv file with the name <station_number>_dni_ghi.csv
        CSVWriter writer = new CSVWriter(new BufferedWriter(new FileWriter(WRITE_TO_SOLAR + "/" + stateName + "/" + station + "_dni_ghi.csv")));
        writer.writeNext(targetHeader, false);

        while ((dniReadings = dniReader.readNext()) != null && (ghiReadings = ghiReader.readNext()) != null) {
            // see if the timestamps are equal and if not, see which one missed a timestamp
            int comparison = dniReadings[0].compareTo(ghiReadings[0]);

            if (comparison < 0) { // GHI value is missing
                System.err.println("Missing GHI value for station " + station);
                dniReader.readNext();
            } else if (comparison > 0) { // DNI value is missing
                System.err.println("Missing DNI value for station " + station);
                ghiReader.readNext();
            } else {
                LocalDateTime datetime = LocalDateTime.parse(dniReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
                datetime = datetime.plusHours(10);
//                ZonedDateTime datetime = ZonedDateTime.parse(dniReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
//                datetime = datetime.withZoneSameInstant(TIME_ZONE_LOOKUP.get(stateName));
                SolarData s = new SolarData(new String[] {DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm").format(datetime), dniReadings[1], ghiReadings[1]});
                writer.writeNext(s.dataString, false);
            }
        }

        writer.close();

        dniReader.close();
        ghiReader.close();

    }

    private static HttpResponse httpGetter(String url, String latitude, String longitude) throws IOException {

        HttpClient httpClient = HttpClientBuilder.create().build();
        HttpGet getRequest = new HttpGet(url + latitude + "/" + longitude);

        return httpClient.execute(getRequest);
    }
}
