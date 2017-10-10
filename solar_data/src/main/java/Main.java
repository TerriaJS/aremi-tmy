import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;

import static org.apache.commons.lang3.ArrayUtils.addAll;
import java.util.Arrays;

import java.io.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.time.*;

public class Main {

    private final static String WRITE_TO_AVERAGED = "BoM_observations/Hourly-averaged-data/";
    private final static String WRITE_TO_ACTUAL = "BoM_observations/Hourly-data/";
    private final static String WRITE_TO_SOLAR = "BoM_observations/Hourly-solar-data/";

    private static String filenamePref, filenameSuff;
    private static String parentDir;
    private static String stateName;

    private final static String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
    private final static String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";

    private final static long AEST = 600; // UTC offset by 600 minutes (10 hours)
    private final static long ACST = 570; // UTC offset by 570 minutes (9 hours 30 minutes)
    private final static long AWST = 480; // UTC offset by 480 minutes (8 hours)

    private static Map<String, Long> TIME_ZONE_LOOKUP;

    static List<WeatherData> wds;
    static List<SolarData> sds;

    public static void main(String[] args) throws IOException {

        // populate the lookup table with states and their corresponding offsets from UTC
        TIME_ZONE_LOOKUP = new HashMap<>();
        TIME_ZONE_LOOKUP.put("NSW", AEST);
        TIME_ZONE_LOOKUP.put("VIC", AEST);
        TIME_ZONE_LOOKUP.put("TAS", AEST);
        TIME_ZONE_LOOKUP.put("QLD", AEST);
        TIME_ZONE_LOOKUP.put("WA", AWST);
        TIME_ZONE_LOOKUP.put("SA", ACST);
        TIME_ZONE_LOOKUP.put("NT", ACST);

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
            halfHourlyData(stnNum);
            //combineSolarValues(stnNum, latitude, longitude);
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

            // use the standard time
            LocalDateTime dt = LocalDateTime.of(Integer.parseInt(weatherReadings[7]), // year
                    Integer.parseInt(weatherReadings[8]),                             // month
                    Integer.parseInt(weatherReadings[9]),                             // date
                    Integer.parseInt(weatherReadings[10]),                            // hours
                    Integer.parseInt(weatherReadings[11]));                           // minutes

            wds.add(new AveragedWD(dt, weatherReadings));

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

        if (station.equals("061412")) {
            System.out.println("Working on " + station);

            CSVReader reader = new CSVReader(new BufferedReader(new FileReader(parentDir + "/" + filenamePref + "Data_" + station + filenameSuff)));
            CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_ACTUAL + "/" + stateName + "/" + station + "_averaged.csv"));

            String[] headers = reader.readNext();
            writer.writeNext(headers, false);

            String[] weatherReadings;
            //List<WeatherData> wds = new ArrayList<>();

            wds = new ArrayList<>();

            System.out.println("Working on the while loop to populate the array");
            while ((weatherReadings = reader.readNext()) != null) {
//                WeatherData w1;

                // use the standard time
                LocalDateTime dt = LocalDateTime.of(Integer.parseInt(weatherReadings[7]), // year
                        Integer.parseInt(weatherReadings[8]),                             // month
                        Integer.parseInt(weatherReadings[9]),                             // date
                        Integer.parseInt(weatherReadings[10]),                            // hours
                        Integer.parseInt(weatherReadings[11]));                           // minutes

                // initialise w1 depending on which state we are dealing with
                if (stateName.equals("NSW") || stateName.equals("QLD") || stateName.equals("WA")) {
                    //w1 = new ActualWD(weatherReadings);
                    wds.add(new ActualWD(dt, weatherReadings));
                } else {
                    //w1 = new ActualWDBrief((weatherReadings));
                    wds.add(new ActualWDBrief(dt, weatherReadings));
                }

//                // averaging the two half hour readings into hourly
//                if (w1.checkQuality()) {
//                    if (w1.mins == 0 && (weatherReadings = reader.readNext()) != null) {
//                        WeatherData w2;
//
//                        // initialise w2 depending on which state we are dealing with
//                        if (stateName.equals("NSW") || stateName.equals("QLD") || stateName.equals("WA")) {
//                            w2 = new ActualWD(weatherReadings);
//                        } else {
//                            w2 = new ActualWDBrief((weatherReadings));
//                        }
//
//                        if (w2.checkQuality()) w1.averageValues(w2);
//                    }
//                    writer.writeNext(w1.combineValues(), false);
//                }
            }
            System.out.println("Check if any we have gaps in terms of missing timestamp");
            FillGaps.fillMissingTimeStamp(station);
            FillGaps.findGaps(wds);


            System.out.println("Now writing the datasets to file");
            for (WeatherData wd : wds) {
                writer.writeNext(wd.combineValues(), false);
            }

            System.out.println("Done with writing to file, done with this station");

            reader.close();
            writer.close();
        }
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


        //List<SolarData> sds = new ArrayList<>();
        sds = new ArrayList<>();

        while ((dniReadings = dniReader.readNext()) != null && (ghiReadings = ghiReader.readNext()) != null) {

            LocalDateTime datetime = LocalDateTime.parse(dniReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
            datetime = datetime.plusMinutes(TIME_ZONE_LOOKUP.get(stateName));
            sds.add(new SolarData(datetime, new String[] {DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(datetime), dniReadings[1], ghiReadings[1]}));

//            // see if the timestamps are equal and if not, see which one missed a timestamp
//            int comparison = dniReadings[0].compareTo(ghiReadings[0]);
//
//            if (comparison < 0) { // GHI value is missing
//                System.err.println("Missing GHI value for station " + station);
//                dniReader.readNext();
//            } else if (comparison > 0) { // DNI value is missing
//                System.err.println("Missing DNI value for station " + station);
//                ghiReader.readNext();
//            } else {
//                LocalDateTime datetime = LocalDateTime.parse(dniReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
//                datetime = datetime.plusMinutes(TIME_ZONE_LOOKUP.get(stateName));
//                SolarData s = new SolarData(new String[] {DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(datetime), dniReadings[1], ghiReadings[1]});
//                writer.writeNext(s.dataString, false);
//            }
        }

        for (SolarData sd : sds) {
            writer.writeNext(sd.dataString, false);
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
