import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.*;
import java.util.regex.Pattern;


public class Main {

    private final static String WRITE_TO_AVERAGED = "BoM_observations/Hourly-averaged-data/";
    private final static String WRITE_TO_ACTUAL = "BoM_observations/Hourly-data/";
    private final static String WRITE_TO_SOLAR = "BoM_observations/Hourly-solar-data/";

    private static String filenamePref, filenameSuff;
    private static String parentDir;
    private static String stateName;

    private final static String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
    private final static String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";

    public static void main(String[] args) throws IOException {

        File f = new File(args[0]);
        parentDir = f.getParent(); // get path of the parent to read the station files

        // find out which state we are working with to know which directory to write into
        String parentDirName = f.getParentFile().getName();
        stateName = parentDirName.split("_")[0];

        CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader(args[0])));

        String[] line;

        while ((line = stationsReader.readNext()) != null) {
            String stnNum = line[1].trim();
            String latitude = line[6].trim();
            String longitude = line[7].trim();

            // need the exact format of the way the files are named,
            String fileName = f.getName();
            String[] splitName = fileName.split(Pattern.quote("StnDet"));
            filenamePref = splitName[0];
            filenameSuff = splitName[1];

            averageHalfHourlyData(stnNum);
            //halfHourlyData(stnNum);
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
        while ((weatherReadings = reader.readNext()) != null) {
            WeatherData w1 = new AveragedWD(weatherReadings);
            if (w1.checkQuality()) {
                if (w1.mins == 0 && (weatherReadings = reader.readNext()) != null) {
                    WeatherData w2 = new AveragedWD(weatherReadings);
                    if (w2.checkQuality()) w1.averageValues(w2);
                }
                writer.writeNext(w1.combineValues(), false);
            }


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

        final String[] targetHeader = {"UTC time", "DNI value", "GHI value"};

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
                writer.writeNext(new String[] {dniReadings[0], dniReadings[1], ghiReadings[1]}, false);
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
