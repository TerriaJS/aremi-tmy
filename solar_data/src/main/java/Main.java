import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.time.*;
import java.util.regex.Pattern;

public class Main {

    private final static String WRITE_TO_AVERAGED = "BoM_observations/Hourly-averaged-data/";
    private final static String WRITE_TO_ACTUAL = "BoM_observations/Hourly-data/";
    private final static String WRITE_TO_SOLAR = "BoM_observations/Hourly-solar-data/";
    private final static String WRITE_TO_SOLAR_PROCESSED = "BoM_observations/Hourly-solar-data-processed/";
    private final static String WRITE_TO_MERGED = "BoM_observations/Hourly-merged/";

    private static String filenamePref, filenameSuff;
    private static String parentDir;
    private static String stateName;
    static String stnNum;

    private final static String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
    private final static String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";

    private final static long AEST = 600; // UTC offset by 600 minutes (10 hours)
    private final static long ACST = 570; // UTC offset by 570 minutes (9 hours 30 minutes)
    private final static long AWST = 480; // UTC offset by 480 minutes (8 hours)

    private final static String TEST_STATION = "032040";

    private static Map<String, Long> TIME_ZONE_LOOKUP;

    static List<WeatherData> wds;
    static List<SolarData> sds;

    private static double yearsOfData;

    // TODO: make sure the software can handle files not found in a robust way and not throw errors

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

        // write the site details to a file
        CSVWriter stationsWriter = new CSVWriter(new BufferedWriter(new FileWriter("SolarStations.csv")));

        String[] header = {"Station name", "Bureau of Meteorology station number",
                "WMO index number", "Years of data", "Rainfall district code", "Month/Year site closed (MM/YYYY)",
                "Latitude", "Longitude", "Height of station above mean sea level in metres",
                "Height of barometer above mean sea level in metres", "Download historical observations (CSV)"};

        stationsWriter.writeNext(header, false);

        File file = new File("BoM_observations/Half-hourly-data");
        String pathToStnDet = "";
        for (File stateFolders : file.listFiles()) {
            System.out.println("In " + stateFolders.getPath());
            for (File stationFiles : stateFolders.listFiles()) {
                String fileName = stationFiles.getName();
                if (fileName.contains("StnDet")) pathToStnDet = stationFiles.getPath();
            }


            File stndet = new File(pathToStnDet);
            parentDir = stndet.getParent(); // get path of the parent to read the station files

            // find out which state we are working with to know which directory to write into

            CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader(pathToStnDet)));

            String[] line;

            while ((line = stationsReader.readNext()) != null) {
                stnNum = line[1].trim();
                String latitude = line[6].trim();
                String longitude = line[7].trim();
                stateName = line[9].trim();

                // need the exact format of the way the files are named,
                String fileName = stndet.getName();
                String[] splitName = fileName.split(Pattern.quote("StnDet"));
                filenamePref = splitName[0];
                filenameSuff = splitName[1];

                System.out.println("Working on station " + stnNum);

                // this fills in the gaps in the combined dni and ghi file
                boolean solarSuccess = processSolarValues();

                // if the solar data can't be processed then we know that we don't need to process anything else
                if (!solarSuccess) continue;

                // this fills in the gaps in the weather data and averages the values into hours
                boolean weatherSuccess = halfHourlyData();

                // if the weather data can't be processed then we know not to merge the data and skip the merging process
                if (!weatherSuccess) continue;

                // this only combines dni and ghi values
                //combineSolarValues(stnNum, latitude, longitude);

//                    System.out.println("I'm writing station " + stnNum + " to the file");
                mergeDatasets(); // if we can merge the datasets we can find the TMY
                // if we can find the TMY add the station to the csv file

                String[] stationDetails = new String[header.length];
                stationDetails[0] = line[3]; // stn name
                stationDetails[1] = stnNum; // stn number
                stationDetails[2] = line[12]; // wmo index number
                stationDetails[3] = Double.toString(yearsOfData); // years of data
                stationDetails[4] = line[2]; // rainfall district code
                stationDetails[5] = (line[5].trim().equals("")) ? "OPEN" : line[5]; // month/year closed
                stationDetails[6] = latitude; // lat
                stationDetails[7] = longitude; // long
                stationDetails[8] = line[10]; // height of stn above sea lvl
                stationDetails[9] = line[11]; // height of barometer above sea lvl
                stationDetails[10] = "<a href=\'http://static.aremi.nicta.com.au/datasets/SolarStationHistoricalObservations-0.1.1.0/" + stnNum + "_averaged.zip\'>" + yearsOfData + " years of data</a>"; // link to historical observations

//                    for (int i = 0; i < stationDetails.length; i++) {
//                        System.out.print(stationDetails[i] + " ");
//                    }
                stationsWriter.writeNext(stationDetails, false);

            }

            stationsReader.close();
        }

        stationsWriter.close();

    }



    private static void mergeDatasets() throws IOException {
//        if (stnNum.equals(TEST_STATION)) {
//            System.out.println("Working on merging solar and weather values");
            if (sds != null && wds != null) {
                int i = 0; // iterator for SolarData array
                int j = 0; // iterator for WeatherData array

                // create the directory first if it doesn't already exist
                if (!checkValidDirectory(WRITE_TO_MERGED, stateName)) {
                    return;
                }

                CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_MERGED + stateName + "/" + stnNum + "_averaged.csv"));

                String[] header = new String[]{"station", "local standard time",
                        "dni mean", "dni count", "dni fill count",
                        "ghi mean", "ghi count", "ghi fill count",
                        "precipitation"                 , "precipitation count"            , "precipitation fill count",
                        "air temperature mean"          , "air temperature count"          , "air temperature fill count",
                        "wet bulb temperature mean"     , "wet bulb temperature count"     , "wet bulb temperature fill count",
                        "dew point temperature mean"    , "dew point temperature count"    , "dew point temperature fill count",
                        "humidity mean"                 , "humidity count"                 , "humidity fill count",
                        "vapour pressure mean"          , "vapour pressure count"          , "vapour pressure fill count",
                        "saturated vapour pressure mean", "saturated vapour pressure count", "saturated vapour pressure fill count",
                        "wind speed mean"               , "wind speed count"               , "wind speed fill count",
                        "wind direction mean"           , "wind direction count"           , "wind direction fill count",
                        "wind gust mean"                , "wind gust count"                , "wind gust fill count",
                        "sea level pressure mean"       , "sea level pressure count"       , "sea level pressure fill count"};

                writer.writeNext(header);

                while (i < sds.size() && j < wds.size()) {

                    LocalDateTime solarDateTime = sds.get(i).dateTime;
                    LocalDateTime weatherDateTime = wds.get(j).dateTime;

                    // for NT and SA datasets, add the timestamp on the hour by 30 mins to match the solar timestamps
                    if (stateName.equals("NT") || stateName.equals("SA")) {
                        weatherDateTime = weatherDateTime.plusMinutes(30);
                    }

                    if (solarDateTime.isAfter(weatherDateTime)) {
                        // ignore the current weather reading because there are no solar reading to merge with
                        j++;
                    } else if (solarDateTime.isBefore(weatherDateTime)) {
                        // ignore the current solar reading because there are no weather reading to merge with
                        i++;
                    } else if (solarDateTime.isEqual(weatherDateTime)) {
                        WeatherData wd = wds.get(j);
                        CombinedData cd = new CombinedData(sds.get(i), wd);

                        writer.writeNext(cd.dataString, false);
                        i++;
                        j++;
                    }
                }

                writer.close();
            }
//        }

    }

    private static boolean processSolarValues() throws IOException {
//        if (stnNum.equals(TEST_STATION)) {
//            System.out.println("Processing solar values");
            try {
                CSVReader reader = new CSVReader(new BufferedReader(new FileReader(WRITE_TO_SOLAR + stateName + "/" + stnNum + "_dni_ghi.csv")));

                // if the directory doesn't exist, then write the create the directory first
                // handles any potential file not found error
                if (!checkValidDirectory(WRITE_TO_SOLAR_PROCESSED, stateName)) {
                    return false;
                }

                CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_SOLAR_PROCESSED + stateName + "/" + stnNum + "_dni_ghi_processed.csv"));

                String[] headers = reader.readNext(); // keep the headers as it is
                writer.writeNext(headers, false);

                String[] solarReadings;
                sds = new ArrayList<>();
                while ((solarReadings = reader.readNext()) != null) {
                    sds.add(new SolarData(solarReadings));
                }

                FillGapsSolar.fillMissingTimeStamp();
                FillGapsSolar.findGaps(sds);
                for (SolarData sd : sds) {
                    writer.writeNext(sd.combineValues(), false);
                }
                return true;
            } catch (Exception e) {
                sds = null;
                System.out.println("Cannot process solar values for station " + stnNum + ", caught an exception \"" + e.getMessage() + "\".");
                return false;
            }
//        }
//        return false;
    }

    private static void averageHalfHourlyData(String station) throws IOException {

//        System.out.println("Working on " + station);

        CSVReader reader = new CSVReader(new BufferedReader(new FileReader(parentDir + "/" + filenamePref + "Data_" + station + filenameSuff)));
        CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_AVERAGED + "/" + stateName + "/" + station + "_averaged.csv"));

        String[] headers = reader.readNext(); // keep the headers as it is
        writer.writeNext(headers, false);

        String[] weatherReadings;
        wds = new ArrayList<>();
        while ((weatherReadings = reader.readNext()) != null) {

            // use the standard time
            LocalDateTime dt = LocalDateTime.of(Integer.parseInt(weatherReadings[7]), // year
                    Integer.parseInt(weatherReadings[8]),                             // month
                    Integer.parseInt(weatherReadings[9]),                             // date
                    Integer.parseInt(weatherReadings[10]),                            // hours
                    Integer.parseInt(weatherReadings[11]));                           // minutes

            wds.add(new AveragedWD(dt, weatherReadings));

        }

        for (WeatherData wd : wds) {
            writer.writeNext(wd.combineValues(), false);
        }
        reader.close();
        writer.close();

    }

    private static boolean halfHourlyData() throws IOException {

//        if (stnNum.equals(TEST_STATION)) {
            try {
//                System.out.println("Processing weather values");

                CSVReader reader = new CSVReader(new BufferedReader(new FileReader(parentDir + "/" + filenamePref + "Data_" + stnNum + filenameSuff)));

                // if the directory doesn't exist, then write the create the directory first
                // handles any potential file not found error
                if (!checkValidDirectory(WRITE_TO_ACTUAL, stateName)) {
                    return false;
                }
                CSVWriter writer = new CSVWriter(new FileWriter(WRITE_TO_ACTUAL + stateName + "/" + stnNum + "_averaged.csv"));

                String[] headers = reader.readNext();
                writer.writeNext(headers, false);

                String[] weatherReadings;

                wds = new ArrayList<>();

                while ((weatherReadings = reader.readNext()) != null) {

                    // use the standard time
                    LocalDateTime dt = LocalDateTime.of(Integer.parseInt(weatherReadings[7]), // year
                            Integer.parseInt(weatherReadings[8]),                             // month
                            Integer.parseInt(weatherReadings[9]),                             // date
                            Integer.parseInt(weatherReadings[10]),                            // hours
                            Integer.parseInt(weatherReadings[11]));                           // minutes

                    // initialise w1 depending on which state we are dealing with
                    if (stateName.equals("NSW") || stateName.equals("QLD") || stateName.equals("WA")) {
                        wds.add(new ActualWD(dt, weatherReadings));
                    } else {
                        wds.add(new ActualWDBrief(dt, weatherReadings));
                    }

                }

                FillGapsWeather.fillMissingTimeStamp();
                FillGapsWeather.checkForGaps(wds);
                yearsOfData = calculateYearsOfData();
//                System.out.println("This station has " + yearsOfData + " years of data");
                Main.wds = FillGapsWeather.averageValues();

                for (WeatherData wd : wds) {
                    writer.writeNext(wd.combineValues(), false);
                }

                reader.close();
                writer.close();

                return true;
            } catch (Exception e) {
                wds = null;
                System.out.println("Cannot process solar values for station " + stnNum + ", caught an exception \"" + e.getMessage() + "\".");
                return false;
            }
//        }
//        return false;
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
        CSVWriter writer = new CSVWriter(new BufferedWriter(new FileWriter(WRITE_TO_SOLAR + stateName + "/" + station + "_dni_ghi.csv")));
        writer.writeNext(targetHeader, false);

        sds = new ArrayList<>();

        while ((dniReadings = dniReader.readNext()) != null && (ghiReadings = ghiReader.readNext()) != null) {

            LocalDateTime datetime = LocalDateTime.parse(dniReadings[0], DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ"));
            datetime = datetime.plusMinutes(TIME_ZONE_LOOKUP.get(stateName));
            sds.add(new SolarData(datetime, new String[] {DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(datetime), dniReadings[1], ghiReadings[1]}));

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

    static boolean checkValidDirectory(String whichDirectory, String state) {
        // if the parent directory already exists
        if (new File(whichDirectory).isDirectory()) {
            // check if the state directory already exists
            if (!new File(whichDirectory + state + "/").isDirectory()) {
                // if the state directory doesn't already exist, create one
                if (new File(whichDirectory + state).mkdir()) {
                    System.out.println("Created a new directory at " + whichDirectory + state);
                } else {
                    System.out.println("Failed to create a directory at " + whichDirectory + state);
                    return false;
                }
            }
        }
        // if the parent directory doesn't already exist
        else {
            // first create the parent directory
            if (new File(whichDirectory).mkdir()) {
                // then create the state directory
                if (new File(whichDirectory + state).mkdir())
                    System.out.println("Created a new directory at " + whichDirectory + state);
                else {
                    System.out.println("Failed to create a directory at " + whichDirectory + state);
                    return false;
                }

            } else {
                System.out.println("Failed to create a directory at " + whichDirectory);
                return false;
            }
        }
        return true;
    }

    private static double calculateYearsOfData() {
        // take the very first date & time
        // and the very last date & time
        // calculate how many hours of values we have
        // divide it by the number of hours in a year
        LocalDateTime start = wds.get(0).dateTime;
        LocalDateTime end = wds.get(wds.size() - 1).dateTime;
        end = end.plusMinutes(30);
        long dur = Duration.between(start, end).toHours();

        // return the value in 1 decimal place
        return Math.round((dur / (365 * 24.0)) * 10) / 10.0;
    }
}
