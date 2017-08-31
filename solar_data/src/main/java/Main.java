import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.commons.io.FileUtils;

import java.io.*;


public class Main {


    public static void main(String[] args) throws IOException {

//        combineSolarValues(args[0]);
//        printHalfHourlyData();
        averageHalfHourlyData();
//        System.out.println((" ".trim().equals("")) ? Double.parseDouble("12.0") : 0);

    }

    private static void averageHalfHourlyData() throws IOException {
        final String directoryName = "BoM_observations/Half-hourly-averaged-data";

        File mainFile = new File(directoryName);
        File[] statesList = mainFile.listFiles();

        if (statesList == null) {
            System.out.println("No files in directory " + mainFile.getName());
            return;
        }

        //for (File statesDir : statesList) {
        CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader("BoM_observations/Half-hourly-averaged-data/NSW_averaged/HD01D_StnDet_999999998661924.txt")));

        String[] line = stationsReader.readNext();

        //while((line = stationsReader.readNext()) != null) {
            String stnNum = line[1].trim();
            System.out.println("Working on " + stnNum + " in " + "NSW_averaged"); //statesDir.getName());

            CSVReader reader = new CSVReader(new BufferedReader(new FileReader("BoM_observations/Half-hourly-averaged-data/NSW_averaged/HD01D_Data_" + stnNum + "_999999998661924.txt")));

            /*hd
            Station Number
            YYYY
            MM
            DD
            HH24
            MI format in Local time
            Year Month Day Hour Minutes in YYYY
            MM
            DD
            HH24
            MI format in Local standard time
            Average air temperature in last 30 minutes in degrees Celsius where observations count >= 12
            Quality of average air temperature in last 30 minutes
            Count of average air temperature observations in last 30 minutes
            Relative humidity in percentage %
            Quality of relative humidity
            Average wind speed in last 30 minutes in km/h where observations count >= 12
            Quality of average wind speed in last 30 minutes
            Count of average wind speed observations in last 30 minutes
            Highest maximum 3 sec wind gust in last 30 minutes in km/h where observations count >= 12
            Quality of Highest maximum 3 sec wind gust in last 30 minutes
            Count of Highest maximum 3 sec wind gust observations in last 30 minutes
            Average direction of wind in last 30 minutes in degrees true where observations count >= 12
            Quality of average direction of wind in last 30 minutes
            Count of average direction of wind observations in last 30 minutes*/


            CSVWriter writer = new CSVWriter(new FileWriter("BoM_observations/Hourly-averaged-data/NSW_averaged/HD01D_Data_" + stnNum + "_999999998661924_averaged.csv"));

            String[] headers = reader.readNext();
            writer.writeNext(headers, false);

            String[] weatherReadings; //skip the headers
            while ((weatherReadings = reader.readNext()) != null) {
                WeatherData w1, w2;

                w1 = new WeatherData(weatherReadings);
                if (w1.mins == 0) {
                    weatherReadings = reader.readNext();
                    w2 = new WeatherData(weatherReadings);
                    if (w1.checkQuality() && w2.checkQuality()) {
                        WeatherData avgWeather = w1;
                        avgWeather.airTemp.value = (w1.airTemp.value + w2.airTemp.value) / 2;
                        avgWeather.humidity.value = (w1.humidity.value + w2.humidity.value) / 2;
                        avgWeather.windSpeed.value = (w1.windSpeed.value + w2.windSpeed.value) / 2;
                        avgWeather.windDir.value = (w1.windDir.value + w2.windDir.value) / 2;
                        avgWeather.windGust.value = (w1.windGust.value + w2.windGust.value) / 2;
                        writer.writeNext(avgWeather.combineValues(), false);
                    }
                } else {
                    if (w1.checkQuality()) writer.writeNext(weatherReadings, false);
                }


/*
                WeatherData w1 = new WeatherData(weatherReadings);
                if (w1.mins == 0) {
                    weatherReadings = reader.readNext();
                    WeatherData w2 = new WeatherData(weatherReadings);

                    // Filters out the qualities which should not be included in the dataset
                    if (w1.checkQuality()) writer.writeNext(weatherReadings);

                } else {
                    continue; // do something with this one value??
                }
*/


            }

        //}

        stationsReader.close();
            reader.close();
            writer.close();
        //}

    }

    private static void printHalfHourlyData() throws IOException {
        final String directoryName = "BoM_observations/Half-hourly-averaged-data";

        File dir = new File(directoryName);
        File[] halfHourData = dir.listFiles();

        if (halfHourData == null) {
            System.out.println("No files in directory " + dir.getName());
            return;
        }

        for (File statesDir : halfHourData) {
            // System.out.println("In the " + statesDir.getName() + " directory");
            System.out.println("|- " + statesDir.getName());
            File state = new File(directoryName + "/" + statesDir.getName());
            File[] stations = state.listFiles();

            if (stations == null) {
                System.out.println("No files in directory " + state.getName());
                return;
            }

            for (File data : stations) {
                // System.out.println("Working on dataset " + data.getName());
                System.out.println("  |- " + data.getName());
            }
        }
    }

    private static void combineSolarValues(String filename) throws IOException {
        final String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
        final String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";
        final String[] targetHeader = {"UTC time", "DNI value", "GHI value"};

        CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader(filename)));
        stationsReader.readNext(); // skip the headers

        // Station number: 1
        // Latitude: 5
        // Longitude: 6

        String[] line;

        while ((line = stationsReader.readNext()) != null) {

            String stnNum = line[1].trim();
            String latitude = line[5].trim();
            String longitude = line[6].trim();

            System.out.println("Working on station " + stnNum);

            HttpResponse dniResponse = httpGetter(DNI, latitude, longitude);
            HttpResponse ghiResponse = httpGetter(GHI, latitude, longitude);

            // Check for HTTP response code: 200 = success
            int dniStatusCode = dniResponse.getStatusLine().getStatusCode();
            int ghiStatusCode = ghiResponse.getStatusLine().getStatusCode();
            if (dniStatusCode != 200 || ghiStatusCode != 200) {
                System.out.println("Failed to get data from station " + stnNum + ". HTTP error code: " + dniStatusCode);
                continue;
            }

            // Read the DNI and GHI csv files
            CSVReader dniReader = new CSVReader(new BufferedReader(new InputStreamReader(dniResponse.getEntity().getContent())));
            CSVReader ghiReader = new CSVReader(new BufferedReader(new InputStreamReader(ghiResponse.getEntity().getContent())));

            dniReader.readNext(); // skip the headers
            ghiReader.readNext(); // skip the headers

            String[] dniReadings, ghiReadings;

            // Save data to a csv file with the name <station_number>_dni_ghi.csv
            CSVWriter writer = new CSVWriter(new BufferedWriter(new FileWriter("solar_data/" + stnNum + "_dni_ghi.csv")));
            writer.writeNext(targetHeader, false);

            while ((dniReadings = dniReader.readNext()) != null && (ghiReadings = ghiReader.readNext()) != null) {
                // see if the timestamps are equal and if not, see which one missed a timestamp
                int comparison = dniReadings[0].compareTo(ghiReadings[0]);

                if (comparison < 0) { // GHI value is missing
                    System.err.println("Missing GHI value for station " + stnNum);
                    dniReader.readNext();
                } else if (comparison > 0) { // DNI value is missing
                    System.err.println("Missing DNI value for station " + stnNum);
                    ghiReader.readNext();
                } else {
                    writer.writeNext(new String[] {dniReadings[0], dniReadings[1], ghiReadings[1]}, false);
                }
            }

            System.out.println("Done with station " + stnNum);

            writer.close();

            dniReader.close();
            ghiReader.close();
        }

        stationsReader.close();
    }

    private static HttpResponse httpGetter(String url, String latitude, String longitude) throws IOException {

        HttpClient httpClient = HttpClientBuilder.create().build();
        HttpGet getRequest = new HttpGet(url + latitude + "/" + longitude);

        return httpClient.execute(getRequest);
    }
}
