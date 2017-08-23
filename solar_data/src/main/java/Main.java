import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.*;


public class Main {

    private static final String DNI = "http://services.aremi.d61.io/solar-satellite/v1/DNI/";
    private static final String GHI = "http://services.aremi.d61.io/solar-satellite/v1/GHI/";
    private static final String[] targetHeader = {"UTC time", "DNI value", "GHI value"};


    public static void main(String[] args) throws IOException {
        CSVReader stationsReader = new CSVReader(new BufferedReader(new FileReader(args[0])));
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

            // Get call to DNI url
            HttpClient httpClientDNI = HttpClientBuilder.create().build();
            HttpGet getDNIRequest = new HttpGet(DNI + latitude + "/" + longitude);

            HttpResponse dniResponse = httpClientDNI.execute(getDNIRequest);

            // Check for HTTP response code: 200 = success
            int dniStatusCode = dniResponse.getStatusLine().getStatusCode();
            if (dniStatusCode != 200) {
                System.out.println("Failed to get DNI from station " + stnNum + ". HTTP error code: " + dniStatusCode);
                continue;
            }

            // Get call to GHI url
            HttpClient httpClientGHI = HttpClientBuilder.create().build();
            HttpGet getGHIRequest = new HttpGet(GHI + latitude + "/" + longitude);

            HttpResponse ghiResponse = httpClientGHI.execute(getGHIRequest);

            // Check for HTTP response code: 200 = success
            int ghiStatusCode = ghiResponse.getStatusLine().getStatusCode();
            if (ghiStatusCode != 200) {
                System.out.println("Failed to get GHI from station " + stnNum + ". HTTP error code : " + ghiStatusCode);
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

                if (comparison < 0) { // somehow there are no corresponding GHI values
                    System.err.println("Missing GHI value for station " + stnNum);
                    dniReader.readNext();
                } else if (comparison > 0) { // somehow there are no corresponding DNI values
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
}
