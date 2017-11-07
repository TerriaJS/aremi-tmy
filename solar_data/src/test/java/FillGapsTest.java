import org.junit.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;

import static org.junit.Assert.assertTrue;

public class FillGapsTest {

    public boolean checkDoubleArrayEquality(double[] arr1, double[] arr2) {
        if (arr1.length != arr2.length) return false;
        for (int i = 0; i < arr1.length; i++) {
            if (Math.abs(arr1[i] - arr2[i]) > 0.001) return false;
        }
        return true;
    }

    @Test
    public void testLinearInterpolate() {
        assertTrue(checkDoubleArrayEquality(FillGaps.linearInterpolate(1,7,5), new double[] {1,2,3,4,5,6,7}));
        assertTrue(checkDoubleArrayEquality(FillGaps.linearInterpolate(5.7, 25.5, 5), new double[] {5.7, 9, 12.3, 15.6, 18.9, 22.2, 25.5}));
        assertTrue(checkDoubleArrayEquality(FillGaps.linearInterpolate(6.9, 19.3, 2), new double[] {6.9, 11.033, 15.166, 19.3}));
        assertTrue(checkDoubleArrayEquality(FillGaps.linearInterpolate(5, 15, 1), new double[] {5, 10, 15}));
        assertTrue(checkDoubleArrayEquality(FillGaps.linearInterpolate(3, -5, 7), new double[] {3, 2, 1, 0, -1, -2, -3, -4, -5}));
    }

    @Test
    public void testAverage() {
        assertTrue(Math.abs(FillGaps.average(0, 5) - 0) < 0.001);
        assertTrue(Math.abs(FillGaps.average(12, 6) - 2) < 0.001);
        assertTrue(Math.abs(FillGaps.average(1, 1) - 1) < 0.001);
    }

    @Test
    public void testFillMissingTimestampWeather() {
        Main.stnNum = "99999";
        Main.wds = new ArrayList<>();
        LocalDateTime test1 = LocalDateTime.of(LocalDate.now(), LocalTime.now());
        Main.wds.add(new ActualWD(test1, null));
        LocalDateTime test2 = test1.plusHours(1);
        Main.wds.add(new ActualWD(test2, null));

        FillGapsWeather.fillMissingTimeStamp();

        // one object should have been added to the list at index 2 because of the hour gap
        assertTrue(Main.wds.size() == 3);
        // check if the object that was added has the desired timestamp
        assertTrue(Main.wds.get(1).dateTime.isEqual(test1.plusMinutes(30)));

        LocalDateTime test3 = test2.plusMinutes(30);
        Main.wds.add(new ActualWD(test3, null));

        FillGapsWeather.fillMissingTimeStamp();

        // the function should not have added any objects to the list so it should stay at size 4
        assertTrue(Main.wds.size() == 4);
        // check if the last Weather object has the correct date and time based on the number of objects in the list
        // i.e. 5 objects in the list means that the last one should have a difference of (30 * 4) from the first one
        assertTrue(Main.wds.get(Main.wds.size() - 1).dateTime.isEqual(test1.plusMinutes((Main.wds.size() - 1) * 30)));

        LocalDateTime test4 = test3.plusHours(5);
        Main.wds.add(new ActualWD(test4, null));

        FillGapsWeather.fillMissingTimeStamp();

        // the function should have added 10 new objects because of the 5 hour timestamp gap
        assertTrue(Main.wds.size() == 14);
        // check if the last Weather object has the correct date and time based on the number of objects in the list
        // i.e. 5 objects in the list means that the last one should have a difference of (30 * 4) from the first one
        assertTrue(Main.wds.get(Main.wds.size() - 1).dateTime.isEqual(test1.plusMinutes((Main.wds.size() - 1) * 30)));

    }

    @Test
    public void testGetReadingWeather() {
        Main.stnNum = "99999";
        Main.wds = new ArrayList<>();

        LocalDateTime test1 = LocalDateTime.of(LocalDate.now(), LocalTime.now());
        Main.wds.add(new ActualWD(test1, null));
        LocalDateTime test2 = test1.plusHours(1);
        Main.wds.add(new ActualWD(test2, null));
        LocalDateTime test3 = test2.plusMinutes(30);
        Main.wds.add(new ActualWD(test3, null));

        for (int i = 0; i < Main.wds.size(); i++) {
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[0]).varName.equals("Precipitation"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[1]).varName.equals("Air temperature"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[2]).varName.equals("Wet bulb temperature"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[3]).varName.equals("Dew point temperature"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[4]).varName.equals("Humidity"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[5]).varName.equals("Vapour pressure"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[6]).varName.equals("Saturated vapour pressure"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[7]).varName.equals("Wind speed"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[8]).varName.equals("Wind direction"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[9]).varName.equals("Wind gust"));
            assertTrue(Main.wds.get(i).getReading(WeatherVar.values()[10]).varName.equals("Sea level pressure"));

        }

    }
}
