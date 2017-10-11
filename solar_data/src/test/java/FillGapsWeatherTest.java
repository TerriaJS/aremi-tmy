import org.junit.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class FillGapsWeatherTest {

    public boolean checkDoubleArrayEquality(double[] arr1, double[] arr2) {
        if (arr1.length != arr2.length) return false;
        for (int i = 0; i < arr1.length; i++) {
            if (Math.abs(arr1[i] - arr2[i]) > 0.001) return false;
        }
        return true;
    }

    @Test
    public void testLinearInterpolate() {
        assertTrue(checkDoubleArrayEquality(FillGapsWeather.linearInterpolate(1,7,5), new double[] {1,2,3,4,5,6,7}));
        assertTrue(checkDoubleArrayEquality(FillGapsWeather.linearInterpolate(5.7, 25.5, 5), new double[] {5.7, 9, 12.3, 15.6, 18.9, 22.2, 25.5}));
        assertTrue(checkDoubleArrayEquality(FillGapsWeather.linearInterpolate(6.9, 19.3, 2), new double[] {6.9, 11.033, 15.166, 19.3}));
        assertTrue(checkDoubleArrayEquality(FillGapsWeather.linearInterpolate(5, 15, 1), new double[] {5, 10, 15}));
        assertTrue(checkDoubleArrayEquality(FillGapsWeather.linearInterpolate(3, -5, 7), new double[] {3, 2, 1, 0, -1, -2, -3, -4, -5}));
    }
}
