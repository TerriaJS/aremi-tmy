import org.junit.*;

import java.time.LocalDateTime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class WeatherDataTest {

    // a sample weather data taken from the file
    WeatherData wd = new ActualWD(LocalDateTime.of(2017, 1, 1, 3, 0, 0),
            new String[] {"hm","46128","2017","01","01","03","00","2017","01","01","03","00",
                    " "," "," 36.2","N"," 21.8","N"," 12.4","N"," 24","N","  14.4","N","  60.1","N"," 17","N","210","N"," 28","N","1005.7","N","","#"});

    @Test
    public void testConstructor() {
        assertEquals(wd.dateTime.getHour(), 3);
        assertEquals(wd.dateTime.getMinute(), 0);
        assertEquals(wd.dateTime.getSecond(), 0);
        assertEquals(wd.dateTime.getYear(), 2017);
        assertEquals(wd.dateTime.getMonthValue(), 1);
        assertEquals(wd.dateTime.getDayOfMonth(), 1);
        assertTrue(wd.precip.value - 0 < 0.001);
        assertFalse(wd.precip.isValid());
        assertTrue(wd.airTemp.value - 36.2 < 0.001);
        assertTrue(wd.airTemp.isValid());
        assertTrue(wd.wbTemp.value - 21.8 < 0.001);
        assertTrue(wd.wbTemp.isValid());
        assertTrue(wd.dpTemp.value - 12.4 < 0.001);
        assertTrue(wd.dpTemp.isValid());
        assertTrue(wd.humidity.value - 24 < 0.001);
        assertTrue(wd.humidity.isValid());
        assertTrue(wd.vapPressure.value - 14.4 < 0.001);
        assertTrue(wd.vapPressure.isValid());
        assertTrue(wd.satVapPressure.value - 60.1 < 0.001);
        assertTrue(wd.satVapPressure.isValid());
        assertTrue(wd.windSpeed.value - 17 < 0.001);
        assertTrue(wd.windSpeed.isValid());
        assertTrue(wd.windDir.value - 210 < 0.001);
        assertTrue(wd.windDir.isValid());
        assertTrue(wd.windGust.value - 28 < 0.001);
        assertTrue(wd.windGust.isValid());
        assertTrue(wd.seaLvlPressure.value - 1005.7 < 0.001);
        assertTrue(wd.seaLvlPressure.isValid());
        
    }

    @Test
    public void testCheckQuality() {
        assertTrue(wd.checkQuality("Y"));
        assertTrue(wd.checkQuality("N"));
        assertTrue(wd.checkQuality("S"));
        assertTrue(wd.checkQuality("F"));
        assertFalse(wd.checkQuality("I"));
        assertFalse(wd.checkQuality(""));
    }

    @Test
    public void testCheckParsable() {
        assertFalse(wd.checkParsable(null));
        assertFalse(wd.checkParsable(""));
        assertFalse(wd.checkParsable("####"));
        assertTrue(wd.checkParsable("-3.5"));
        assertTrue(wd.checkParsable("1234"));
        assertTrue(wd.checkParsable("3.0"));
        assertTrue(wd.checkParsable("012.345"));
    }

    @Test
    public void testGetReading() {
        assertTrue(wd.getReading(WeatherVar.values()[0]).varName.equals("Precipitation"));
        assertTrue(wd.getReading(WeatherVar.values()[1]).varName.equals("Air temperature"));
        assertTrue(wd.getReading(WeatherVar.values()[2]).varName.equals("Wet bulb temperature"));
        assertTrue(wd.getReading(WeatherVar.values()[3]).varName.equals("Dew point temperature"));
        assertTrue(wd.getReading(WeatherVar.values()[4]).varName.equals("Humidity"));
        assertTrue(wd.getReading(WeatherVar.values()[5]).varName.equals("Vapour pressure"));
        assertTrue(wd.getReading(WeatherVar.values()[6]).varName.equals("Saturated vapour pressure"));
        assertTrue(wd.getReading(WeatherVar.values()[7]).varName.equals("Wind speed"));
        assertTrue(wd.getReading(WeatherVar.values()[8]).varName.equals("Wind direction"));
        assertTrue(wd.getReading(WeatherVar.values()[9]).varName.equals("Wind gust"));
        assertTrue(wd.getReading(WeatherVar.values()[10]).varName.equals("Sea level pressure"));
    }

    @Test
    public void testContainsVar() {
        assertTrue(wd.containsVar(WeatherVar.values()[0]));
        assertTrue(wd.containsVar(WeatherVar.values()[1]));
        assertTrue(wd.containsVar(WeatherVar.values()[2]));
        assertTrue(wd.containsVar(WeatherVar.values()[3]));
        assertTrue(wd.containsVar(WeatherVar.values()[4]));
        assertTrue(wd.containsVar(WeatherVar.values()[5]));
        assertTrue(wd.containsVar(WeatherVar.values()[6]));
        assertTrue(wd.containsVar(WeatherVar.values()[7]));
        assertTrue(wd.containsVar(WeatherVar.values()[8]));
        assertTrue(wd.containsVar(WeatherVar.values()[9]));
        assertTrue(wd.containsVar(WeatherVar.values()[10]));
    }
}
