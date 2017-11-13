import org.junit.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class SolarDataTest {

    SolarData sd = new SolarData(new String[] {"2017-01-01 03:00:00", "10", ""});

    @Test
    public void testConstructor() {
        assertEquals(sd.dataString[0], "2017-01-01 03:00:00");
        assertEquals(sd.dataString[1], "10");
        assertEquals(sd.dataString[2], "");
        assertEquals(sd.dateTime.getHour(), 3);
        assertEquals(sd.dateTime.getMinute(), 0);
        assertEquals(sd.dateTime.getSecond(), 0);
        assertEquals(sd.dateTime.getYear(), 2017);
        assertEquals(sd.dateTime.getMonthValue(), 1);
        assertEquals(sd.dateTime.getDayOfMonth(), 1);
        assertTrue(sd.dni.value - 10 < 0.001);
        assertTrue(sd.dni.isValid());
        assertFalse(sd.ghi.isValid());
    }

    @Test
    public void testCheckParsable() {
        assertFalse(sd.checkParsable(null));
        assertFalse(sd.checkParsable(""));
        assertFalse(sd.checkParsable("-"));
        assertFalse(sd.checkParsable("####"));
        assertTrue(sd.checkParsable("1234"));
        assertTrue(sd.checkParsable("3.0"));
        assertTrue(sd.checkParsable("012.345"));
    }

    @Test
    public void testGetReading() {
        assertEquals(sd.getReading(SolarVar.DNI), sd.dni);
        assertEquals(sd.getReading(SolarVar.GHI), sd.ghi);
    }
}
