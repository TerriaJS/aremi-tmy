import org.junit.*;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class FillGapsTest {

    @Test
    public void testLinearInterpolate() {
        assertTrue(Arrays.equals(FillGaps.linearInterpolate(1,7,5), new double[] {1,2,3,4,5,6,7}));
    }
}
