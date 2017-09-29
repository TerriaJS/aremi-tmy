public class FillGaps {

    public static double[] linearInterpolate(double from, double to, int gapsCount) {
        double[] res = new double[gapsCount + 2];

        // the first and last value should be from and to
        res[0] = from;
        res[res.length - 1] = to;

        // calculate the gradient over the two points
        double gradient = (to - from) / (gapsCount + 1);

        for (int i = 1; i < res.length - 1; i++) {
            res[i] = gradient * (i + 1); // add one because index starts from 0
        }

        return res;
    }
}
