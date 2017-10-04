import java.time.LocalDateTime;
import java.util.List;

public class FillGaps {

    public static final int PRECIP = 0;
    public static final int WBTEMP = 1;
    public static final int DPTEMP = 2;
    public static final int AIRTEMP = 3;
    public static final int HUMIDITY = 4;
    public static final int VAP = 5;
    public static final int SATVAP = 6;
    public static final int WINDSPD = 7;
    public static final int WINDDIR = 8;
    public static final int WINDGUST = 9;
    public static final int SEALVL = 10;


    public static double[] linearInterpolate(double from, double to, int gapsCount) {
        double[] res = new double[gapsCount + 2];

        // the first and last value should be from and to
        res[0] = from;
        res[res.length - 1] = to;

        // calculate the gradient over the two points
        double gradient = (to - from) / (gapsCount + 1);

        for (int i = 1; i < res.length - 1; i++) {
            res[i] = res[i-1] + gradient;

        }

        return res;
    }

    // only works for ActualWD for now
    public static void fillMissingTimeStamp(String station, List<WeatherData> list) {
        LocalDateTime currDateTIme = list.get(0).dateTime;
        for (int i = 1; i < list.size(); i++) {
            currDateTIme = currDateTIme.plusMinutes(30);
            if (!list.get(i).dateTime.equals(currDateTIme)) {
                // we found a gap, meaning we skipped one half-hourly reading

                // create a weather data object and add it to the list
                String[] weatherReadings = new String[30];
                weatherReadings[0] = "hm";
                weatherReadings[1] = Integer.toString(Integer.parseInt(station));
                weatherReadings[2] = Integer.toString(currDateTIme.getYear());
                weatherReadings[3] = Integer.toString(currDateTIme.getMonthValue());
                weatherReadings[4] = Integer.toString(currDateTIme.getDayOfMonth());
                weatherReadings[5] = Integer.toString(currDateTIme.getHour());
                weatherReadings[6] = Integer.toString(currDateTIme.getMinute());
                // std time
                weatherReadings[7] = Integer.toString(currDateTIme.getYear());
                weatherReadings[8] = Integer.toString(currDateTIme.getMonthValue());
                weatherReadings[9] = Integer.toString(currDateTIme.getDayOfMonth());
                weatherReadings[10] = Integer.toString(currDateTIme.getHour());
                weatherReadings[11] = Integer.toString(currDateTIme.getMinute());

                weatherReadings[29] = "#";

                list.add(i, new ActualWD(currDateTIme, weatherReadings));
                System.out.println("At index " + i + ", we found a timestamp gap");
            }
        }
    }

    static int[] counter = new int[11];

    public static void fillAnyGaps(int arrayIndex, Reading whichVariable) {
        if (!whichVariable.isValid) {
            counter[arrayIndex]++;
        } else {
            if (counter[arrayIndex] > 0) {
                // do linear interpolation
                System.out.println("Took care of " + counter[arrayIndex] + " gaps for " + whichVariable.varName);
                counter[arrayIndex] = 0;
            }
        }
    }

    public static void findGaps(List<WeatherData> list) {
        // initialise an array as counter, size is the number of variables
        // if we find a gap for a variable, add one to the array
        // otherwise (no gaps), if the array value > 0
            // if array value <= 10: linear interpolate and fill in the gaps
                // how to fill in the gaps:
                    // take the gap count and the array returned by linear interpolation
                    // for loop j to fill in the values: index of weather data would be i - (gapCount + 1) - j
            // if array value <= 48: take the averages of the previous 24 hours and next 24 hours
            // otherwise, leave it empty and reset counter back to 0

        for (int i = 0; i < list.size(); i++) {
            fillAnyGaps(PRECIP, list.get(i).precip);
            fillAnyGaps(WBTEMP, list.get(i).wbTemp);
            fillAnyGaps(DPTEMP, list.get(i).dpTemp);
            fillAnyGaps(AIRTEMP, list.get(i).airTemp);
            fillAnyGaps(HUMIDITY, list.get(i).humidity);
            fillAnyGaps(VAP, list.get(i).vapPressure);
            fillAnyGaps(SATVAP, list.get(i).satVapPressure);
            fillAnyGaps(WINDSPD, list.get(i).windSpeed);
            fillAnyGaps(WINDDIR, list.get(i).windDir);
            fillAnyGaps(WINDGUST, list.get(i).windGust);
            fillAnyGaps(SEALVL, list.get(i).seaLvlPressure);
        }
        /*for (int i = 0; i < list.size(); i++) {
            if (!list.get(i).precip.isValid) {
                counter[PRECIP]++;
            } else {
                if (counter[PRECIP] > 0) {
                    // do linear interpolation
                    System.out.println("Took care of " + counter[PRECIP]);
                    counter[PRECIP] = 0;
                }
            }

            if (!list.get(i).wbTemp.isValid) {
                counter[WBTEMP]++;
            } else {
                if (counter[WBTEMP] > 0) {
                    // do linear interpolation
                    counter[WBTEMP] = 0;
                }
            }

            if (!list.get(i).dpTemp.isValid) {
                counter[DPTEMP]++;
            } else {
                if (counter[DPTEMP] > 0) {
                    // do linear interpolation
                    counter[DPTEMP] = 0;
                }
            }

            if (!list.get(i).airTemp.isValid) {
                counter[AIRTEMP]++;
            } else {
                if (counter[AIRTEMP] > 0) {
                    // do linear interpolation
                    counter[AIRTEMP] = 0;
                }
            }

            if (!list.get(i).humidity.isValid) {
                counter[HUMIDITY]++;
            } else {
                if (counter[HUMIDITY] > 0) {
                    // do linear interpolation
                    counter[HUMIDITY] = 0;
                }
            }

            if (!list.get(i).vapPressure.isValid) {
                counter[VAP]++;
            } else {
                if (counter[VAP] > 0) {
                    // do linear interpolation
                    counter[VAP] = 0;
                }
            }

            if (!list.get(i).satVapPressure.isValid) {
                counter[SATVAP]++;
            } else {
                if (counter[SATVAP] > 0) {
                    // do linear interpolation
                    counter[SATVAP] = 0;
                }
            }

            if (!list.get(i).windSpeed.isValid) {
                counter[WINDSPD]++;
            } else {
                if (counter[WINDSPD] > 0) {
                    // do linear interpolation
                    counter[WINDSPD] = 0;
                }
            }

            if (!list.get(i).windDir.isValid) {
                counter[WINDDIR]++;
            } else {
                if (counter[WINDDIR] > 0) {
                    // do linear interpolation
                    counter[WINDDIR] = 0;
                }
            }

            if (!list.get(i).windGust.isValid) {
                counter[WINDGUST]++;
            } else {
                if (counter[WINDGUST] > 0) {
                    // do linear interpolation
                    counter[WINDGUST] = 0;
                }
            }

            if (!list.get(i).seaLvlPressure.isValid) {
                counter[SEALVL]++;
            } else {
                if (counter[SEALVL] > 0) {
                    // do linear interpolation
                    counter[SEALVL] = 0;
                }
            }
        }*/
    }
}
