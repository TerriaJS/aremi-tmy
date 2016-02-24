import unittest
import filecmp
import pandas as pd
import sys
import os
import shutil
import json
sys.path.append(os.path.abspath(os.path.join("..", os.path.dirname(__file__))))
import tmy
import validate_data

CONFIG_DIR = os.path.abspath(os.path.join("..", os.path.dirname(__file__)))

class TmyTests(unittest.TestCase):

    def test_has_data_works(self):
        args = {"config": os.path.join(CONFIG_DIR, "tmy-config.json"),
              "verbose": False,
              "plot_cdf": False,
              "bomfile": "melbourne.csv"}
        tmy.main(args)

    def test_missing_data_fails(self):
        args = {"config": os.path.join(CONFIG_DIR, "tmy-config.json"),
              "verbose": False,
              "plot_cdf": False,
              "bomfile": "cape_grim.csv"}
        with self.assertRaises(SystemExit):
            tmy.main(args)

    def test_remove_months_with_nulls(self):
        test_csv_filepath = "test_csv.csv"
        expected_csv_filepath = "expected_csv.csv"
        # If we failed to clean up after the last test for some reason, do so now.
        if os.path.exists(test_csv_filepath):
           os.remove(test_csv_filepath)
        if os.path.exists(expected_csv_filepath):
           os.remove(expected_csv_filepath)

        with open(test_csv_filepath, 'w') as f:
           f.write("time,col1,col2,col3\n"\
                   "1998-01-01T10:00:00,,1,2\n"\
                   "1998-01-02T10:00:00,3,0,0\n"\
                   "1998-02-01T10:00:00,3,4,5\n"\
                   "1998-02-01T10:00:00,6,7,8\n"\
                   "1998-02-01T10:00:00,9,10,\n"\
                   "1998-03-01T10:00:00,11,12,13\n"\
                   "1998-04-01T10:00:00,14,15,14\n"\
                   "1998-04-01T10:00:00,16,17,18\n"\
                   "1998-04-01T10:00:00,19,20,21\n"\
                   "1999-05-01T10:00:00,19,20,21\n"\
                   "1999-05-01T10:00:00,19,20,21\n"\
                   "1998-05-01T10:00:00,,,\n")
        with open(expected_csv_filepath, 'w') as f:
           f.write("time,col1,col2,col3\n"\
                   "1998-03-01T10:00:00,11,12,13\n"\
                   "1998-04-01T10:00:00,14,15,14\n"\
                   "1998-04-01T10:00:00,16,17,18\n"\
                   "1998-04-01T10:00:00,19,20,21\n"\
                   "1999-05-01T10:00:00,19,20,21\n"\
                   "1999-05-01T10:00:00,19,20,21\n")
        d = pd.read_csv(test_csv_filepath, parse_dates=[0])
        d.set_index('time', inplace=True)
        col_names = ["col1", "col2", "col3"]

        d = tmy.removeMonthsWithNulls(col_names, d)
        expected_d = pd.read_csv(expected_csv_filepath, parse_dates=[0])
        expected_d.set_index('time', inplace=True)

        self.assertEqual(repr(d), repr(expected_d))

        # Clean up
        if os.path.exists(test_csv_filepath):
           os.remove(test_csv_filepath)
        if os.path.exists(expected_csv_filepath):
           os.remove(expected_csv_filepath)

    def test_data_duplicate_timestamps(self):
        test_csv_filepath = "little_melbourne_dup_timestamps.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._timestampsAreUnique())

    def test_data_timestamps_not_monotonic_increase(self):
        test_csv_filepath = "little_melbourne_no_monotonic.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._timestampsAreMonotonicIncreasing())

    def test_data_gaps(self):
        test_csv_filepath = "little_melbourne_has_gaps.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._dataHasNoGaps())

    def test_timestamps_not_on_hour(self):
        test_csv_filepath = "little_melbourne_ts_off_hour.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._timestampsAreOnTheHour())

    def test_nan_check(self):
        test_csv_filepath = "little_melbourne_has_nans.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._dataHasNoNans())

    def test_sufficient_data_1(self):
        # All months have less than nine (not ok)
        test_csv_filepath = "sufficient_data_01.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        config["verbose"] = True
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._sufficientDataAvailable())

    def test_sufficient_data_2(self):
        # One month has no data (not ok)
        test_csv_filepath = "sufficient_data_02.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        config["verbose"] = True
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertFalse(validator._sufficientDataAvailable())

    def test_sufficient_data_3(self):
        # 1 has nine or more but rest have 1 (ok)
        test_csv_filepath = "sufficient_data_03.csv"
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        config["verbose"] = True
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        d_no_null = tmy.removeMonthsWithNulls(config["params"].keys(), d)
        validator = validate_data.DataValidator(d, d_no_null, False, 9)
        self.assertTrue(validator._sufficientDataAvailable())

    def test_merge(self):
        test_csv_filepath = "months_merge.csv"
        test_csv_out_path = "months_merge_out.csv"
        test_csv_expected_path = "months_merge_out_tmy_expected.csv"

        if os.path.exists(test_csv_out_path):
           os.remove(test_csv_out_path)
        with open("tmy-config2.json") as f:
            config = json.load(f)

        config["verbose"] = True
        config["bomfile"] = test_csv_out_path
        d = tmy.loadBomCsvFile(test_csv_filepath, config["params"])
        typical_meterological_years = [2012, 2012]
        filepath_out = tmy.mergeMonths(d, typical_meterological_years, config)
        self.assertTrue(os.path.exists(filepath_out))
        self.assertTrue(filecmp.cmp(test_csv_expected_path, filepath_out))

        # Clean up
        if os.path.exists(filepath_out):
           os.remove(filepath_out)

    def test_update_solar_stations_csv_no_existing(self):
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        tmy_path = "SolarStationsTmy.csv"
        if os.path.exists(tmy_path):
           os.remove(tmy_path)
        self.assertFalse(os.path.exists("SolarStationsTmy.csv"))
        new_solar_path = tmy.updateSolarStationsCsv(86282, [2012, 2015, 1904], config, [5, 12, 2], "fake/filepath.csv")
        self.assertTrue(filecmp.cmp(new_solar_path, "SolarStationsTmy_orig.csv"))
        if os.path.exists(tmy_path):
           os.remove(tmy_path)

    def test_update_solar_stations_csv_existing(self):
        with open(os.path.join(CONFIG_DIR, "tmy-config.json")) as f:
            config = json.load(f)
        tmy_path = "SolarStationsTmy.csv"
        if os.path.exists(tmy_path):
           os.remove(tmy_path)
        shutil.copy("SolarStationsTmy_orig.csv", tmy_path)
        self.assertTrue(os.path.exists(tmy_path))
        new_solar_path = tmy.updateSolarStationsCsv(3003, [2222, 1993, 1901], config, [8, 7, 9], "fake/filepath.csv")
        expected_file_path = "SolarStationsTmy_existing_expected.csv"
        self.assertTrue(filecmp.cmp(new_solar_path, expected_file_path))

        if os.path.exists(tmy_path):
           os.remove(tmy_path)




if __name__ == '__main__':
    unittest.main()
