"""
Validate the input data for a weather station historical dataset.
"""
from datetime import timedelta
import pandas as pd
import calendar
import numpy as np

class DataValidator(object):
    """
    Validate the data. Most data will have some invalid sections.
    """

    def __init__(self, d, d_no_nulls, verbose, min_years_required):
        """
        @param d: pandas.DataFrame with bom data
        @param d_no_nulls: d with months that have null values for values of interest removed
        @param verbose: whether to print extra information
        @param min_years_required: minimum number of years of data that must exist to be able to analyse the data
        """
        self.d = d
        self.d_no_nulls = d_no_nulls
        self.verbose = verbose
        self.min_years = min_years_required

    def validate(self):
        """ Check everything we think might be a problem. """

        timestamps_are_unique = self._timestampsAreUnique()
        timestamps_are_monotonic_increasing = self._timestampsAreMonotonicIncreasing()
        no_gaps_exist_in_data = self._dataHasNoGaps()
        timestamps_are_on_the_hour = self._timestampsAreOnTheHour()
        data_has_no_nans = self._dataHasNoNans()
        sufficient_data_available = self._sufficientDataAvailable()

        print("\nReport on validity of data:\n")
        print("   Timestamps are unique:\t\t\t%s" % timestamps_are_unique)
        print("   Timestamps are monotonic increasing:\t\t%s" % timestamps_are_monotonic_increasing)
        print("   Data has no gaps:\t\t\t\t%s" % no_gaps_exist_in_data)
        print("   Timestamps are on the hour:\t\t\t%s" % timestamps_are_on_the_hour)
        print("   Data has no missing values (NaNs):\t\t%s" % data_has_no_nans)
        print("   Sufficient data available:\t\t\t%s" % sufficient_data_available)

        all_good = timestamps_are_unique &\
                   timestamps_are_monotonic_increasing &\
                   no_gaps_exist_in_data&\
                   timestamps_are_on_the_hour&\
                   data_has_no_nans&\
                   sufficient_data_available

        if all_good:
            return True

        # Some conditions have failed, but we can continue
        if not timestamps_are_unique or\
           not timestamps_are_monotonic_increasing or\
           not timestamps_are_on_the_hour or\
           not sufficient_data_available:
            print("\nERROR: No valid data found. See report above.\n")
            return False

        # All conditions weren't true, but none of the show-stoppers were true.
        print("\nWARNING: dataset is incomplete.\n")
        return True

    def _timestampsAreUnique(self):
        """ Are all the timestamps unique, that is, there are no duplicates? """
        if not self.d.index.is_unique:
            duplicates = self.d.groupby(level=0).filter(lambda x: len(x) > 1)
            nDups = len(duplicates)
            print( "Found {} duplicate timestamps".format(nDups) )
            if self.verbose:
                for g in duplicates:
                    print g
            return False
        return True

    def _timestampsAreMonotonicIncreasing(self):
        """ Are timestamps always increasing? """
        if not self.d.index.is_monotonic_increasing:
            print( "Timestamps are not monotonic increasing.")
            return False
        return True

    def _dataHasNoGaps(self):
        """ Are there any gaps in the data? """
        # Find gaps in the data
        t = pd.DataFrame(self.d.index)
        t['dt'] = t['time'].diff()
        t['gap']  = t['dt'] > timedelta(hours=1)
        t['gap']  = t['gap'].apply(lambda x: 1 if x else 0).cumsum()
        gaps = t.groupby('gap').head(1).drop(0)
        nGaps = len(gaps)
        if (nGaps > 0):
            lt6h = gaps[gaps.dt < timedelta(hours=6)]
            lt1d = gaps[(gaps.dt < timedelta(days=1)) & (gaps.dt > timedelta(hours=6))]
            lt1w = gaps[(gaps.dt < timedelta(days=7)) & (gaps.dt > timedelta(days=1))]
            lt1m = gaps[(gaps.dt < timedelta(days=30)) & (gaps.dt > timedelta(days=7))]
            gt1m = gaps[gaps.dt > timedelta(days=30)]
            print "Found {} gaps in the dataset:".format(nGaps)
            print("  {} gaps < 6 hours".format(len(lt6h)))
            print("  {} gaps < 1 day (> 6 hours)".format(len(lt1d)))
            print("  {} gaps < 1 week (> 1 day)".format(len(lt1w)))
            print("  {} gaps < 1 month (> 1 week)".format(len(lt1m)))
            print("  {} gaps > 1 month".format(len(gt1m)))
            return False
        return True

    def _timestampsAreOnTheHour(self):
        """ Are timestamps on the hour? """
        t = pd.DataFrame(self.d.index)
        # Find any timestamps that are not on the hour
        dt = pd.DatetimeIndex(t['time'])
        t['minute'] = dt.minute
        t['second'] = dt.second
        t['nanosecond'] = dt.nanosecond
        offHour = t.query('(minute != 0) | (second != 0) | (nanosecond != 0)')
        nOffHour = len(offHour)
        if (nOffHour > 0):
            print "Found {} times not on the hour in the dataset".format(nOffHour)
            if self.verbose:
                print offHour
            return False
        return True

    def _dataHasNoNans(self):
        """ Are there missing values in the dataset? """
        col_has_nan = self.d.isnull().any()
        if col_has_nan.any():
            print("Found {} NaN values present in dataset:".format(self.d.isnull().sum().sum()))
            for c in col_has_nan.index:
                if col_has_nan[c]:
                    dnull = self.d[self.d[c].apply(np.isnan)]
                    print("  {} rows with NaN values for {}".format(len(dnull),c))
                    if self.verbose:
                        print dnull
            return False
        return True

    def _sufficientDataAvailable(self):
        """ Ensure there are enough valid years worth of data for each month. """
        years = []

        for m in range(1,13):
            years.append(len(np.unique(self.d_no_nulls[self.d_no_nulls.index.month==m].index.year)))

        if (min(years) < self.min_years):
            print("Found month(s) with less than {} years of data:".format(self.min_years))

        if self.verbose:
            print("Each month has the following number of years data available:")

        total_years = 0
        if ( (min(years) < self.min_years) or self.verbose ):
            for i,yr in enumerate(years):
                print "  {:10} {}".format(calendar.month_name[i+1],yr)

        # Must have at least one year!
        for i,yr in enumerate(years):
            total_years = total_years + yr
        if total_years < 1:
            return False

        return True

