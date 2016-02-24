#!/usr/bin/python

"""
Calculate the Typical Meterological Year (TMY) for a weather station given
a historical dataset

References:
    Wilcox, S., Marion, W., 'Users Manual for TMY3 Data Sets', NREL
"""

import sys, os, re
sys.path.append(os.path.abspath(os.path.dirname(__file__)))
import validate_data
import argparse
import json
import random
import string
import wget
import zipfile
import shutil
from datetime import datetime, timedelta
import calendar
import collections
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import matplotlib

matplotlib.style.use('ggplot')


def loadBomCsvFile(bom_file,params):
    """
    Load csv file containing weather station data into a pandas DataFrame
    """
    if ( not os.path.isfile(bom_file) ):
        raise IOError("BOM data file: '{}' not found".format(bom_file))

    col_names = dict( zip(params.values(), params.keys()) )

    print("About to read %s" % bom_file)
    d = pd.read_csv(bom_file,usecols=col_names,parse_dates=[params['time']])
    d.rename(columns=col_names,inplace=True)
    d.set_index('time',inplace=True)

    return d

def removeMonthsWithNulls(col_names, d):
    """ Remove any months that have rows that contain a null, as we can't do statistics on this. """
    months_to_remove = set()
    nulls = d.isnull()
    for (date, series) in nulls.iterrows():
        for (ind, val) in series.iteritems():
            if val:
                months_to_remove.add((date.month, date.year))

    for month, year in list(months_to_remove):
        _, end_month = calendar.monthrange(year, month)
        start = datetime(year,month,1,0,0,0)
        end = datetime(year,month,end_month,0,0,0) + timedelta(days=1)
        d = d[(d.index > end) | (d.index < start)]
    return d

def cdf(d,prop,bins):
    """
    Calculate the Cumulative Distribution Function (CDF) for the given property
    """
    y = d[prop].values
    y = y[~np.isnan(y)]
    h, b = np.histogram(y, bins, density=True)
    cdf = np.cumsum(h*np.diff(b))

    return cdf, b


def selectYear(d, m, config):
    """
    Use the Sandia method, to select the most typical year of data
    for the given month
    """
    d = d[d.index.month==m]
    n_bins = config['cdf_bins']

    weights = dict(config['weights'])
    total = weights.pop('total')

    score = dict.fromkeys(d.index.year,0)
    fs = dict.fromkeys(weights)
    cdfs = dict.fromkeys(weights)

    for w in weights:
        cdfs[w] = dict([])
        fs[w] = dict([])

        # Calculate the long term CDF for this weight
        cdfs[w]['all'], bin_edges = cdf(d,w,n_bins)

        x = bin_edges[:-1] * np.diff(bin_edges)/2

        for yr in set(d.index.year):
            dy = d[d.index.year==yr]

            # calculate the CDF for this weight for specific year
            cdfs[w][yr], b  = cdf(dy,w,bin_edges)

            # Finkelstein-Schafer statistic (difference between long term
            # CDF and year CDF
            fs[w][yr] = np.mean( abs(cdfs[w]['all'] - cdfs[w][yr]) )

            # Add weighted FS value to score for this year
            score[yr] += fs[w][yr] * weights[w]/total


    # select the top 5 years ordered by their weighted scores
    top5 = sorted(score,key=score.get)[:5]

    # TODO: select best year based on further statistics
    best_year = top5[0]

    if config['plot_cdf']:
        plotCdfs(m,weights,x,fs,cdfs,set(d.index.year),best_year)

    return best_year


def plotCdfs(m,weights,x,fs,cdfs,years_set,best_year):
    """
    Plot the cdfs for each param for each year
    """
    plt.figure()
    plt.suptitle("Typical Meteorolgical Year: {} = {}".format(
        calendar.month_name[m],best_year),fontsize=18)
    k = 1
    for w in weights:
        ax = plt.subplot(3,4,k)
        labels = []
        fs_min = min(fs[w],key=fs[w].get)
        for yr in years_set:
            labels.append(yr)
            ax.set_xlabel(w)
            ax.set_ylabel("CDF")
            ax.set_ylim(top=1)
            p = ax.plot(x,cdfs[w][yr])
            if (yr == fs_min):
                plt.setp(p,color='r',linewidth=2)
            if (yr == best_year):
                plt.setp(p,color='b',linewidth=2)
            if (yr == fs_min) and (yr == best_year):
                plt.setp(p,color=[1,0,1],linewidth=2)

        k += 1
        ax.plot(x,cdfs[w]['all'],color='k',linewidth=2,linestyle='--')
        labels.append('All')
        ax.legend(labels,prop={'size':9},loc='best')

        out = "{} => ".format(w.ljust(max(map(len,weights))))
        for yr in years_set:
            out += "{}: {:.3f}, ".format(str(yr)[2:],fs[w][yr])
        print out

    plt.show()


def calculateTmy(d, config):
    """
    Calculate TMY from historical data using Sandia method
    """
    tmys = []
    for month in range(1,13):
        year = selectYear(d,month,config)
        tmys.append(year)

    print "TMY for '{}' data set:\n{}".format(config['bomfile'],tmys)
    return tmys


def mergeMonths(d, tmys, config):
    """
    Merge selected months of TMY together. No interpolation done at this time.
    """
    out = []
    for mi,yr in enumerate(tmys):
        m = mi + 1
        dy = d[d.index.year==yr]
        dm = dy[dy.index.month==m]
        out.append(dm)

    dtmy = pd.concat(out)
    col_names = dict(zip(config["params"].keys(), config["params"].values()))
    dtmy.rename(columns=col_names, inplace=True)
    filepath_out = os.path.splitext(config["bomfile"])[0] + "_tmy" + os.path.splitext(config["bomfile"])[-1]
    print("Writing merged months to %s" % filepath_out)
    dtmy.to_csv(filepath_out, index_label=config["params"]["time"])
    return filepath_out


def updateSolarStationsCsv(station_num, tmys, config, years, merged_csv_filepath):
    """
    Append tmy information to solar stations csv. If this is in current dir, use that one, because that allows us to
    process multiple stations at once. Otherwise download from data source.
    """
    new_solar_path, new_file = getSolarStationsPath(config)

    bom_station_num = "Bureau of Meteorology station number"
    download_tmy = "Download TMY (CSV)"
    new_data = collections.OrderedDict({bom_station_num: station_num})
    new_data[download_tmy] = "<a href='%s%s'>TMY File</a>" % (config["csv_datasets_url"], merged_csv_filepath)
    for i, typical_meterological_year in enumerate(tmys):
        month = i + 1
        new_data[calendar.month_name[month] + " TMY"] = [typical_meterological_year]

    for i, year in enumerate(years):
        month = i + 1
        new_data[calendar.month_name[month] + " valid years"] = [year]
    new_data_pd = pd.DataFrame(new_data, columns = new_data.keys())
    solar_stations = pd.read_csv(new_solar_path)

    if new_file:
        # If the file has been downloaded, it might contain old data for January TMY etc. Clear this.
        new_data_keys = new_data.keys()
        new_data_keys.remove(bom_station_num)
        for new_data_key in new_data_keys:
            try:
                del solar_stations[new_data_key]
            except:
                print("Couldn't delete %s from downloaded solar stations. It's possible it doesn't exist." % new_data_key)

        full_data = pd.merge(left=solar_stations, right=new_data_pd, how='outer')
        print("Writing solar stations csv to %s" % new_solar_path)
        full_data.to_csv(new_solar_path, index=False)
    else:
        cols = solar_stations.columns
        solar_stations.set_index(bom_station_num, inplace=True)
        new_data_pd.set_index(bom_station_num, inplace=True)
        solar_stations.update(new_data_pd, overwrite=True)

        solar_stations.reset_index(drop=False, inplace=True)
        solar_stations = solar_stations[cols]
        print("Writing solar stations csv to existing file %s" % new_solar_path)
        solar_stations.to_csv(new_solar_path, index=False, cols=cols)
    return new_solar_path


def getSolarStationsPath(config):
    new_solar_path = "SolarStationsTmy.csv"
    new_file = False
    if not os.path.exists(new_solar_path):
        print("Can't find %s. Downloading..." % new_solar_path)
        solar_stations_csv = wget.download(config["solar_stations_url"])
        shutil.move(solar_stations_csv, new_solar_path)
        new_file = True
    return new_solar_path, new_file


def validateConfig(config):
    """ Ensure config is valid."""
    # Check weights add up
    weights = dict(config['weights'])
    total = weights.pop('total')
    wsum = sum(weights.values())
    if (total != wsum):
        err_msg = "Weights sum ({}) and provided total ({}) do not match".format(wsum,total)
        raise IOError(err_msg)


def main(args, continue_on_fail=False):
    with open(args['config']) as f:
        config = json.load(f)

    config.update(args)

    validateConfig(config)

    if config['verbose']:
        print(json.dumps(config, indent=4))

    if 'all' in args and args['all']:
        doAll(args, config)

    else:
        if args["bomfile"] is None:
            print("ERROR: Need to specify bom file, unless option 'all' is specified.")
            sys.exit(1)
        doOne(args, config, continue_on_fail)
    return True


def doAll(args, config):
    # Next line from http://stackoverflow.com/questions/2257441/random-string-generation-with-upper-case-letters-and-digits-in-python
    rand_str = ''.join(random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(5))
    dirname = "tmy_outfiles_%s" % rand_str
    orig_dir = os.getcwd()
    os.mkdir(dirname)
    os.chdir(dirname)

    solar_stations_path, _ = getSolarStationsPath(config)
    solar_stations = pd.read_csv(solar_stations_path)
    # Don't want this in list of csvs to process
    os.remove(solar_stations_path)

    data_files = solar_stations.loc[:, config["ss_download_historical_obs"]]
    for data_file in data_files:
        hist_obs_url = re.search("<a href='(.*)'>", data_file).group(1)
        hist_objs_zip = wget.download(hist_obs_url)

        zfile = zipfile.ZipFile(hist_objs_zip)
        # Can only be used with trustworthy zips
        zfile.extractall()
        os.remove(hist_objs_zip)

    hist_obs_csvs = os.listdir(os.getcwd())
    fail_csvs = []
    for hist_obs_csv in hist_obs_csvs:
        print("------------------------------------\n")
        config["bomfile"] = hist_obs_csv
        tmys, merged_csv_filepath, validator, station_num = doOne(args, config, continue_on_fail=True)
        if tmys is None:
            fail_csvs.append(hist_obs_csv)
            continue
        updateSolarStationsCsv(station_num,
                               tmys,
                               config,
                               validator.getValidYearsOfDataForEachMonth(),
                               merged_csv_filepath)

    for hist_obs_csv in hist_obs_csvs:
        os.remove(hist_obs_csv)

    print("\n See %s for output files." % dirname)
    if len(fail_csvs) > 0:
        print("Some files appeared to have no valid data. These weren't processed. They were:")
        for f in fail_csvs:
            print(f)
    os.chdir(orig_dir)


def doOne(args, config, continue_on_fail):
    d = loadBomCsvFile(config['bomfile'], config['params'])
    # Keeping this in addition to d, because we want to report on any nulls that exist to the user.
    d_no_nulls = removeMonthsWithNulls(config["params"].keys(), d)
    validator = validate_data.DataValidator(d, d_no_nulls, config["verbose"], config["min_years_required"])
    success = validator.validate()
    if not success:
        if not continue_on_fail:
            print("Cannot continue with invalid data. Exiting...")
            sys.exit(1)
        else:
            return (None, None, None, None)

    tmys = calculateTmy(d_no_nulls, config)
    merged_csv_filepath = mergeMonths(d, tmys, config)
    station_num = int(d.loc[:, config["params"]["station"]][-1])
    return (tmys, merged_csv_filepath, validator, station_num)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Calculate a Typical Meteorological Year (TMY)')
    parser.add_argument('-a','--all',action='store_true', default=False,
            help='Download every station, work out tmy, and dump out csv files including updating master csv.'\
                 ' Otherwise, provide a bomfile argument to operate on.')
    parser.add_argument('-c','--config',metavar="<config_file>",required=True,
            help='JSON config file for TMY settings')
    parser.add_argument('-v','--verbose',action='store_true',
            help='Print lots of info')
    parser.add_argument('-p','--plot-cdf',action='store_true',
            help='Plot CDF for each param for each year')
    parser.add_argument('-b','--bomfile',metavar="<bom_file>", default=None, required=False,
            help='CSV file containing weather station data (required if "all" not specified)')
    args = parser.parse_args(sys.argv[1:])

    main(vars(args))

