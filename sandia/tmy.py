#!/usr/bin/python

"""
Calculate the Typical Meterological Year (TMY) for a weather station given
a historical dataset

References:
    Wilcox, S., Marion, W., 'Users Manual for TMY3 Data Sets', NREL
"""

import sys, os
sys.path.append(os.path.abspath(os.path.dirname(__file__)))
import validate_data
import argparse
import json
from datetime import datetime, timedelta
import calendar
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import matplotlib

matplotlib.style.use('ggplot')
pd.options.mode.chained_assignment = None  # default='warn'

def loadBomCsvFile(bom_file,params):
    """
    Load csv file containing weather station data into a pandas DataFrame
    """
    if ( not os.path.isfile(bom_file) ):
        raise IOError("BOM data file: '{}' not found".format(bom_file))

    col_names = dict( zip(params.values(), params.keys()) )

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


def calculateTmy(d,config):
    """
    Calculate TMY from historical data using Sandia method
    """
    tmys = []
    for month in range(1,13):
        year = selectYear(d,month,config)
        tmys.append(year)

    print "TMY for '{}' data set:\n{}".format(config['bomfile'],tmys)

    return tmys


def mergeMonths(d, tmys):
    """
    Merge selected months of TMY together
    """

    #TODO: this function is incomplete

    def resetYear(d):
        # This has to be a leap year, otherwise if you get a month from a leap year and try to convert it to a
        # non-leap year, you get "day is out of range for month".
        d.index=d.index.map(lambda x: x.replace(year=1904))

    out = []
    d_merge_last = None
    for mi,yr in enumerate(tmys):
        m = mi + 1
        dy = d[d.index.year==yr]
        dm = dy[dy.index.month==m]
        buf = timedelta(hours=6)

        if (m > 1):
            s = datetime(yr,m,1,0,0,0)
            si = d.index.get_loc(s-buf)
            ei = d.index.get_loc(s+buf)+1
            d_merge = d[si:ei]
            resetYear(d_merge)

            for x in d_merge_last.index:
                s = s.replace(year=1901)
                r = (s + buf - x)
                t = float(r.total_seconds())/3600/12
                print "x %s s %s r %s t %s" % (x, s, r, t)
                for col in d_merge_last:
                    d_merge_last.loc[x,col] = t*d_merge_last.loc[x,col] + (1-t)*d_merge.loc[x,col]

            #print d_merge_last

        if (m < 12):
            e = datetime(yr,m+1,1,0,0,0)
            si = d.index.get_loc(e-buf)
            ei = d.index.get_loc(e+buf)+1
            d_merge_last = d[si:ei]
            resetYear(d_merge_last)
            #print d_merge_last

        resetYear(dm)
        out.append(dm)

    dtmy = pd.concat(out)

    plt.figure()
    #ax=plt.subplot(311)
    #ax=plt.subplot()
    dtmy['global_horiz_radiation'].plot()
    #ax.ylabel('dry_bulb_tmp_mean')
    """
    ax=plt.subplot(312)
    dtmy['wind_velocity_mean'].plot()
    #ax.ylabel('wind_velocity_mean')

    ax=plt.subplot(313)
    dtmy['global_horiz_radiation'].plot()
    #ax.ylabel('global_horiz_radiation')
    """

    plt.show()


def validateConfig(config):
    """ Ensure config is valid."""
    # Check weights add up
    weights = dict(config['weights'])
    total = weights.pop('total')
    wsum = sum(weights.values())
    if (total != wsum):
        err_msg = "Weights sum ({}) and provided total ({}) do not match".format(wsum,total)
        raise IOError(err_msg)


def main(args):

    with open(args['config']) as f:
        config = json.load(f)

    config.update(args)

    validateConfig(config)

    if config['verbose']:
        print(json.dumps(config, indent=4))

    d = loadBomCsvFile(config['bomfile'], config['params'])
    # Keeping this in addition to d, because we want to report on any nulls that exist to the user.
    d_no_nulls = removeMonthsWithNulls(config["params"].keys(), d)
    validator = validate_data.DataValidator(d, d_no_nulls, config["verbose"], config["min_years_required"])
    success = validator.validate()
    if not success:
        print("Cannot continue with invalid data. Exiting...")
        sys.exit(1)

    tmys = calculateTmy(d_no_nulls, config)
    mergeMonths(d, tmys)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Calculate a Typical Meteorological Year (TMY)')
    parser.add_argument('-c','--config',metavar="<config_file>",required=True,
            help='JSON config file for TMY settings')
    parser.add_argument('-v','--verbose',action='store_true',
            help='Print lots of info')
    parser.add_argument('-p','--plot-cdf',action='store_true',
            help='Plot CDF for each param for each year')
    parser.add_argument('bomfile',metavar="<bom_file>",
            help='CSV file containing weather station data')
    args = parser.parse_args(sys.argv[1:])

    main( vars(args) )

