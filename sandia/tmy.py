#!/usr/bin/env python3

"""
Calculate the Typical Meteorological Year (TMY) for a weather station given
a historical dataset

References:
    Wilcox, S., Marion, W., 'Users Manual for TMY3 Data Sets', NREL
"""

import sys, os
import argparse
import json
from datetime import datetime, timedelta
import calendar
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import matplotlib

matplotlib.style.use('ggplot')

def load_bom_csv_file(bom_file,params):
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

def clean_data(d):

    # remove months with NaNs
    d = d.groupby(pd.TimeGrouper('M')).filter(lambda x: not x.isnull().any().any())

    return d

def validate_data(d, config):
    """ Validate the data.
    
    Args:
        d (pd.DataFrame): historical weather data
        config (dict): configuration

    Returns:
        bool: True if data is continuous and complete, False otherwise.
    """
    print_details = config['verbose']
    valid = True

    # Ensure timestamps are unique
    if not d.index.is_unique:
        valid = False
        duplicates = d.groupby(level=0).filter(lambda x: len(x) > 1)
        nDups = len(duplicates)
        print( "Found {} duplicate timestamps".format(nDups) )
        if print_details:
            for g in duplicates:
                print(g)

    # Ensure montonic increasing
    if not d.index.is_monotonic_increasing:
        valid = False
        print( "Timestamps are not monotonic increasing")

    # Find gaps in the data
    t = pd.DataFrame(d.index)
    t['dt'] = t['time'].diff()
    t['gap']  = t['dt'] > timedelta(hours=1)
    t['gap']  = t['gap'].apply(lambda x: 1 if x else 0).cumsum() 
    gaps = t.groupby('gap').head(1).drop(0)
    nGaps = len(gaps) 
    if (nGaps > 0):
        valid = False
        lt6h = gaps[gaps.dt < timedelta(hours=6)]
        lt1d = gaps[(gaps.dt < timedelta(days=1)) & (gaps.dt > timedelta(hours=6))]
        lt1w = gaps[(gaps.dt < timedelta(days=7)) & (gaps.dt > timedelta(days=1))]
        lt1m = gaps[(gaps.dt < timedelta(days=30)) & (gaps.dt > timedelta(days=7))]
        gt1m = gaps[gaps.dt > timedelta(days=30)]
        print("Found {} gaps in the dataset:".format(nGaps))
        print("  {} gaps < 6 hours".format(len(lt6h)))
        print("  {} gaps < 1 day (> 6 hours)".format(len(lt1d)))
        print("  {} gaps < 1 week (> 1 day)".format(len(lt1w)))
        print("  {} gaps < 1 month (> 1 week)".format(len(lt1m)))
        print("  {} gaps > 1 month".format(len(gt1m)))

    # Find any timestamps that are not on the hour
    dt = pd.DatetimeIndex(t['time'])
    t['minute'] = dt.minute
    t['second'] = dt.second
    t['nanosecond'] = dt.nanosecond
    offHour = t.query('(minute != 0) | (second != 0) | (nanosecond != 0)')
    nOffHour = len(offHour)
    if (nOffHour > 0):
        valid = False
        print("Found {} times not on the hour in the dataset".format(nOffHour))
        if print_details:
            print(offHour)

    # Check for NaNs
    col_has_nan = d.isnull().any()
    if col_has_nan.any():
        valid = False
        print("Found {} NaN values present in dataset:".format(d.isnull().sum().sum()))
        for c in col_has_nan.index:
            if col_has_nan[c]:
                dnull = d[d[c].apply(np.isnan)]
                print("  {} rows with NaN values for {}".format(len(dnull),c))
                if print_details:
                    print(dnull)
        
    # Ensure there are enough valid years worth of data for each month
    years = []
    min_years = config['min_years_required']
    for m in range(1,13):
        years.append(len(np.unique(d[d.index.month==m].index.year)))

    if (min(years) < min_years):
        valid = False
        print("Found month(s) with less than {} years of data:".format(min_years))

    if print_details:
        print("Each month has the following number of years data avaliable:")
    
    if ( (min(years) < min_years) or print_details ):
        for i,yr in enumerate(years):
            print("  {:10} {}".format(calendar.month_name[i+1],yr))
        
    # Print warning if not valid
    if not valid:
        print("\nWARNING: dataset is incomplete.\n")

    return valid


def cdf(d,prop,bins):
    """
    Calculate the Cumulative Distribution Function (CDF) for the given property
    """
    y = d[prop].values
    y = y[~np.isnan(y)]
    h, b = np.histogram(y, bins, density=True)
    cdf = np.cumsum(h*np.diff(b))

    return cdf, b


def select_year(d, m, config):
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

        for yr in set(d[w].index.year):
            dy = d[d.index.year==yr]

            # calculate the CDF for this weight for specific year
            f, b  = cdf(dy,w,bin_edges)
            f[np.isnan(f)] = np.inf
            cdfs[w][yr] = f

            # Finkelstein-Schafer statistic (difference between long term
            # CDF and year CDF
            fs[w][yr] = np.mean( abs(cdfs[w]['all'] - cdfs[w][yr]) )

            # Add weighted FS value to score for this year 
            score[yr] += fs[w][yr] * weights[w]/total
            if np.isnan(score[yr]):
                print("score for {} is nan".format(yr))
                score[yr] = np.inf
    

    # select the top 5 years ordered by their weighted scores
    top5 = sorted(score,key=score.get)[:5]
    
    # TODO: select best year based on further statistics
    best_year = top5[0]

    if config['plot_cdf']:
        plot_cdfs(m,weights,x,fs,cdfs,set(d[w].index.year),best_year)

    return best_year


def plot_cdfs(m,weights,x,fs,cdfs,years_set,best_year):
    """
    Plot the cdfs for each param for each year
    """
    plt.figure()
    plt.suptitle("Typical Meteorological Year: {} = {}".format(
        calendar.month_name[m],best_year),fontsize=18)
    k = 1
    for w in weights:
        ax = plt.subplot(3,4,k)
        labels = []
        fs_min = min(fs[w],key=fs[w].get)
        for yr in cdfs[w]:
            labels.append(yr)
            ax.set_xlabel(w)
            ax.set_ylabel("CDF")
            ax.set_ylim(top=1)
            p = ax.plot(x,cdfs[w][yr])
            if yr == fs_min:
                plt.setp(p,color='r',linewidth=2)
            if yr == best_year:
                plt.setp(p,color='b',linewidth=2)
            if (yr == fs_min) and (yr == best_year):
                plt.setp(p,color=[1,0,1],linewidth=2)
            if yr == 'all':
                plt.setp(p,color='k',linewidth=2,linestyle='--')

        k += 1
        ax.legend(labels,prop={'size':9},loc='best')

        out = "{} => ".format(w.ljust(max(map(len,weights))))
        for yr in years_set:
            out += "{}: {:.3f}, ".format(str(yr)[2:],fs[w][yr])
        print(out)

    plt.show()


def calculate_tmy(d,config):
    """
    Calculate TMY from historical data using Sandia method
    """
    tmys = []
    for month in range(1,13):
        year = select_year(d,month,config)
        tmys.append(year)

    print("TMY for '{}' data set:\n{}".format(config['bomfile'],tmys))

    return tmys
    

def merge_months(d,tmy):
    """
    Merge selected months of TMY together
    """

    #TODO: this function is incomplete

    def reset_year(d):
        d.index=d.index.map(lambda x: x.replace(year=1901))

    out = []
    d_merge_last = None
    for mi,yr in enumerate(tmy):
        m = mi + 1
        dy = d[d.index.year==yr]   
        dm = dy[dy.index.month==m]
        #ndays = calendar.monthrange(1901,m)[1]
        buf = timedelta(hours=6)
        
        if (m > 1):
            s = datetime(yr,m,1,0,0,0)
            si = d.index.get_loc(s-buf)
            ei = d.index.get_loc(s+buf)+1
            d_merge = d[si:ei]
            reset_year(d_merge)
            
            for x in d_merge_last.index:
                s = s.replace(year=1901)
                r = (s + buf - x)
                t = float(r.total_seconds())/3600/12
                print( x, s, r, t)
                for col in d_merge_last:
                    #d_merge_last.loc(x,col) = t*d_merge_last[col][x] + (1-t)*d_merge[col][x]
                    d_merge_last.loc[x,col] = t*d_merge_last.loc[x,col] + (1-t)*d_merge.loc[x,col]

            print(d_merge_last)

        if (m < 12):
            e = datetime(yr,m+1,1,0,0,0)
            si = d.index.get_loc(e-buf)
            ei = d.index.get_loc(e+buf)+1
            d_merge_last = d[si:ei]
            reset_year(d_merge_last)
            print(d_merge_last)

        reset_year(dm)
        out.append(dm)
    
    dtmy = pd.concat(out)
    
    plt.figure()
    ax=plt.subplot(311)
    dtmy['awAirTemp'].plot()
    ax.ylabel('awAirTemp')

    ax=plt.subplot(312)
    dtmy['awWindSpeed'].plot()
    ax.ylabel('awWindSpeed')

    ax=plt.subplot(313)
    dtmy['slGhiMean'].plot()
    ax.ylabel('slChiMean')

    plt.show()


def validate_config(config):
    """ Ensure config is valid.
    """
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

    validate_config(config)

    if config['verbose']:
        print(json.dumps(config,indent=4))

    d = load_bom_csv_file(config['bomfile'],config['params'])

    d = clean_data(d)
    is_valid = validate_data(d,config)

    if True:
        tmy = calculate_tmy(d,config)

        #merge_months(d,tmy)


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

