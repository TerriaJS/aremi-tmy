# aremi-tmy

A tool to work out Typical Meteorological Year (TMY) from ground weather station observations together with satellite solar irradiance data.


## License

This code is licensed under the open source Apache License Version 2.0. For full details please see the [LICENSE](LICENSE) file in the root of this repository.

Everything in here is &copy; CSIRO Data61 2017.


## Instructions

There are two programs, one written in Haskell and the other in Java - full instructions for building and running below.
Both of these software produces merged (and data-filled) solar and weather data, but each with different inputs, which is to be used for calculating the TMY.

### Haskell Software

#### Building
_Building this software requires Haskell Stack. This software relies on minute solar and weather data provided by the Bureau of Meteorology._

For initial setup, install  all of the necessary dependencies for this software:
`stack setup`

To build and compile the files:
`stack build`


#### Running
**Run the Haskell software to obtain the averaged data of each station**
`stack exec aremi-tmy-oneMinSolar <data>`

_NOTE: click [here](#data) to see how to pass the data files_

### Java Software

#### Building
_Building this software requires Java (Maven). This software relies on half-hourly weather data provided by the Bureau of Meteorology, and hourly solar satellite data._

To build and compile the files:
`mvn package`

#### Running
**Run the Java software to obtain the averaged data of each station**
`java solar_data/target/classes/Main`

_NOTE: this program will rely on these [here](#data) to produce the output._

Once the TMY has been obtained, to modify the output to have the same uniform year throughout the TMY file, run the ChangeYear program
`java solar_data/target/classes/ChangeYear path/to/file/<station_number>_averaged_tmy.csv`

### TMY Python Script
**Once the solar and weather data has been processed for each station, run the Python software to calculate the TMY for each station**
_Requires Python 2 (does not work with Python 3) and Anaconda 2*._
* To calculate TMY for a specific station:
`python sandia/tmy.py -c <config_file> -b <station number>_averaged.csv`

* To calculate TMY for all stations:
`python sandia/tmy.py -c <config_file> -a`

The config file will depend on which program has been used to process the solar and weather data:
Haskell: `tmy-config.json`
Java: `tmy-config-0.2.0.0.json`

## Typical Meteorological Year (TMY)
TODO


## Data
### For the Haskell program
The data obtained from different stations should be saved in a directory within the root in this way:
```
.
├── data
│   ├── 015590
│   ├── 016001
│   ├── ...
│   ├── IDCJAD00114_site_details_file.txt
│   ├── ...
```

Run the software automatically for all stations by passing the site details file:

`data/IDCJAD00114_site_details_file.txt`

### For the Java program

The program would work if the data is laid out within the root directory in the following way:
```
.
├── BoM_observations
│   ├── Half-hourly-data
│   │   ├── NSW_half_hourly
│   │   │   ├── HM01X_Data_046012_999999998661926.txt
│   │   │   ├── ... (the other station files)
│   │   │   ├── HM01X_StnDet_999999998661926.txt
│   │   │   ├── ...
│   │   ├── ... (the other states)
│   ├── Hourly-solar-data
│   │   ├── NSW
│   │   │   ├── 046012_dni_ghi.csv
│   │   │   ├── ... (the other station files)
│   │   ├── ... (the other states)
```

## AREMI
TODO


&copy; CSIRO Data61 2017.
