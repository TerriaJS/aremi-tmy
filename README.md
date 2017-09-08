# aremi-tmy

A tool to work out Typical Meteorological Year (TMY) from ground weather station observations together with satellite solar irradiance data.


## License

This code is licensed under the open source Apache License Version 2.0. For full details please see the [LICENSE](LICENSE) file in the root of this repository.

Everything in here is &copy; CSIRO Data61 2017.


## Instructions

This software is written in Haskell - full instructions for building and running below.


### Building
_Building this software requires Haskell Stack._

For initial setup, install  all of the necessary dependencies for this software:

`stack setup`


To build and compile the files:

`stack build`


### Running
**Run the Haskell software to obtain the averaged data of each station**

`stack exec aremi-tmy-oneMinSolar <data>`

_NOTE: click [here](#data) to see how to pass the data files_


**Run the Python software to calculate the TMY for each station**

_Requires Python 2 (does not work with Python 3) and Anaconda 2*._

* To calculate TMY for a specific station:

`python sandia/tmy.py -c sandia/tmy-config.json -b <station number>_averaged.csv`

* To calculate TMY for all stations:

`python sandia/tmy.py -c sandia/tmy-config.json -a`



## Typical Meteorological Year (TMY)
TODO


## Data
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


## AREMI
TODO


&copy; CSIRO Data61 2017.
