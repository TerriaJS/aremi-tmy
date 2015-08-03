Calculate TMY using the Sandia method
=====================================

A python script for determining the best 12 months from a historical dataset.

Usage
-----

```
./tmy.py -c <config_file> <bom_file>
```

A `<bom_file>` contains hourly meterological data for a single location for a period of, 
typically, 10 or more years. Data is require to be complete and continuous (the script 
will print warnings if this is not the case)

A JSON `<config_file>` maps the columns in the provided `<bom_file>` to those parameters
used to calculate the TMY, and specifies the weights to be used for each parameter. See
example config.

References
----------

Users Manual for TMY3 Data Sets (Revised) - NREL 
  (www.nrel.gov/docs/fy08osti/43156.pdf)

National Solar Radiation Database 1991–2005 Update: User’s Manual 
  (http://www.nrel.gov/docs/fy07osti/41364.pdf)
