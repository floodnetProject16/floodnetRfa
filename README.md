# FloodnetProject16 (Under construction)

## Introduction

As part of the objectives in theme 1.6, the [NSERC FloodNet project](http://www.nsercfloodnet.ca/) will contribute to the R-package `CSHShydRology`, which contains various tools for Canadian hydrologists, including flood frequency analysis.
The present R-package is built on top of `CSHShydRology` and has for objective to further automatize flood estimation according to FloodNet guidelines.
The package also provides a direct interface to the 
[HYDAT database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
to interact directly with a downloaded version of the database.
This interfacing is made possible by the R-package (of the same name) provided by 
the [centre for hydrology](https://github.com/CentreForHydrology/HYDAT).

Further descriptions of the package capabilities are presented [here](http://htmlpreview.github.io/?https://github.com/floodnetProject16/floodnetProject16/blob/master/inst/doc/floodNetProject16.html).

Please note that the package is in active development and may frequently change.

## Installation

To install the R-package `floodnetProject16`, it is necessary that the packages `CSHShydRology` and `HYDAT` be installed. This can be done using the R terminal.

    library(devtools)
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')

Afterwards, the R-package itself can be installed.

    install_github('floodnetProject16/floodnetProject16')

Each package has its own dependencies that are automatically
downloaded. This may take some time...

Also, it is recommended to download the SQLite version of the HYDAT database from the
[National Water Data Archive](http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/).
The version used for testing this package is dated August 11, 2019.

## Development

For building the vignette and testing the R-package locally, the path of the database must be saved in the file `inst/config`, which defines the variable `DB_HYDAT`. 
See the example `inst/config-example`.
Afterwards, the variable `DB_HYDAT` point to the desired database and can be loaded using 

    source(system.file('config','floodnetProject16'))
    
Finally, the vignette can be rebuild using

    devtools::build_vignettes()

