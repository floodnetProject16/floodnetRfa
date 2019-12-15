# FloodnetProject16 (Under construction)

## Introduction

As part of the objectives in theme 1.6 , the [NSERC FloodNet project](http://www.nsercfloodnet.ca/) has contributed to the R-package `CSHShydRology` that contains various tools for Canadian hydrologists, including flood frequency analysis.
The present R-package is built on top of `CSHShydRology` and has for objective to further automatize flood estimation according to FloodNet guidelines.
The package also provides a direct interface to the 
[HYDAT database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
to interact directly with a downloaded version of the database.
This interfacing is facilitated by the R-package (of the same name) provided by 
the [centre for hydrology](https://github.com/CentreForHydrology/HYDAT).

A demo of the package is presented [here](http://htmlpreview.github.io/?https://github.com/floodnetProject16/floodnetProject16/blob/master/vignettes/floodNetProject16.html)

## Installation

To install the R-package `floodnetProject16`, it is first required that the packages `CSHShydRology` and `HYDAT` be installed. This can be done using the R terminal.

    library(devtools)
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')


The R-package itself can be then installed.

    install_github('floodnetProject16/floodnetProject16')

Please note that each package has its own dependencies that will be automatically
downloaded from the default CRAN mirror. 
This may take some times...

In addition, it is recommended to download the SQLite version of the HYDAT database from the 
[National Water Data Archive](http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/).
For building and testing the R-package, the local path of the database must be written in the file `inst/config`.
The code below can later be used to create a variable `DB_HYDAT` that point to this database.

    source(system.file('config','floodnetProject16'))
    
The version of the database used for testing the package is dating from August 11, 2019.

