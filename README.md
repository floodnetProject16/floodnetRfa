# FloodnetRfa (Under construction)

## Introduction

As part of its objectives of theme 1.6, the [NSERC FloodNet project](http://www.nsercfloodnet.ca/) contributes to the R-package `CSHShydRology` by introducing various tools for Canadian hydrologists related to flood frequency analysis.
The R-package `floodnetRfa` is built on top of `CSHShydRology` and has for objective to further automatize flood estimation according to FloodNet guidelines.
The package allows interacting with the 
[HYDAT database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
trough the [centre for hydrology](https://github.com/CentreForHydrology/HYDAT).

Further descriptions of the package capabilities are presented [here](http://htmlpreview.github.io/?https://github.com/floodnetProject16/floodnetRfa/blob/master/inst/floodnetRfa.html), which are also found in the install folder.

**_Please note that the package is in active development and may frequently change._**

## Installation

Before installing `floodnetRfa`, the packages `CSHShydRology` and `HYDAT` must be present on the machine. 
The installation can be done manually or using the R terminal.

    library(devtools)
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')

Afterward, the R-package itself can be installed.

    install_github('floodnetProject16/floodnetRfa')

Each package has dependencies that are automatically
downloaded, which may take some time...

Also, it is recommended to download the SQLite version of the HYDAT database from the
[National Water Data Archive](http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/).
The version used for developing and testing the package is dated August 11, 2019.

## Development

For building the vignette and testing the R-package locally, the path of the HYDAT database must be saved in the file `inst/config` that defines the variable `DB_HYDAT`. 
See the example `inst/config-example`.
Afterward, the package is built, the variable `DB_HYDAT` can be loaded using the command.

    source(system.file('config', package = 'floodnetRfa'))
    
and will point to the desired database file and the vignette can be rebuilt using the command.

    devtools::build_vignettes()

