# floodnetRfa

**_Please note that the package is in active development and will change frequently._**

## Introduction

One objective [FloodNet](http://www.nsercfloodnet.ca/) is to provide Canadian engineers and hydrologists with a set of tools that allows them to perform flood frequency analysis (FFA) easily and accurately.
To this end, common methods in FFA were investigated and implemented in the R-package [CSHShydRology](https://github.com/floodnetProject16/CSHShydRology).
Another tool available to the Canadian water science community is the R-package [HYDAT](https://github.com/CentreForHydrology/HYDAT) that simplifies the communication between R and a local version of the [National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html).
The R-package `floodnetRfa` is built on the top of these two R-packages and 
aims to create en coherent environment for applying Floodnet recommendations.
The package includes instructions that can be invoked directly from the R terminal or via a graphical interface.

Further descriptions of the package capabilities are presented [here](https://drive.google.com/file/d/1I6JM9Gmkbnrn6p42gQYWDjJtpGazsUD6/view?usp=sharing) or in the package documentation.


## Installation

Before installing `floodnetRfa`, the packages `CSHShydRology` and `HYDAT` must be installed, which can be done manually by downloading them from their Github repository or by using the R terminal.

    library(devtools)
    
    ## Install the dependencies
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')

    ## Install the package
    install_github('floodnetProject16/floodnetRfa')

Each package has several dependencies that are automatically
downloaded.
The installation process may take some time...
It is also recommended to download the most recent version of the HYDAT database (sqlite3) from the
[National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html).

## Contributing to the package development

The development of the package was done using an HYDAT database dating of August 11th, 2019, which can be found [here](https://drive.google.com/file/d/1YI8pmB0U2Tp9FVVPpu2So8SmWIid9PsP/view?usp=sharing).
For building the vignette and run the tests, the path of the database must be saved in the file `inst/config` that defines the variable `DB_HYDAT`.
See the example `inst/config-example`.
After the package is built, the variable `DB_HYDAT` can be loaded using the command.

    source(system.file('config', package = 'floodnetRfa'))
    
and the vignette rebuilt using the command.

    devtools::build_vignettes()
