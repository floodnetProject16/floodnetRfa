# floodnetRfa

**_This package is in active development and will change frequently._**

## Introduction

One objective of [FloodNet](http://www.nsercfloodnet.ca/) is to provide Canadian engineers and hydrologists with a set of tools that allows them to perform flood frequency analysis (FFA) efficiently and accurately.
To this end, a fork of the R-package [CSHShydRology](https://github.com/floodnetProject16/CSHShydRology)conventional was created and the methods the recommended methods were implemented.
Another tool available to the Canadian water science community is the R-package [HYDAT](https://github.com/CentreForHydrology/HYDAT) that simplifies the communication between R and a local version of the [National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html).
The R-package `floodnetRfa` is built on the top of these two R-packages and aims to create a coherent environment for carrying out the Floodnet recommendations.
The package includes functions that are invoked directly from the R terminal or via a shiny app.

An overall presentation of the GUI is provided [here]() while further descriptions of the package capabilities are presented [here](https://drive.google.com/file/d/1I6JM9Gmkbnrn6p42gQYWDjJtpGazsUD6/view?usp=sharing) 
Tutorials describing the functionality added to the CSHShydrology fork are available in the links below:

* [Annual Maxima](https://drive.google.com/file/d/1tmYs7yev8epRYL3b07YDxbdaWBxObt-0/view?usp=sharing)
* [Peak Over Threshold](https://drive.google.com/file/d/1pkOSuJauiVaXAiHh_CFC1mP2GjR_VqFv/view?usp=sharing)
* [Regional Frequency Analysis](https://drive.google.com/file/d/1FspvEqg4Mc2kmvg_womhidmE9MkYGPef/view?usp=sharing)
* [Prediction at Ungauged Sites](https://drive.google.com/file/d/1OI0uMTTPQ9loEgg2RRPWkkATKhNUR0jR/view?usp=sharing)



## Installation

First, an R environment must be installed, which can be download from the official website [here](https://cloud.r-project.org/).
Once the environment is set up, the two R-packages dependencies `CSHShydRology` and `HYDAT` must be installed, before installing the R-packages itself.
The dependencies and the `floodnetRfa` can be download manually from the previous GitHub pages or the whole installation process can be simplified by using the tool `devtools`. 
In this line, the code below can be executed in R for a complete installation.

    library(devtools)
    
    ## Install the dependencies
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')

    ## Install the package
    install_github('floodnetProject16/floodnetRfa')

The installation process may take some time because each package has dependencies that are automatically downloaded.
Moreover, we also recommend downloading the most recent version of the HYDAT database (sqlite3) from the
[National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html) as a natural input for FFA.

## Supplementary material

Two companion datasets: `gauged_sites.csv` and `descriptors.csv` can be downloaded [here](https://drive.google.com/file/d/1TD-XhX1hQtjcWQFSKErzOvlBXfvYtrnf/view?usp=sharing).
Their content has been collected during the FloodNet Project, and they illustrate the information needed by a user to perform FFA with `floodnetRfa`.
Please note that the companion datasets are provided for demonstration purposes only.
Some other supplementary materials related to `floodnetRfa` and the companion datasets are available [here](https://github.com/martindurocher/floodnetRfa_extra). 

## For developers

The development of the package used an HYDAT database dating of August 11th, 2019 that can be found [here](https://drive.google.com/file/d/1YI8pmB0U2Tp9FVVPpu2So8SmWIid9PsP/view?usp=sharing).
For building the vignette and run the tests, the path of the database and companion dataset must be indicated in the file `inst/config`.
See the example `inst/config-example`.
When the package is built, the global variable can be loaded using the command.

    source(system.file('config', package = 'floodnetRfa'))
    
and the vignette rebuilt using the command.

    devtools::build_vignettes()

