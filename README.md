# floodnetRfa

**_This package is in development and will change frequently._**

## Introduction

One objective of [FloodNet](http://www.nsercfloodnet.ca/) is to provide Canadian engineers and hydrologists with a set of tools that allows them to perform flood frequency analysis (FFA) efficiently and accurately.
To this end, a fork of the R-package [CSHShydRology](https://github.com/floodnetProject16/CSHShydRology) was created and the methods the recommended methods were added.
Another tool available to the Canadian water science community is the R-package [HYDAT](https://github.com/CentreForHydrology/HYDAT) that simplifies the communication between R and a local version of the [National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html).
The R-package `floodnetRfa` is built on the top of these two R-packages and aims to create a coherent environment for carrying out the Floodnet recommendations.
The package includes functions that are invoked directly from the R terminal or via a shiny app.

An overall presentation of the GUI is provided [here](https://drive.google.com/file/d/1afqucRG49VSV7vPhFpKZV8WOMYdnauri/view?usp=sharing) and further descriptions of the package capabilities are presented [here](https://drive.google.com/file/d/1I6JM9Gmkbnrn6p42gQYWDjJtpGazsUD6/view?usp=sharing). 
Tutorials describing the functionality added to the CSHShydrology fork are available in the links below:

* [Annual Maxima](https://drive.google.com/file/d/1tmYs7yev8epRYL3b07YDxbdaWBxObt-0/view?usp=sharing)
* [Peak Over Threshold](https://drive.google.com/file/d/1pkOSuJauiVaXAiHh_CFC1mP2GjR_VqFv/view?usp=sharing)
* [Regional Frequency Analysis](https://drive.google.com/file/d/1FspvEqg4Mc2kmvg_womhidmE9MkYGPef/view?usp=sharing)
* [Prediction at Ungauged Sites](https://drive.google.com/file/d/1OI0uMTTPQ9loEgg2RRPWkkATKhNUR0jR/view?usp=sharing)



## Installation

First, R itself must be installed, which can be download from the official website [here](https://cloud.r-project.org/).
Once the R environment is set up, the two R-packages dependencies `CSHShydRology` and `HYDAT` must be installed.
These dependencies and the `floodnetRfa` can be download manually from the previous GitHub pages.
However, it is easier to use the R package `devtools` to perform these installations within the R terminal. 
The code below will install `floodnetRfa` and all its dependencies.

    library(devtools)
    
    ## Install the dependencies
    install_github('floodnetProject16/CSHShydRology')
    install_github('CentreForHydrology/HYDAT')

    ## Install the package
    install_github('floodnetProject16/floodnetRfa')

Note that the installation process may take some time.
We also recommend downloading the most recent version of the HYDAT database (sqlite3) from the
[National Water Data Archive](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html) as it can be used directly as input data.

After the installation, the Graphical User Interface (GUI) can be launched from the R terminal using the following instructions.  

    library(floodnetRfa)
    FloodnetApp()

If the user provides a file path to the function `FloodnetApp`, it creates instead a script that launches the GUI from the system terminal. For example, the code below creates a script on the desktop of the user 'JohnDoe'. 

    FloodnetApp(shortcut = "C:/Users/JohnDoe/Desktop/flood.cmd")


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

