# Scripts for performing flood frequency analysis.

## Description

This folder provides a demonstration of how the R packages 
"floodnetProject16" can be used inside scripts to routinely perform 
flood frequency analysis according to Floodnet recommendations.
This example is used to carry out flood estimation for a 
list of sites of interest. 

The script "example_script.cmd" loop across the list of stations 
provided in "stations.txt" and performs the analyses defined by the 
scripts in the folder "/bin".
These scripts are simple wraps around the R package functions,
which extract the data from the HYDAT database and output 
the flood quantile estimates in a CSV file. 
The scrips can be further customized by editing the global variables
in the file "config.cmd".

This example is done outside R 
