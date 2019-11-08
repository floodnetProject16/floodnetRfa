# Scripts for performing flood frequency analysis.

## Description

This folder provides a demonstration of how the R packages 
"floodnetProject16" can be used inside scripts to routinely perform 
flood frequency analysis according to Floodnet recommendations.
This example is used to carry out flood estimation for a 
list of sites of interest. 

The script "example_r.R" loop across all sites of interest and output 
the flood quantile estimates in CSV files. 
All CSV files are later merged together in "flood_quantiles.csv"
The computing inside R is using the library "foreach" to perform 
parallel computing.

