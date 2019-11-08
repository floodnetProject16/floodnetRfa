:: location of the R program
SET R_HOME=C:\Program Files\Microsoft\R Open\R-3.5.1

:: Path of the HYDAT database
SET HYDAT=..\extdata\Hydat.sqlite3

:: List of stations to analyze
SET LST=.\stations.txt

:: Output file containing the flood quantiles estimates
SET OUT=.\flood_quantiles.csv

:: Sample size for bootstrap inference
SET NSIM=2000
