@ECHO ON

:: location of the R program
SET R_HOME=C:\Program Files\Microsoft\R Open\R-3.5.1

:: Path of the HYDAT database
SET HYDAT=..\extdata\Hydat.sqlite3

:: List of stations to analyze
SET LST=.\stations.txt

:: Output file containing the flood quantiles estimates
SET OUT=.\flood_quantiles.csv

:: Temporary folder
SET CACHE=.\cache

::Perform the analysis in R
IF NOT EXIST %CACHE% mkdir %CACHE%
CALL "%R_HOME%\bin\Rscript.exe" example_r.R

:: Merge resulting CSV files 
ECHO "site","method","distribution","period","variable","value" > %OUT%

FOR %%x IN (%CACHE%\*.csv) DO MORE +1 %%x >> %OUT%

:: Clean up
rmdir /Q /S %CACHE%

PAUSE
