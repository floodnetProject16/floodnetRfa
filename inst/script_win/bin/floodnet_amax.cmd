@ECHO ON

:: Extract arguments
SET SITE=%1
SET OUTFILE=~tmp.csv
SET RSCRPT=~tmp.R

:: Create the Rscript to execute
@echo out = floodnetProject16::FloodnetAmax(site = "%SITE%",^
db = "%HYDAT%", quiet = TRUE, nsim = %NSIM%) > %RSCRPT%

@echo write.csv(out, file = "%OUTFILE%", row.names = FALSE) >> %RSCRPT%

:: Perform the analysis
ECHO [Analyzing %SITE% using AMAX]
CALL "%R_HOME%\bin\Rscript.exe" %RSCRPT%
