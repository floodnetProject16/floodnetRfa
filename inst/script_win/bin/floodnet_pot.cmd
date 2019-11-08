@echo on
SET R_HOME=C:\Program Files\Microsoft\R Open\R-3.5.1

:: Extract arguments
SET SITE=%1
SET OUTFILE=~tmp.csv
SET RSCRPT=~tmp.R

:: Create the Rscript
@echo out = floodnetProject16::FloodnetPot(site = "%SITE%",^
db = "%HYDAT%", quiet = TRUE, nsim = %NSIM%) > %RSCRPT%

@echo write.csv(out, file = "%OUTFILE%", row.names = FALSE) >> %RSCRPT%

:: Perform the analysis
echo [Analyzing %SITE% using POT]
CALL "%R_HOME%\bin\Rscript.exe" %RSCRPT%
