@ECHO OFF

:: Starting program
echo Flood frequency analysis: Floodnet Project 1.6
echo start time: %time%

:: Loading global variables
CALL config.cmd

:: Convert HYDAT path following R convention "\" -> "/"
CALL SET HYDAT=%%HYDAT:\=/%%

:: Creating an empty file for saving the outputs
ECHO "site","method","distribution","period","variable","value" > %OUT%

:: Performing all the analyses
FOR /F %%x IN (%LST%) DO (
  CALL .\bin\floodnet_amax %%x 
  MORE +1 ~tmp.csv >> %OUT%
  
  CALL .\bin\floodnet_pot %%x 
  MORE +1 ~tmp.csv >> %OUT%
)

:: Clean up
DEL /F ~tmp.csv ~tmp.R

echo end time: %time%
pause
