library(floodnetRfa)
library(CSHShydRology)

inplacePot <- function(station, period)
{
  ## Station of interest and path to the HYDAT database
  #mystation <- station
  db <- './HYDAT/Hydat.sqlite3'
  set.seed(1)
  out <- FloodnetPot(site = station, db = db, area = 184, u = 20, period = period, out.model = TRUE)
  
  return(out)
}