library(floodnetRfa)
library(CSHShydRology)

inplaceAmax <- function(station, period, distr)#, file, period)
{
  ## Station of interest and path of the HYDAT database
  #mystation <- station
  db <- './HYDAT/Hydat.sqlite3'
  if(distr == "Default") {
    distr = NULL
  }
  set.seed(1)
  out <- FloodnetAmax(site = station, db = db, period = period, distr = distr, out.model = TRUE)
  
  return(out)
}