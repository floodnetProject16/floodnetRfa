library(floodnetRfa)
library(CSHShydRology)

rfaAmax <- function(station, period, distr)#, file, period)
{
  ## Station of interest and path of the HYDAT database
  #mystation <- station
  db <- './HYDAT/Hydat.sqlite3'
  if(distr == "Default") {
    distr = NULL
  }
  
  ## Filter nonstationary sites from the super region of the target
  target.supreg <- with(gaugedSites, supreg_km12[station == station])
  cond_supreg <- with(gaugedSites, supreg_km12 == target.supreg)
  
  pval.mk <- gaugedSites$trend_mk ## Mann-Kendall
  pval.pt <- gaugedSites$trend_pt ## Pettitt
  cond.trend <- pval.mk >= .05 & pval.pt >= .05
  mysites <- gaugedSites[cond_supreg & cond.trend,'station']
  
  addmargins(table(cond_supreg, cond.trend))
  
  ## Prepare hydrometric for pooling group in form of a dataset
  season.dist <- SeasonDistanceData(mysites, db)
  
  xd <- AmaxData(mysites, db, target = station, size = 25,
                 distance = season.dist)
  set.seed(1)
  out <- FloodnetPool(x = xd, target = station, 
                      period = period, distr = distr, out.model = TRUE, verbose = FALSE)
  return(out)
}