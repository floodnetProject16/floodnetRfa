library(floodnetRfa)
library(CSHShydRology)

rfaPot <- function(station, period)
{
  ## Station of interest and path to the HYDAT database
  #mystation <- station
  db <- './HYDAT/Hydat.sqlite3'
  
  ## Filter nonstationary sites from the super region of the target
  target.supreg <- with(gaugedSites, supreg_km12[station == station])
  cond_supreg <- with(gaugedSites, supreg_km12 == target.supreg)
  pval.mx <- gaugedSites$trend_mx ## Mann-Kendall
  pval.lg <- gaugedSites$trend_lg ## logistic regression
  cond.trend <- pval.lg >= .05 & pval.mx >= .05
  
  info <- gaugedSites[cond_supreg & cond.trend, c('station','auto','area')]
  season.dist <- SeasonDistanceData(info$station, db)
  addmargins(table(cond_supreg, cond.trend))
  
  xd <- DailyPeaksData(info, db, target = station, size = 25, distance = season.dist)
  
  set.seed(1)
  out <- FloodnetPool(xd, target = station, period = period, verbose = TRUE, out.model = TRUE)
  
  return(out)
}