library(foreach)
library(doParallel)

rm(list = ls())

source('inst/trend/trend_lib.R')

## Path of the HYDAT database
HYDAT <- "inst/extdata/Hydat.sqlite3"

## Size of the bootstrap sample
NSIM <- 2000

RES <- 560

## Number of parallel processus
NCPU <- 4

## Temporary files used during parallel computing
CACHE <- 'C:/Users/mdurocher/Downloads/cache/cache_trend'

## Filename for monitoring the computation
PROGRESS <- 'progress.log'

################################################
## ## Perform the POT analysis for all sites
################################################

cache.file <- file.path(CACHE, PROGRESS)

## Load the list of stations to analyse
stations <- gaugedSites$station

## Monitoring file for parallel computing
t0 <- Sys.time()
write(format(t0), file = cache.file)

## Set-up parallel computing
cl <- makeCluster(NCPU)
registerDoParallel(cl)

statu <- foreach(ii = seq_along(stations),
                 .errorhandling = 'pass',
                 .packages = c('CSHShydRology', 'floodnetProject16',
                 							'Kendall','boot','splines','trend')) %dopar%{

  site <- stations[ii]

  write(paste0('(', ii ,') Analyzing using Trend ', site),
	      file = cache.file, append = TRUE)

  ##########################
	## Trend in the AMAX data
	##########################

  ## read annual maximum from HYDAT
	con <- RSQLite::dbConnect(RSQLite::SQLite(), HYDAT)
	an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, site)
  RSQLite::dbDisconnect(con)

  an <- an[an$peak == 'MAXIMUM', c(3,6)]
  an <- na.omit(an)
  nyear <- nrow(an)

  ## Compute the p-value of the MannKendall test
  mk <- AutoMK(an$value)
  mk.pvalue <- mk[2]

  ## Compute the p-value of the Pettitt test
  pt <- pettitt.test(an$value)
  pt.pvalue <- pt$p.value

  ##########################
	## Trend in the POT
	##########################

  ## Read Daily data
	con <- RSQLite::dbConnect(RSQLite::SQLite(), HYDAT)
	xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, site)
  RSQLite::dbDisconnect(con)

  xd <- na.omit(xd[,c('date','value')])

  ## Identify peaks using previously selected thresholds
  u <- gaugedSites[gaugedSites$station == site, 'auto']
  area <- gaugedSites[gaugedSites$station == site, 'auto']
  peaks <- which.floodPeaks(value~date, xd , u = u, r = 4 + log(area))

  ## Compute p-value for the logistic model (Trend in number of peaks)
  logis.pvalue <- TrendLogis(xd$date, peaks)

  ## Compute p-value for the Mann-Kendall test (Trend in mean excess)
  mex.pvalue <- AutoMK(xd[peaks,'value'])[2]

  ####################################
  ## save the p-values
  ####################################
  all.pvalue <- data.frame(trend_mk = mk.pvalue,
  											trend_pt = pt.pvalue,
  											trend_lg = logis.pvalue,
  											trend_mx = mex.pvalue)

  pval.file <- paste0(paste0(site, '.csv'))
  write.csv(round(all.pvalue,4),
  					file = file.path(CACHE, pval.file),
  					row.names = FALSE)

  ####################################
  ## Summary graphics
  #####################################

  ## Compute the fitted value of the change point identify in the Pettitt test
  ptk <- pt$estimate
  pt1 <- mean(an$value[1:ptk])
  pt2 <- mean(an$value[(ptk +1):nyear], na.rm = TRUE)
  pt.yhat <- rep(pt2,nyear)
  pt.yhat[1:ptk] <- pt1


  fig.file <- paste0(paste0(site, '.png'))
	png(file = file.path(CACHE, fig.file), height = 2*RES, width = 1.5*RES)

	par(mfrow = c(2,1), mar = c(5,5,5,5))

	##---- Plot the AMAX ----##
  plot(an, type = 'b',
  		 xlab = 'Year', ylab = 'Annual maxima',
  		 main = paste0('Trend AMAX: MK=', round(mk.pvalue,3),
  		 							 '; PT=', round(pt.pvalue,3) ))

  ## estimate a smooth trend
  an0 <- an
  an0$value <- log(an0$value)
  an.sm <- smooth.spline(an0)
  an.sm$y <- exp(an.sm$y)

  lines(an.sm, col = 'red', lty = 2, lwd = 2)
  lines(an$year,pt.yhat, col = 'blue', lwd = 3)

  ##----- Plot of the POT -----##

  plot(xd[peaks,], pch = 3, ylab = 'Flood peaks', xlab = 'Date',
  		 main = paste0('Trend : MX=', round(mex.pvalue,3),
  		 							 '; LG=', round(logis.pvalue,3) ))

  ## Add a smooth lines for the mean excess
  xd0 <- xd[peaks,]
  xd0$value <- log(xd0$value)
  xd.sm <- smooth.spline(xd0)
  xd.sm$y <- exp(xd.sm$y)
  lines(xd.sm, col = 'red', lwd = 3)

  ## Add a smooth line for the number of PPY
  npy <- tapply(seq_along(xd$date) %in% peaks,
  							format(xd$date,'%Y'), sum)

  np.sm <- smooth.spline(log(npy+1))

  npd <- as.Date(paste0(names(npy),'/07/15'))

  par(new = TRUE)
  plot(npd, exp(np.sm$y)-1, axes = FALSE, ylab = NA, xlab = NA, pch = 16, col = 'blue',
  		 ylim = c(0,5), type = 'l', lty = 2, lwd = 3)

  axis(side = 4)
  mtext(side = 4, line = 3, 'Number of PPY')

  legend('top', horiz = TRUE, legend = c('Mean excess','trend in PPY'),
  			 lty = 1:2, col = c('red','blue'))

  dev.off()

  return(0)
}

stopCluster(cl)

fun <- function(z) read.csv(file = file.path(CACHE, paste0(z,'.csv')))
trend <- lapply(stations,fun)
trend <- cbind(stations, do.call(rbind, trend))

gaugedSites <- cbind(gaugedSites,trend[,-1])
save(gaugedSites, file = 'gauged_sites.rda')
