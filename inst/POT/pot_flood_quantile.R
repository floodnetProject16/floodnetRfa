library(floodnetProject16)
library(foreach)
library(doParallel)

rm(list = ls())

## Path of the HYDAT database
HYDAT <- "inst/extdata/Hydat.sqlite3"

## Size of the bootstrap sample
NSIM <- 2000

## Number of parallel processus
NCPU <- 5

## Temporary files used during parallel computing
CACHE <- 'C:/Users/mdurocher/Downloads/cache/cache_fpot'

## Filename for monitoring the computation
PROGRESS <- 'progress.log'

## Final output file with the flood quantiles
OUTFILE <- 'inst/POT/pot_flood_quantile.csv'

################################################
## ## Perform the POT analysis for all sites
################################################

cache.file <- file.path(CACHE, PROGRESS)

## Load the list of stations to analyse
stations <- gaugedSites$station
u <- gaugedSites$auto

## Monitoring file for parallel computing
t0 <- Sys.time()
write(format(t0), file = cache.file)

## Set-up parallel computing
cl <- makeCluster(NCPU)
registerDoParallel(cl)

statu <- foreach(ii = seq_along(stations),
                 .errorhandling = 'pass',
                 .packages = 'floodnetProject16') %dopar%{

	site <- stations[ii]

	write(paste0('Analyzing using POT ', site),
	      file = cache.file, append = TRUE)

	## extract the daily data
  con <- RSQLite::dbConnect(RSQLite::SQLite(), HYDAT)
	xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, site)[,2:3]
  RSQLite::dbDisconnect(con)

  ## Find non-empty years of data
  xd <- na.omit(xd)

  ## determine if a site is seasonally operated
  yy <- format(xd$date, '%Y')
  nn <- tapply(yy, yy, length)
  is.seasonal <- sum(nn > 346) < 10

  ## Padding the data for seasonally operated sites.
  ## it is assumed that missing values are not extremes.
  if(is.seasonal){

  	## Add missing days
  	rg.yy <- range(as.integer(yy))
  	rg.date <- as.Date(c(paste0(rg.yy[1], '/01/01'),
  							         paste0(rg.yy[2], '/12/31')))

  	all.date <- data.frame(date = seq(rg.date[1],rg.date[2], 'day'))
	  xd <- dplyr::left_join(all.date, xd, by = 'date')

	  ## Remove empty years (i.e. less than a month of data)
	  yy <- format(xd$date, '%Y')
	  nn <- tapply(xd$value, yy, function(z) sum(is.finite(z)))
	  xd <- xd[yy %in% names(nn[nn > 90]), ]

	  ## Impute zero to missing
	  xd[is.na(xd$value), 'value'] <- 0
  }

	out <- try(FloodnetPot(site = site,
										 x = xd,
	                   u = u[ii],
										 nsim = NSIM,
										 verbose = FALSE))

	if(class(out) == 'try-error'){
    return('Exit with error')

	} else {
	  pot.file <- paste0('pot',site,'.csv')
	  write.csv(out, file = file.path(CACHE, pot.file), row.names = FALSE)
	}

	return('Exit normally')
}

stopCluster(cl)

write(format(t0-Sys.time()), file = cache.file, append = TRUE)

print(unlist(statu))

###############################################
## Combine the output of the parallel computing
###############################################

file.lst <- list.files(CACHE, pattern = '*.csv')
Fz <- function(z) read.csv(file = file.path(CACHE,z))
all.file <- lapply(file.lst,Fz)
flood.quantile <- do.call(rbind, all.file)

write.csv(flood.quantile, file = OUTFILE, row.names = FALSE)

## verify if some file site were not analyzed
which( !(gaugedSites$station %in% unique(flood.quantile$site)) )
