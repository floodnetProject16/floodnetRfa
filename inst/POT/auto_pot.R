library(floodnetProject16)
library(tidyverse)
library(foreach)
library(doParallel)

rm(list = ls())

## Path of the HYDAT database
HYDAT <- "inst/extdata/Hydat.sqlite3"

## Path of a temporary folder to save files
CACHE <- "inst/POT/cache"

## File to monitor progress
PROGRESS <- "progress.log"

## Resolution of the graphics in pixels (RES x ReS)
RES_FIG <- 640

## Number of parallel processes
NCPU <- 5

##########################################
##########################################

## Load the list of stations to analyse
stations <- gaugedSites[,1]

## Determine the minimal time between extracted peaks
rarea <- ceiling(4 + log(gaugedSites[,'area']))

## Set-up parallel computing
cl <- makeCluster(NCPU)
registerDoParallel(cl)

## Monitoring progress for parallel computing
t0 <- Sys.time()
write(format(t0), file = file.path(CACHE, PROGRESS))

statu <- foreach(ii = seq_along(stations),
								 .packages = c('CSHShydRology', 'tidyverse'),
								 .errorhandling = 'pass') %dopar%{

	site <- stations[ii]

	write(paste0('Analyzing ', site),
				file = file.path(CACHE, PROGRESS), append = TRUE)

	#####################################
	## Extract the daily and annual data
	#####################################

	con <- RSQLite::dbConnect(RSQLite::SQLite(), HYDAT)
	an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, site)
	xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, site)[,2:3]
  RSQLite::dbDisconnect(con)

  ## remove missing
	xd <- as.tibble(xd)
	xd <- na.omit(xd)

	## Remove incomplete years
	## A year is incomplete if it has more than 5% of missing values
	xd <- mutate(xd, year = format(xd$date,'%Y'))

	complete.year <- xd %>%
		group_by(year) %>%
		summarise(n = n()) %>%
		filter(n > 346)

	xd0 <- filter(xd, year %in% complete.year$year)
	nyear.complete <- length(unique(xd0$year))

	## Padding the data
	## We are assuming that missing values are below the threshold
	## This will allow the analysis of seasonal station
	rg.date <- range(xd$date)
	all.date <- tibble(date = seq(rg.date[1],rg.date[2], 'day'))
	xd <- left_join(all.date, xd, by = 'date')
	xd[is.na(xd$value), 'value'] <- 0

	nyear <- nrow(xd)/365.25

	## Clean up table for annual maxima
	an <- as.tibble(an)
	an <- na.omit(an[,1:6])

	an <- an %>%
		filter(peak == 'MAXIMUM') %>%
		mutate(date = as.Date(paste(year, month, day, sep = '/'))) %>%
		rename(station = 'station_number') %>%
		select(station, date, year, value)

  ###########################################
	## Search for thresholds
	###########################################

	## preselect a set of candidate thresholds based on observations
	candidate <- sort(unique(xd$value), decreasing = TRUE)

	## allocate memory
	umat <- matrix(0, length(candidate), 6)
	colnames(umat) <- c('u','PPY','AD','Shape', 'MRL', 'Q10')

	## Compute the p-value of the Anderson Darling test for all candidates
	for(jj in seq_along(candidate)){

		suppressWarnings(fit0 <- try(
			FitPot(value~date, xd, u = candidate[jj], declust = 'wrc', r = rarea[jj]),
		  silent = TRUE))

		## If POT fails
    if(class(fit0) != 'fpot'){
      umat[jj,] <- c(candidate[jj], rep(0,ncol(umat)-1))

    ## If less than 30 peaks
    } else if(fit0$nexcess < 20){
      umat[jj,] <- c(candidate[jj], rep(0,ncol(umat)-1))

    ## Normal condition perform the AD test and compute the flood quantile Q10
    } else{

    	hat0 <- predict(fit0, rt = 10)
      gof0 <- GofTest(fit0)$pvalue

      umat[jj,] <- c(candidate[jj],
      							 fit0$nexcess/fit0$nyear,
      							 gof0,
      							 fit0$estimate[2],
      							 fit0$mrl,
      							 hat0)
    }

		## Terminate loops when PPY is greater than 4
    if(umat[jj, 'PPY'] > 4)
    	break

	}#endfor

	umat <- as.tibble(umat) %>%
		filter(PPY >0 & u > 0)

	## Identify an automatic threshold.
	u25 <- filter(umat, PPY <= 2.5)
	usgn <- filter(u25, AD >= .25)$u
	umax <- filter(u25, AD == max(u25$AD))$u

	u0 <- ifelse(length(usgn) > 0, min(usgn), min(umax))

	## Apply the False discovery rate method
	uid <- order(u25$u)
	uu <- umat$u[uid]
	uad <- umat$AD[uid]
	un <- length(uad)

	ufdr <- sapply(seq_along(uad), function(z) min(p.adjust(uad[z:un],'fdr')) )
  ufdr <- u25 %>%
  	bind_cols(data.frame(FDR = ufdr)) %>%
    filter(AD >= .25, FDR >= .10)

  u1 <- ifelse(length(ufdr) > 0, min(ufdr$u), min(umax))

	##########################################
	## Find thresholds of given PPY
	##########################################

	## Because of the declustering techniques the association between
	## PPY and candidate thresholds (u) is not a strictly monotone
	## We look for a smooth approximation to obtain the threshold associated
	## to a given PPY.

	## PPY of interest: 1, 1.25, ..., 2.5
	ppy.step <- seq(1, 2.5, 0.25)

	## fit a smoothing spline model with 4 equally space knots
	ufit <- try(smooth.spline(umat[,c('PPY','u')], nknots = 4))

	## An error occur when the same threshold lead often to the same peaks
	## The defaut argument tol has to be changed
	if(class(ufit) == 'try-error')
     ufit <- smooth.spline(umat[,c('PPY','u')], nknots = 4, tol = 1e-6)

	uhat <- predict(ufit, ppy.step)$y

	## Identify the closest candidates to the predicted thresholds
	uref <- abs(outer(umat$u, uhat, '-'))
	uref <- apply(uref, 2, which.min)
	uref <- rev(umat$u[uref])

	names(uref) <- paste0('ppy',ppy.step*100)

	#############################################
  ## Seasonal analysis
  #############################################

	## For annual maxima
	if(nrow(an) > 5){
  	ss <- SeasonStat(an$date)
	  names(ss) <- paste0('season_', names(ss))
	}

  ######################################
	## graphics for validating the threshold
	######################################

	## Get the range of value for plot limits
	rg <- apply(umat, 2, range)
	rg.diff <- apply(rg,2,diff) *.1

  fname <- file.path(CACHE, paste0(site,'.png'))
	png(fname,  width = 3*RES_FIG, height = 2*RES_FIG, pointsize = 24)

	palette(c('#000000','#e41a1c','#4daf4a','#377eb8','#984ea3',
					'#ff7f00','#a65628','#f781bf'))

	even.id <- seq(1,length(uref),2)

	par(mfrow = c(2,2))

	## Plot main label
  mlabs <- c('','',
  					 paste0('Station ID = ',site),
  					 paste0('Auto threshold, u = ', round(u0, 1)),
  					 paste0('Year = ', round(nyear,1), ', complete = ',
  					 			 round(nyear.complete,1)),
  					 paste0('Season(m,a) = (', round(ss[3]/pi*6) ,', ',
  					 			 round(ss[4],2), ')')
  					 )

	for(kk in 3:6){
	  plot(umat[,c(1,kk)], pch = 16, type = 'l',
	  		 ylim = rg[,kk] + rg.diff[kk] * c(-1,1),
	  		 main = mlabs[kk],
	  		 xlab = 'Threshold (u)',
	  		 ylab = colnames(umat)[kk], log = 'x')
	  abline(v = u0, col = 2, lwd = 3)
	  abline(v = u1, col = 3, lwd = 3)
	  abline(v = uref, lty = 3, col = 5)

	  text(x = rev(uref)[even.id], y = rg[1,kk] - rg.diff[kk]/2,
	  		 labels = ppy.step[even.id],
	  		 pos = 1, cex = 1.2, col = 5)
	}

	dev.off()

	####################################
	## Output the threshold values
	####################################

	fname <- file.path(CACHE, paste0(site,'.txt'))
	out <- list(paste0('"',site,'"'),
								round(nyear,1),
								round(nyear.complete,1),
								round(ss,3),
								round(u0,1),
								round(uref,1))

	write(paste(unlist(out), collapse = ','), file = fname)

	return("exit normally")

}#endforeach

stopCluster(cl)

write(format(t0-Sys.time()),
			file = file.path(CACHE, PROGRESS), append = TRUE)

print(table(unlist(statu)))


###########################################
## Read the output
###########################################

lst <- list.files(CACHE, '*.txt')

Fz <- function(z) read.csv(file = file.path(CACHE, z), header = FALSE)
xthresh <- do.call(rbind, lapply(lst,Fz))
colnames(xthresh) <- c('station', 'year', 'complete',
											 'season_x', 'season_y', 'season_radius', 'season_angle',
											 'auto', 'ppy250', 'ppy225', 'ppy200',
											 'ppy175', 'ppy150', 'ppy125', 'ppy100')

xthresh <- as.tibble(xthresh) %>%
  mutate(station = as.character(station),
  			 complete = as.numeric(complete))


## verify which stations did not succeeded
fail.id <- which(!gaugedSites$station %in% xthresh$station)
print(fail.id)

gaugedSites <- left_join(gaugedSites, xthresh, by = 'station')
save(gaugedSites, file = 'gauged_sites.rda')

