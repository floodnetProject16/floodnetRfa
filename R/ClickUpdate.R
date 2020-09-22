#' @export
.ClickUpdate <- function(input, db, gaugedSites, distThresh, threshType){

	# Extract return periods from string
	period.str <- as.numeric(unlist(strsplit(input$periodString,',')))

	# Setup thresh depending on option chosen
	if (threshType == "pot") {
		if (distThresh == "Default") {
			thresh <- NULL
		} else {
			thresh <- as.numeric(unlist(strsplit(distThresh,',')))
		}
	} else if (threshType == "rfaPot") {
		if (distThresh == "Default") {
			thresh <- NULL
		} else {
			thresh <- distThresh
		}
	}


	if(input$method == "amax"){
		ans <- .ClickUpdateAmax(input$station, period.str, distThresh, db, input$confidenceLevel, input$simulations, input$pool)
	} else if(input$method == "pot"){
		ans <- .ClickUpdatePot(input$station, period.str, db, thresh, input$confidenceLevel, input$simulations, input$pool)
	} else if(input$method == "rfaAmax"){
		ans <- .ClickUpdateRfaAmax(input$station, period.str, distThresh, db, gaugedSites, input$supReg, input$confidenceLevel, input$simulations,
															 input$heterogeneity, input$pool, input$intersite)
	} else if(input$method == "rfaPot"){
		ans <- .ClickUpdateRfaPot(input$station, period.str, db, thresh, gaugedSites, input$supReg, input$confidenceLevel, input$simulations,
															input$heterogeneity, input$pool, input$intersite)
	}

	return(ans)
}

.ClickUpdateAmax <- function(station, period, distr, db, level, nsim, size)
{

	if(distr == "Default") {
		distr = NULL
	}

	out <- db %>%

		floodnetRfa::AmaxData(station, size = size) %>%

		floodnetRfa::FloodnetAmax(period = period, distr = distr, level = level, nsim = nsim, out.model = TRUE)

	return(out)
}

# .ClickUpdateAmax <- function(station, period, distr, db)
# {
#
# 	if(distr == "Default") {
# 		distr = NULL
# 	}
#
# 	xd <- floodnetRfa::AmaxData(db, station) # Due to change in AmaxData, needed to flip order of db/station
# 	out <- floodnetRfa::FloodnetAmax(xd, period = period,
# 																	 distr = distr, out.model = TRUE)
#
#
#
# 	return(out)
# }

.ClickUpdateRfaAmax <- function(station, period, distr, db, gaugedSites, supregSelected, level, nsim, tol.H, size, corr)
{

	if(distr == "Default") {
		distr = NULL
	}

	## Filter nonstationary sites from the super region of the target
	## If/else (switch case possible?) needed to access different names of gaugedSites -- is it possible to access gaugedSites$"variable" directly?
	if (supregSelected == "supreg_hc6") {
		target.supreg <- gaugedSites$supreg_hc6[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_hc6 == target.supreg)
	} else if (supregSelected == "supreg_hc12") {
		target.supreg <- gaugedSites$supreg_hc12[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_hc12 == target.supreg)
	} else if (supregSelected == "supreg_km6") {
		target.supreg <- gaugedSites$supreg_km6[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_km6 == target.supreg)
	} else {
		target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)
	}


#	pval.mk <- gaugedSites$trend_mk ## Mann-Kendall
#	pval.pt <- gaugedSites$trend_pt ## Pettitt
#	cond.trend <- pval.mk >= .05 & pval.pt >= .05
	mysites <- gaugedSites[cond.supreg, 'station'] #& cond.trend, 'station']

	out <- db %>%

		floodnetRfa::AmaxData(mysites, target = station, size = size) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, distr = distr, level = level, nsim = nsim, tol.H = tol.H, corr = corr, verbose = FALSE)

	return(out)
}

# .ClickUpdateRfaAmax <- function(station, period, distr, db)#, file, period)
# {
#
# 	if(distr == "Default") {
# 		distr = NULL
# 	}
#
# 	## Filter nonstationary sites from the super region of the target
# 	target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
# 	cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)
#
# 	pval.mk <- gaugedSites$trend_mk ## Mann-Kendall
# 	pval.pt <- gaugedSites$trend_pt ## Pettitt
# 	cond.trend <- pval.mk >= .05 & pval.pt >= .05
# 	mysites <- gaugedSites[cond.supreg & cond.trend, 'station']
#
# 	xd <- AmaxData(mysites, db, target = station, size = 15)
#
# 	out <- FloodnetPool(x = xd, target = station,
# 											period = period, distr = distr, verbose = FALSE)
# 	return(out)
# }

.ClickUpdatePot <- function(station, period, db, thresh, level, nsim, size){

	out <- db %>%

		floodnetRfa::DailyData(station, size = size) %>%

		floodnetRfa::FloodnetPot(area = 184, period = period, u = thresh, level = level, nsim = nsim, out.model = TRUE)
	#it looks like u is no longer used? trying to use custom input other than 20 gives "Error: non-numeric argument to binary operator", with as.numeric(thresh), it has another "expecting TRUE/FALSE"-like error

	return(out)
}

# .ClickUpdatePot <- function(station, period, db){
#
# 	xd <- DailyData(db, station)
# 	out <- FloodnetPot(xd, area = 184, u = 20, period = period)
#
# 	return(out)
# }

.ClickUpdateRfaPot <- function(station, period, db, thresh, gaugedSites, supregSelected, level, nsim, tol.H, size, corr){

	## Filter nonstationary sites from the super region of the target
	## If/else (switch case possible?) needed to access different names of gaugedSites -- is it possible to access gaugedSites$"variable" directly?
	if (supregSelected == "supreg_hc6") {
		target.supreg <- gaugedSites$supreg_hc6[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_hc6 == target.supreg)
	} else if (supregSelected == "supreg_hc12") {
		target.supreg <- gaugedSites$supreg_hc12[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_hc12 == target.supreg)
	} else if (supregSelected == "supreg_km6") {
		target.supreg <- gaugedSites$supreg_km6[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_km6 == target.supreg)
	} else {
		target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
		cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)
	}

# pval.mx <- gaugedSites$trend_mx ## Mann-Kendall
# pval.lg <- gaugedSites$trend_lg ## logistic regression
#	cond.trend <- pval.lg >= .05 & pval.mx >= .05

	info <- gaugedSites[cond.supreg, c('station','auto','area')]

	out <- db %>%

		floodnetRfa::DailyPeaksData(info, target = station, size = size) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, level = level, nsim = nsim, tol.H = tol.H, corr = corr, verbose = TRUE, out.model = TRUE)
	#thresh = thresh,
				#Thresh not actually a parameter... looks like it's set to automatically be taken from dataset
	return(out)
}

# .ClickUpdateRfaPot <- function(station, period, db){
#
# 	## Filter nonstationary sites from the super region of the target
# 	target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
# 	cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)
# 	pval.mx <- gaugedSites$trend_mx ## Mann-Kendall
# 	pval.lg <- gaugedSites$trend_lg ## logistic regression
# 	cond.trend <- pval.lg >= .05 & pval.mx >= .05
#
# 	info <- gaugedSites[cond.supreg, c('station','auto','area')]
#
# 	xd <- DailyPeaksData(info, db, target = station, size = 25)
#
# 	out <- FloodnetPool(xd, target = station, period = period, verbose = TRUE, out.model = TRUE)
#
# 	return(out)
# }



