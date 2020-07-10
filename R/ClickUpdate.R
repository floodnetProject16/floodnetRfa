#' @export
.ClickUpdate <- function(input, db){

	# Extract return periods from string
	period.str <- as.numeric(unlist(strsplit(input$periodString,',')))



	if(input$method == "amax"){
		ans <- .ClickUpdateAmax(input$station, period.str, input$disthresh, db)
	} else if(input$method == "pot"){
		ans <- .ClickUpdatePot(input$station, period.str, db, input$disthresh)
	} else if(input$method == "rfaAmax"){
		ans <- .ClickUpdateRfaAmax(input$station, period.str, input$disthresh, db, input$supReg)
	} else if(input$method == "rfaPot"){
		ans <- .ClickUpdateRfaPot(input$station, period.str, db, input$supReg)
	}

	return(ans)
}

.ClickUpdateAmax <- function(station, period, distr, db)
{

	if(distr == "Default") {
		distr = NULL
	}

	out <- db %>%

		floodnetRfa::AmaxData(station) %>%

		floodnetRfa::FloodnetAmax(period = period, distr = distr, out.model = TRUE)

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

.ClickUpdateRfaAmax <- function(station, period, distr, db, supregSelected)
{

	if(distr == "Default") {
		distr = NULL
	}

	## Filter nonstationary sites from the super region of the target
	## If/else (switch case possible?) needed to access different names of GAUGEDSITES -- is it possible to access GAUGEDSITES$"variable" directly?
	if (supregSelected == "supreg_hc6") {
		target.supreg <- GAUGEDSITES$supreg_hc6[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_hc6 == target.supreg)
	} else if (supregSelected == "supreg_hc12") {
		target.supreg <- GAUGEDSITES$supreg_hc12[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_hc12 == target.supreg)
	} else if (supregSelected == "supreg_km6") {
		target.supreg <- GAUGEDSITES$supreg_km6[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_km6 == target.supreg)
	} else {
		target.supreg <- GAUGEDSITES$supreg_km12[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_km12 == target.supreg)
	}


#	pval.mk <- GAUGEDSITES$trend_mk ## Mann-Kendall
#	pval.pt <- GAUGEDSITES$trend_pt ## Pettitt
#	cond.trend <- pval.mk >= .05 & pval.pt >= .05
	mysites <- GAUGEDSITES[cond.supreg, 'station'] #& cond.trend, 'station']

	out <- db %>%

		floodnetRfa::AmaxData(mysites, target = station, size = 15) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, distr = distr, verbose = FALSE)

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
# 	target.supreg <- GAUGEDSITES$supreg_km12[GAUGEDSITES$station == station][1]
# 	cond.supreg <- with(GAUGEDSITES, supreg_km12 == target.supreg)
#
# 	pval.mk <- GAUGEDSITES$trend_mk ## Mann-Kendall
# 	pval.pt <- GAUGEDSITES$trend_pt ## Pettitt
# 	cond.trend <- pval.mk >= .05 & pval.pt >= .05
# 	mysites <- GAUGEDSITES[cond.supreg & cond.trend, 'station']
#
# 	xd <- AmaxData(mysites, db, target = station, size = 15)
#
# 	out <- FloodnetPool(x = xd, target = station,
# 											period = period, distr = distr, verbose = FALSE)
# 	return(out)
# }

.ClickUpdatePot <- function(station, period, db, thresh){

	out <- db %>%

		floodnetRfa::DailyData(station) %>%

		floodnetRfa::FloodnetPot(area = 184, u = 20, period = period, out.model = TRUE)
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

.ClickUpdateRfaPot <- function(station, period, db, supregSelected){

	## Filter nonstationary sites from the super region of the target
	## If/else (switch case possible?) needed to access different names of GAUGEDSITES -- is it possible to access GAUGEDSITES$"variable" directly?
	if (supregSelected == "supreg_hc6") {
		target.supreg <- GAUGEDSITES$supreg_hc6[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_hc6 == target.supreg)
	} else if (supregSelected == "supreg_hc12") {
		target.supreg <- GAUGEDSITES$supreg_hc12[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_hc12 == target.supreg)
	} else if (supregSelected == "supreg_km6") {
		target.supreg <- GAUGEDSITES$supreg_km6[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_km6 == target.supreg)
	} else {
		target.supreg <- GAUGEDSITES$supreg_km12[GAUGEDSITES$station == station][1]
		cond.supreg <- with(GAUGEDSITES, supreg_km12 == target.supreg)
	}

# pval.mx <- GAUGEDSITES$trend_mx ## Mann-Kendall
# pval.lg <- GAUGEDSITES$trend_lg ## logistic regression
#	cond.trend <- pval.lg >= .05 & pval.mx >= .05

	info <- GAUGEDSITES[cond.supreg, c('station','auto','area')]

	out <- db %>%

		floodnetRfa::DailyPeaksData(info, target = station, size = 25) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, verbose = TRUE, out.model = TRUE)

	return(out)
}

# .ClickUpdateRfaPot <- function(station, period, db){
#
# 	## Filter nonstationary sites from the super region of the target
# 	target.supreg <- GAUGEDSITES$supreg_km12[GAUGEDSITES$station == station][1]
# 	cond.supreg <- with(GAUGEDSITES, supreg_km12 == target.supreg)
# 	pval.mx <- GAUGEDSITES$trend_mx ## Mann-Kendall
# 	pval.lg <- GAUGEDSITES$trend_lg ## logistic regression
# 	cond.trend <- pval.lg >= .05 & pval.mx >= .05
#
# 	info <- GAUGEDSITES[cond.supreg, c('station','auto','area')]
#
# 	xd <- DailyPeaksData(info, db, target = station, size = 25)
#
# 	out <- FloodnetPool(xd, target = station, period = period, verbose = TRUE, out.model = TRUE)
#
# 	return(out)
# }



