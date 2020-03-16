#' @export
.ClickUpdate <- function(input, db){

	# Extract return periods from string
	period.str <- as.numeric(unlist(strsplit(input$periodString,',')))

	if(input$method == "amax"){
		ans <- .ClickUpdateAmax(input$station, period.str, input$distr, db)
	} else if(input$method == "pot"){
		ans <- .ClickUpdatePot(input$station, period.str, db)
	} else if(input$method == "rfaAmax"){
		ans <- .ClickUpdateRfaAmax(input$station, period.str, input$distr, db)
	} else if(input$method == "rfaPot"){
		ans <- .ClickUpdateRfaPot(input$station, period.str, db)
	}

	return(ans)
}

.ClickUpdateAmax <- function(station, period, distr, db)#, file, period)
{

	if(distr == "Default") {
		distr = NULL
	}

	xd <- AmaxData(station, db)
	out <- FloodnetAmax(xd, period = period,
											distr = distr, out.model = TRUE)



	return(out)
}

.ClickUpdateRfaAmax <- function(station, period, distr, db)#, file, period)
{

	if(distr == "Default") {
		distr = NULL
	}

	## Filter nonstationary sites from the super region of the target
	target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
	cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)

	pval.mk <- gaugedSites$trend_mk ## Mann-Kendall
	pval.pt <- gaugedSites$trend_pt ## Pettitt
	cond.trend <- pval.mk >= .05 & pval.pt >= .05
	mysites <- gaugedSites[cond.supreg & cond.trend, 'station']

	xd <- AmaxData(mysites, db, target = station, size = 15)

	out <- FloodnetPool(x = xd, target = station,
											period = period, distr = distr, verbose = FALSE)
	return(out)
}

.ClickUpdatePot <- function(station, period, db){

	xd <- DailyData(station , db)
	out <- FloodnetPot(xd, area = 184, u = 20, period = period)

	return(out)
}

.UpdateRfaPot <- function(station, period, db){

	## Filter nonstationary sites from the super region of the target
	target.supreg <- gaugedSites$supreg_km12[gaugedSites$station == station][1]
	cond.supreg <- with(gaugedSites, supreg_km12 == target.supreg)
	pval.mx <- gaugedSites$trend_mx ## Mann-Kendall
	pval.lg <- gaugedSites$trend_lg ## logistic regression
	cond.trend <- pval.lg >= .05 & pval.mx >= .05

	info <- gaugedSites[cond.supreg, c('station','auto','area')]

	xd <- DailyPeaksData(info, db, target = station, size = 25)

	out <- FloodnetPool(xd, target = station, period = period, verbose = TRUE, out.model = TRUE)

	return(out)
}



