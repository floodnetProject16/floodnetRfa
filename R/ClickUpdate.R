#' @export
.ClickUpdate <- function(input, db, gaugedSites, distThresh){

	# Extract return periods from string
	period.str <- as.numeric(unlist(strsplit(input$periodString,',')))

	# Check if csv or sqlite
	if (substring(db,nchar(db)-2,nchar(db)) == "csv") {
		csv <- read.csv(db)
		data <- floodnetRfa::SequenceData(value = csv$value,  site = csv$site,  date = csv$date)
	} else { #else if not csv, should be sqlite
		data <- db
	}


	if(input$method == "amax"){
		ans <- .ClickUpdateAmax(input$station, period.str, distThresh, db, input$confidenceLevel, input$simulations, input$pool)
	} else if(input$method == "pot"){
		ans <- .ClickUpdatePot(input$station, period.str, db, distThresh, input$confidenceLevel, input$simulations, input$pool, gaugedSites)
	} else if(input$method == "rfaAmax"){
		ans <- .ClickUpdateRfaAmax(input$station, period.str, distThresh, db, gaugedSites, input$supReg, input$confidenceLevel, input$simulations,
															 input$heterogeneity, input$pool, input$intersite)
	} else if(input$method == "rfaPot"){
		ans <- .ClickUpdateRfaPot(input$station, period.str, db, distThresh, gaugedSites, input$supReg, input$confidenceLevel, input$simulations,
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


.ClickUpdateRfaAmax <- function(station, period, distr, db, gaugedSites, supregSelected, level, nsim, tol.H, size, corr)
{

	if(distr == "Default") {
		distr = NULL
	}

	## Filter nonstationary sites from the super region of the target
	target.supreg <- gaugedSites[gaugedSites$station == station, supregSelected] #get target supreg for type and station selected
	cond.supreg <- gaugedSites[, supregSelected] == target.supreg

	mysites <- gaugedSites[cond.supreg, 'station'] #& cond.trend, 'station']

	out <- db %>%

		floodnetRfa::AmaxData(mysites, target = station, size = size) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, distr = distr, level = level, nsim = nsim, tol.H = tol.H, corr = corr, verbose = FALSE)

	return(out)
}


.ClickUpdatePot <- function(station, period, db, thresh, level, nsim, size, gaugedSites){

	# Set up thresh
		if (thresh == "Default") {
			thresh <- NULL
		} else {
			thresh <- as.numeric(unlist(strsplit(thresh,',')))
		}

	# Get area for site
	area <- gaugedSites[gaugedSites$station == station, 'area']

	out <- db %>%

		floodnetRfa::DailyData(station, size = size) %>%

		floodnetRfa::FloodnetPot(area = area, period = period, u = thresh, level = level, nsim = nsim, out.model = TRUE)
	#it looks like u is no longer used? trying to use custom input other than 20 gives "Error: non-numeric argument to binary operator", with as.numeric(thresh), it has another "expecting TRUE/FALSE"-like error

	return(out)
}


.ClickUpdateRfaPot <- function(station, period, db, thresh, gaugedSites, supregSelected, level, nsim, tol.H, size, corr){

	## Filter nonstationary sites from the super region of the target
	target.supreg <- gaugedSites[gaugedSites$station == station, supregSelected] #get target supreg for type and station selected
	cond.supreg <- gaugedSites[, supregSelected] == target.supreg

	info <- gaugedSites[cond.supreg, c('station', thresh,'area')] #use thresh type selected, or 'auto' if default

	out <- db %>%

		floodnetRfa::DailyPeaksData(info, target = station, size = size) %>%

		floodnetRfa::FloodnetPool(target = station, period = period, level = level, nsim = nsim, tol.H = tol.H, corr = corr, verbose = TRUE, out.model = TRUE)
	return(out)
}



