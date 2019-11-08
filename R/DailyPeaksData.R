#' Extract Peaks from HYDAT database
#'
#' Return a list of extracted independent thresholds.
#'
#' @param db Path to the HYDAT database.
#' @param info Site information. Must be a data frame with 3
#'   columns: site, area, threshold.
#' @param tol Number of days considered as a complete year.
#'
#' @export
#'
#' @examples
#'
#' print('hello')
#'
#'
DailyPeaksData <- function(db, info, tol = 346){

	info <- as.data.frame(info)

	if(ncol(info) == 2){

		## Case drainage area is not provided
	  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
	  area <- HYDAT::StationMetadata(con, as.character(info[,1]))
	  area <- area$drainage_area_gross
	  RSQLite::dbDisconnect(con)

	  info <- cbind(info, area = area)

	} else
		info <- info[,1:3]

  ## Read HYDAT. Note xd is sorted
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
	xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, info[,1])[,1:3]
	RSQLite::dbDisconnect(con)

	# If missing drainage area
	sid <- which(!is.finite(info[,3]))
	for(ii in sid){
    isite <- as.character(info[ii,1])
    xbar <- mean(xd[xd[,1] == isite,3], na.rm = TRUE)
    info[ii, 3] <- exp(4.0934 + 0.9944 * log(xbar))
	}

	## Make sure that info is sorted by site.
	info <- info[order(as.character(info[,1])), ]

	## Extract the peaks
	ans <- ExtractPeaks(xd, info, tol, sorted = TRUE)

	return(ans)
}

#' @export
ExtractPeaks <- function(x, info, tol, sorted = FALSE){

  ## Order by site
	if(!sorted){
	  info <- info[order(as.character(info[,1])), ]
	  x <- as.data.frame(x[order(x[,1], x[,2]), ])
	}

	## verify that all site in x are in info
	all.site <- as.character(unique(x[,1]))
	if(!all(all.site %in% info[,1]))
     stop('Sites are missing from the info dataset.')

	## remove missing
	x <- na.omit(x)

	## split the dataset by site
  xd <- split(x, as.character(x[,1]))

  ## Compute the minimal time difference between peaks
	rsep <- ceiling(4 + log(info[,3]))

	## allocate memory
	nyear <- vector('numeric', length(xd))
	names(nyear) <- names(xd)

	for(ii in 1:length(xd)){

  	## Remove incomplete years
  	yy <- format(xd[[ii]]$date,'%Y')
  	yy.nb <- tapply(yy, yy, length)
  	yy.complete <- names(yy.nb[yy.nb >= tol])
  	yid <- yy %in% yy.complete

  	nyear[ii] <- length(yy.complete)

  	xd[[ii]] <- xd[[ii]][yid,]

  	## Extract peaks
    pid <- which.floodPeaks(value ~ date, xd[[ii]],
    												u = info[ii,2], r = rsep[ii])
    xd[[ii]] <- xd[[ii]][pid,]
  }

  xd <- do.call(rbind, xd)
  rownames(xd) <- NULL


	return(list(peaks = xd, nyear = nyear))
}
