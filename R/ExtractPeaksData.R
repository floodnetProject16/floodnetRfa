#' @import CSHShydRology
#' @export
#' @rdname DailyPeaksData
ExtractPeaksData <-
	function(x,
					 info,
					 pad = FALSE,
					 tol = 346,
					 sorted = FALSE){

  ## Order by site
	if(!sorted){
	  info <- info[order(as.character(info[,1])), ]
	  x <- as.data.frame(x[order(x[,1], x[,2]), ])
	}

	## verify that all site in x are in info
	all.site <- as.character(unique(x[,1]))
	if(!all(all.site %in% info[,1]))
     stop('Sites are missing from the info dataset.')

	## Verify that site in info are unique
	if(anyDuplicated(info[,1]) > 0)
		stop('Sites in info are not unique')

	## remove missing
	x <- stats::na.omit(x)

	if(pad)
		x <- PadPot(x,  z = 0, tol = tol)

	## split the dataset by site
  xd <- split(x, as.character(x[,1]))

  ## Compute the minimal time difference between peaks
	rsep <- ceiling(4 + log(info[,3]))

	## allocate memory
	ppy <- vector('numeric', length(xd))

	for(ii in 1:length(xd)){

  	## Extract peaks
		uii <- info[ii,2]
		nobs <- nrow(xd[[ii]])
    pid <- which.floodPeaks(value ~ date, xd[[ii]],	u = uii, r = rsep[ii])
    xd[[ii]] <- xd[[ii]][pid,]

    ## Remove threshold value
    xd[[ii]][,3] <- xd[[ii]][,3] - uii

    ## Save properties
    ppy[ii] <- nrow(xd[[ii]]) / nobs * 365.25
  }

  ans <- do.call(rbind, xd)
  rownames(ans) <- NULL

	PeaksMeta(ans) <- data.frame(site = info[,1],
															 thresh = info[,2],
															 ppy = ppy)

	return(ans)
}

