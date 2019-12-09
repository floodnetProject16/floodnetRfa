#' @import CSHShydRology
#' @export
#' @rdname DailyPeaksData
ExtractPeaksData <- function(x, info, pad = FALSE, tol = 346, sorted = FALSE){

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
	x <- stats::na.omit(x)

	if(pad)
		x <- PadPot(x,  z = 0, tol = tol)

	## split the dataset by site
  xd <- split(x, as.character(x[,1]))

  ## Compute the minimal time difference between peaks
	rsep <- ceiling(4 + log(info[,3]))

	## allocate memory
	nyear <- npeak <- vector('numeric', length(xd))

	for(ii in 1:length(xd)){

  	## Extract peaks
		uii <- info[ii,2]
    pid <- which.floodPeaks(value ~ date, xd[[ii]],	u = uii, r = rsep[ii])
    xd[[ii]] <- xd[[ii]][pid,]

    ## Remove threshold value
    xd[[ii]][,3] <- xd[[ii]][,3] - uii

    ## Save properties
    yy <- format(xd[[ii]]$date,'%Y')
  	nyear[ii] <- length(unique(yy))
    npeak[ii] <- length(pid)
  }

  peaks <- do.call(rbind, xd)
  rownames(peaks) <- NULL

  ans <- list(peaks = peaks,
  						sites = names(xd),
  						npeak = npeak,
  						nyear = nyear,
  						thresh = info[,2])

  class(ans) <- 'peaksdata'

	return(ans)
}



