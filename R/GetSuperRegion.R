###############################################################################
#' Super regions
#'
#' Return the super region associated to a target site as suggested by the
#' information provided in the dataset \code{gaugedSites}.
#'
#' @param site Station identification number.
#'
#' @param type Type of frequency analysis: Annual maximum (\code{'amax'}) or
#'   peaks over threshold (\code{'pot'}).
#'
#' @param cluster Clustering thechniques. It corresponds to a column of
#'   \code{gaugedSites}. Must be one of \code{'km6'}, \code{'km12'},
#'   \code{'hc6'} or \code{'hc12'}.
#'
#' @param trend.tol Significance level of the trend tests. This used to remove
#'   stations that may not have constant flood risk over time.
#'
#' @param thresh Threshold to return. It corresponds to a column of
#'   \code{gaugedSites}. For example, \code{'auto'} or \code{'ppy200'}.
#'
#' @seealso \link{gaugedSites}
#'
#' @export
#'
#' @examples
#'
#' GetSuperRegion('01AF009')
#
#' GetSuperRegion('01AD002', type = 'pot', thresh = 'ppy200')
#'
GetSuperRegion <-
	function(site,
					 type = 'amax',
					 cluster = 'km12',
					 trend.tol = 0.05,
					 thresh = 'auto'){

	## Find sites in the same super region
  xd <- get("gaugedSites")
	sreg <- xd[,paste0('supreg_',cluster)]
	target.supreg <- sreg[xd$station == site]
	cond.supreg <- sreg == target.supreg


	if(type == 'amax'){
		## Identify the stationary sites
		cond.trend <- xd$trend_mk >= trend.tol &
									xd$trend_pt >= trend.tol

		## Return the intersection
		ans <- as.character(xd[cond.supreg & cond.trend, 'station'])

	} else if (type == 'pot'){
		## Identify the stationary sites
		cond.trend <- xd$trend_mx >= trend.tol &
									xd$trend_lg >= trend.tol

		## Return the intersection
		ans <- xd[cond.supreg & cond.trend, c('station', thresh, 'area')]
	}

	return(ans)
}
