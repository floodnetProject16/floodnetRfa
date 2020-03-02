#' Extract Peaks from HYDAT database
#'
#' The function `DailyPeaksData` return an object containing exccedances above
#' given thresholds extracted from HYDAT.
#' The function `ExtractPeaksData` extract the same object from a data.frame.
#' The function `PeaksData` serves to create the same output object from known
#' exceedances and `BindPeaksData` can merged together 2 of these objectes.
#'
#' @param info Site information. Must be a data frame with 3
#'   columns: site, threshold and area, except for function \code{PeaksData} where
#'   area must be the total number of years.
#'
#' @param db Path to the HYDAT database.
#'
#' @param x Hydrometric Data. Must have 3 columns:
#'   station, threshold and drainage area.
#'
#' @param pad Logical. Should the time series be padded.
#'   See \link[CSHShydRology]{PadPot}.
#'
#' @param tol Number of days considered as a complete year.
#'
#' @param target Target station of the pooling group.
#'
#' @param size Size of the pooling group.
#'
#' @param distance Distance between stations. If not provided, the distance
#'   between the regularity and timing of the annual flood peaks is used.
#'
#' @param sorted Logical. Is the data sorted.
#'
#' @param ... Other parameters
#'
#' @details
#'
#' For \code{DailyPeaksData}, if \code{info} has two columns, they are assumed
#' to be the station and threshold.
#' The drainage area is then extracted from the HYDAT database.
#'
#' The utility function \code{PeaksData} can be used to construct the same output
#' from already extracted peaks.
#'
#' @seealso \link{DailyData}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  ## Path the HYDAT database
#'  db <- DB_HYDAT
#'
#'  ## Data.frame containing the threshold and drainage area
#'  info <- gaugedSites[1:2, c('station','ppy200','area')]
#'
#'  ## Reading AMAX data for one station
#'  x <- DailyPeaksData(info, db, pad = TRUE)
#'  head(x$peaks, 3)
#'
#'  ## Manually extracting the data
#'  xd <- DailyData(info$station,db)
#'  x2 <- ExtractPeaksData(xd, info, pad = TRUE)
#'  head(x2$peaks, 3)
#'
#'  ## Create and merged Peaks
#'  p1 <- PeaksData(x2$peaks, info)
#'  p2 <- BindPeaksdata(p1, p1)
#'
#' }
#'
DailyPeaksData <- function(info, db, pad = FALSE, tol = 346,
													 target = NULL, size = 25, distance = NULL){

	info <- as.data.frame(info)

  ###################################
  ## Find the pooling groups
  ###################################

	if(!is.null(target)){

		sites <- as.character(info[,1])

    if(!(target %in% sites))
      stop('Target must be in the selected sites.')

    distance <- SeasonDistanceData(sites, db = db, target = target)

    size <- pmin(length(distance), size)
    pool <- sites[sort(order(distance)[1:size])]

  } else if(!is.null(distance)){

    if(sum(distance <= 0) > 1)
      stop('There must be a unique target with distance zero')

    size <- pmin(length(distance), size)
    pool <- sites[sort(order(distance)[1:size])]
  }

	info <- info[info[,1] %in% pool,]

	## Make sure that info is sorted by site.
	info <- info[order(as.character(info[,1])), ]

	###################################
  ## Find the pooling groups
  ###################################

  ## Read HYDAT. Note xd is sorted
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
	xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, info[,1])[,1:3]
	RSQLite::dbDisconnect(con)

	colnames(xd) <- c('site','date','value')

	## Extract the peaks
	ans <- ExtractPeaksData(xd, info, pad, tol, sorted = TRUE)

	attr(ans, 'dtype') <- 'peaksdata'

	return(ans)
}
