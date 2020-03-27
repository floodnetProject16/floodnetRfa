#' Extract independent Peaks
#'
#' The function `ExtractPeaksData` returned independent peaks for one or more
#' sites and the function `DailyPeaksData` applies it directly on daily data
#' extracted from HYDAT.
#' The function `PeaksMeta` serves to link meta-information about the threshold
#' and exceedance rate to the extracted peaks.
#'
#' @param db Path to the HYDAT database.
#'
#' @param x Hydrometric Data. Must have 3 columns: site, date and value.
#'
#' @param info Site information. Must have 3 columns:
#'   site, threshold and drainage area.
#'
#' @param meta,value Meta information about thresholds.
#'   Must be a data frame with 3
#'   columns: site, threshold and exceedance rates (peaks per year).
#'
#' @param pad Logical. Should the time series be padded.
#'   See \link[CSHShydRology]{PadPot}.
#'
#' @param tol Number of days to consider a year as complete.
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
#'
#' @seealso \link{DailyData}, link{FloodnetPool}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  ## Data.frame containing thresholds and drainage area
#'  info <- gaugedSites[1:2, c('station','ppy200','area')]
#'
#'  ## Reading for one station
#'  x <- DailyPeaksData(DB_HYDAT, info)
#'  head(x)
#'
#'  ## Manually extracting the data
#'  DB_HYDAT %>%
#'   DailyData(info$station) %>%
#'    ExtractPeaksData(info) %>%
#'    head()
#'
#'  ## Create a dataset of exceedances manually
#'  xd <- SequenceData(3, site = unique(info$station))
#'  PeaksMeta(xd) <- info
#'
#' }
#'

DailyPeaksData <- function(db, info, target = NULL, size = 25, distance = NULL,
													 pad = FALSE, tol = 346){

	info <- as.data.frame(info)

	if(ncol(info) != 3)
		stop('Input info must have 3 columns: site, thresh, area')

	colnames(info) <- c('site', 'thresh', 'area')

	sites <- info[,1]

  ###################################
  ## Find the pooling groups
  ###################################

  if(!is.null(target)){

    if(!(target %in% sites))
      stop('Target must be in the selected sites.')

    distance <- SeasonDistanceData(sites, db = db, target = target)

    size <- pmin(length(distance), size)
    sites <- sites[sort(order(distance)[1:size])]

  } else if(!is.null(distance)){

    if(sum(distance <= 0) > 1)
      stop('There must be a unique target with distance zero')

    size <- pmin(length(distance), size)
    sites <- sites[sort(order(distance)[1:size])]
  }

	## filter and sort
	info <- info[info$site %in% sites, ]
	info <- info[order(as.character(sites)), ]

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

	return(ans)
}
