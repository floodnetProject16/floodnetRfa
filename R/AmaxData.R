#' Extract annual or daily hydrometric data from HYDAT
#'
#' Return a dataset of hydrometric data from a local HYDAT database.
#' Optionally, a target or vector of distances can be passed to extract only the
#' pooling group from a list of stations.
#' If a distance is passed, the target has a distance of zero.
#' If a target is passed, the similarity measure based on the seasonality of the
#' annual maxima are used as the distance.
#'
#' @param db Path of the HYDAT database.
#'
#' @param sites List of stations from which to extract the data.
#'
#' @param distance A vector of distance between all sites. The target has distance
#'   equal to zero.
#'
#' @param target Target site.
#'
#' @param size Size of the pooling group.
#'
#' @param pad,tol Logical and number of days. Should the daily data be padded.
#'   See \link[CSHShydRology]{PadPot}.
#'
#' @seealso \link{DailyPeaksData}, \link{SeasonDistance}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## The variable DB_HYDAT containing the path to the HYDAT database.
#' #DB_HYDAT <- '.../Hydat.sqlite3'
#'
#' ## Reading AMAX data for one station.
#' x <- AmaxData(DB_HYDAT, c('01AD002'))
#' head(x, 3)
#'
#' ## Reading multiple stations.
#' x <- AmaxData(DB_HYDAT, c('01AD002', '01AF009'))
#' x[seq(85,95),]
#'
#' ## Reading Daily data.
#' x <- DailyData(DB_HYDAT, c('01AD002','01AF009'))
#' head(x, 3)
#'
#' ## A pooling group of size 5 based on seasonality distance.
#' x <- AmaxData(DB_HYDAT, gaugedSites$station, target = '01AF009', size = 5)
#'
#' ## Extracted site.
#' sort(unique(x$site))
#'
#' ## Pooling group with different a distance.
#' meta <- log(gaugedSites[, c('area','map')])
#' h <- as.matrix(dist(scale(meta)))
#' x <- AmaxData(DB_HYDAT, gaugedSites$station, distance = h[2,], size = 5)
#'
#' ## Extracted site.
#' sort(unique(x$site))
#'
#' }
#'
AmaxData <- function(
    db,
    sites,
    target = NULL,
    distance = NULL,
    size = 25){

  ###################################
  ## Extract annual data
  ###################################

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, as.character(sites))
  RSQLite::dbDisconnect(con)

  an <- an[an$peak == 'MAXIMUM',]
  an$date <- as.Date(with(an, paste(year,month,day, sep = '/')), optional = TRUE)
  an <- an[, c('station_number','date','value')]
  rownames(an) <- NULL
  colnames(an) <- c('site','date','value')

  an <- stats::na.omit(an)

  ###################################
  ## Find the pooling groups
  ###################################

  if(!is.null(distance)){

    ## Verify that there is a unique target
    if(sum(distance <= 0) > 1)
      stop('There must be a unique target with distance zero')

    size <- pmin(length(distance), size)
    pool <- sites[sort(order(distance)[1:size])]

  } else if(!is.null(target) ){

    if(!(target %in% sites))
      stop('The target must be in selected sites.')

    season <- CSHShydRology::SeasonStat(date ~ site, an)
    season.dist <- CSHShydRology::DistSeason(radius~angle,season)
    distance <- season.dist[which(target == sites), ]

    size <- pmin(length(distance), size)
    pool <- sites[sort(order(distance)[1:size])]

  } else {
    pool <- unique(an$site)
  }

  an <- an[an$site %in% pool,]

  return(an)
}
