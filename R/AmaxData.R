#' Extract Annual, daily data from HYDAT
#'
#' Return a dataset of hydrometric data from HYDAT.
#' Optionally, a vector distance can be passed and so only a pooling groups is returned.
#' the target is the site with distance zero.
#'
#' @param sites List of stations.
#'
#' @param db HYDAT database.
#'
#' @param year Logical. Should the full date be return or only the year.
#'
#' @param distance A vector of distance between all sites.
#'
#' @param size Size of the pooling group.
#'
#' @param pad,tol Logical and number of days. Should the daily data be padded.
#'   See \link[CSHShydRology]{PadPot}.
#'
#' @seealso \link{DailyPeaksData}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## This create a variable DB_HYDAT that point to the database
#' db <- DB_HYDAT
#'
#' ## Reading AMAX data for one station
#' x <- AmaxData(c('01AD002'), db)
#' head(x, 3)
#'
#' ## Reading multiple stations
#' x <- AmaxData(c('01AD002','01AF009'), db, year = FALSE)
#' x[seq(85,95),]
#'
#' ## Reading Daily data
#' x <- DailyData(c('01AD002','01AF009'), db)
#' head(x, 3)
#'
#' ## Filter the stations to keep only a pooling group of size 5
#' sid <- gaugedSites$supreg_km12 == 11
#' sites <- gaugedSites$station[sid]
#'
#' coord <- gaugedSites[sid, c('lon','lat')]
#' rownames(coord) <- sites
#'
#' h <- as.matrix(dist(coord))
#'
#' x <- AmaxData(sites, db, target = '01AF009', size = 5,  distance = h)
#' table(x$station)
#' round(sort(h['01AF009',])[1:10],2)
#' }
#'
AmaxData <- function(sites, db, target = NULL, distance = NULL, size = 25){

  ## Verify that there is an unique target if distances are passed

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
