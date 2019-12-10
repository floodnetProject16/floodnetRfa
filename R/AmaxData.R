#' Extract Annual, daily data from HYDAT
#'
#' Return a dataset of hydrometric data from HYDAT.
#' Optionally, a target site can be passed and so only its pooling groups is returned.
#'
#' @param sites List of stations.
#'
#' @param db HYDAT database.
#'
#' @param year Logical. Should the full date be return or only the year.
#'
#' @param target A target site of the pooling group.
#'
#' @param size Size of the pooling group.
#'
#' @param distance A vector of distance between all sites.
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
#' ## This create a variable DB_HYDAT that point to database
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
#' head(x)
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
AmaxData <- function(sites, db, year = TRUE,
                     target = NULL, size = 25, distance = NULL){

  ###################################
  ## Extract annual data
  ###################################

  if(!is.null(target))
    if(!(target %in% sites))
      stop('Target must be in the selected sites.')

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, as.character(sites))
  RSQLite::dbDisconnect(con)

  an <- an[an$peak == 'MAXIMUM',]
  an$date <- as.Date(with(an, paste(year,month,day, sep = '/')), optional = TRUE)
  an <- an[, c('station_number','year','value','date')]
  colnames(an) <- c('station','year','value','date')
  rownames(an) <- NULL

  if(year){
    ans <- an[,-4]
  } else {
    ans <- an[,c(1,4,3)]
  }

  ans <- stats::na.omit(ans)

  if(is.null(target)){
    return(ans)
  }

  ###################################
  ## Find the pooling groups
  ###################################

  if(is.null(distance)){
    stop('A matrix or vector of distance must be provided.')
  } else if(!is.vector(distance)){
    distance <- as.matrix(distance)[target, ]
  }

  size <- pmin(length(distance), size)
  pool <- sites[sort(order(distance)[1:size])]

  return(ans[ans$station %in% pool,])
}
