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
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Path the HYDAT database
#' db <- "extdata/Hydat.sqlite3"
#'
#' x <- AmaxData(c('01AD002'), db)
#' head(x, 3)
#'
#' x <- AmaxData(c('01AD002','01AF009'), db, year = FALSE)
#' x[seq(85,95),]
#'
#' x <- DailyData(c('01AD002','01AF009'), db)
#' head(x)
#'
#' ## Only the pooling group of the target is returned
#' sites <- with(gaugedSites, station[supreg_km12 == 11])
#' x <- DailyData(sites, db, target = '01AF009', size = 5)
#' table(x$station)
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
