#' Seasonal distance between stations
#'
#' Return the distance matrix between stations based on the
#' annual flood peaks regularity and timing.
#'
#' @param sites Stations
#'
#' @param db HYDAT database
#'
#' @param x Alternative input. Must be a data.frame of annual maximums
#'    with 3 columns: site, time, value. The time variable must be of the class 'Date'.
#'
#' @param ... Other parameter passed to \link[CSHShydRology]{DistSeason}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' db <- 'extdata/Hydat.sqlite3'
#'
#' mysites <- c("01AD002", "01AD003", "01AE001", "01AF007", "01AF009")
#'
#' SeasonDistanceData(mysites, db)
#' }
#'
SeasonDistanceData <- function(sites, db, x = NULL, target = NULL, ...){

  if(!is.null(x)){
    an <- as.data.frame(x)[,1:2]
    colnames(an) <- c('site','date')

  } else{
    an <- AmaxData(sites, db)
  }

  season <- CSHShydRology::SeasonStat(date ~ site, an)
  season.dist <- CSHShydRology::DistSeason(radius~angle,season, ...)
  colnames(season.dist) <- rownames(season.dist) <- rownames(season)

  if(!is.null(target))
    season.dist <- season.dist[target,]

  return(season.dist)
}
