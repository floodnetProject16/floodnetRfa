#' Seasonal distance between stations
#'
#' Return the distance matrix between stations based on the
#' annual flood peaks regularity and timing.
#'
#' @param sites Stations
#' @param db HYDAT database
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
SeasonDistanceData <- function(sites, db, ...){

  an <- AmaxData(sites, db, year = FALSE)
  season <- CSHShydRology::SeasonStat(date ~ station, an)
  season.dist <- CSHShydRology::DistSeason(radius~angle,season, ...)
  colnames(season.dist) <- rownames(season.dist) <- rownames(season)

  return(season.dist)
}
