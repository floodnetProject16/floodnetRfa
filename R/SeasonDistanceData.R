#' Similarity measure based on seasonality of annual maxima
#'
#' Return the distance matrix between stations based on the
#' seasonality of the annual maximum events.
#' Optionally, a target can be passed to extract only the
#' pooling group from a list of stations.
#'
#' @param x Hydrometric data.
#'
#' @param db Path of the HYDAT database,
#'
#' @param sites List of sites in HYDAT.
#'
#' @param form Formula of the form date~site, that point to the columns with
#'   date and site ID.
#'
#' @param target Sites for which distance is returned.
#'
#' @param ... Other parameters passed to \link[CSHShydRology]{DistSeason}.
#'
#' @seealso \link{AmaxData}.
#'
#' @export
#'
#' @examples
#'
#' ## Extract similarity measure from multiple site
#' ## with respect to target station 01AF009
#' h <- SeasonDistance(DemoData('region'))
#' print(h[1:3,1:3])
#' print(h[2,])
#'
#' \dontrun{
#'
#' ## Path to HYDAT
#' # DB_HYDAT = ".../hydat.sqlite3"
#'
#' ## Compute the distance using HYDAT directly
#' SeasonDistanceData(DB_HYDAT, names(h), target = '01AF009')
#'
#' }
#'
SeasonDistance <- function(x, form = date ~ site, target = NULL, ...){

  x <- as.data.frame(x)
  x <- stats::na.omit(x)

  season <- CSHShydRology::SeasonStat(form, x)
  season.dist <- CSHShydRology::DistSeason(radius~angle,season, ...)
  colnames(season.dist) <- rownames(season.dist) <- rownames(season)

  if(!is.null(target))
    season.dist <- season.dist[target,]

  return(season.dist)
}

#' @export
#' @rdname SeasonDistance
SeasonDistanceData <- function(db, sites, ...){
  x <- AmaxData(db, sites)
  return(SeasonDistance(x, ...))
}
