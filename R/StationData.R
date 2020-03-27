#' Station information
#'
#' Return the coordinates and drainage area found in the HYDAT database
#'  for a group of stations.
#'
#' @param db HYDAT database.
#'
#' @param sites List of stations.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' StationData(DB_HYDAT, c('01AD002','01AF009'))
#'
#' }
#'
StationData <- function(db, sites){

	con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  xd <- HYDAT::StationMetadata(con, as.character(sites))
  RSQLite::dbDisconnect(con)

  xd <- xd[,c(1,7,6,8)]
  colnames(xd) <- c('site','lon','lat','area')

  return(xd)
}
