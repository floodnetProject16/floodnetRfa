#' @export
#' @rdname AmaxData
StationData <- function(sites, db){

	con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  xd <- HYDAT::StationMetadata(con, as.character(sites))
  RSQLite::dbDisconnect(con)

  xd <- xd[,c(1,7,6,8)]
  colnames(xd) <- c('site','lon','lat','area')

  return(xd)
}
