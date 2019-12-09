#' @import stats
#' @export
#' @rdname AmaxData
DailyData <- function(sites, db,
                      pad = FALSE, tol = 346,
                      target = NULL, size = 25, distance = NULL){

  ###################################
  ## Find the pooling groups
  ###################################

  if(!is.null(target)){

    if(!(target %in% sites))
      stop('Target must be in the selected sites.')

    if(is.null(distance)){
      distance <- SeasonDistanceData(sites, db)[target, ]

    } else if(!is.vector(distance)){
      distance <- as.matrix(distance)[target, ]
    }

    size <- pmin(length(distance), size)
    sites <- sites[sort(order(distance)[1:size])]

  }

  ###################################
  ## Exract Daily data
  ###################################

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  xd <- HYDAT::DailyHydrometricData(con, get_flow = TRUE, as.character(sites))
  RSQLite::dbDisconnect(con)

  xd <- xd[,1:3]
  colnames(xd) <- c('station','date','value')

  xd <- stats::na.omit(xd)

  if(pad)
    xd <- CSHShydRology::PadPot(xd, tol = tol)

  return(xd)

}
