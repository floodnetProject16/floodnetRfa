#' @export
#' @rdname AmaxData
DailyData <- function(db, sites,
                      pad = FALSE, tol = 346,
                      target = NULL, distance = NULL, size = 25){

  ###################################
  ## Find the pooling groups
  ###################################

  if(!is.null(target)){

    if(!(target %in% sites))
      stop('Target must be in the selected sites.')

    distance <- SeasonDistanceData(sites, db = db, target = target)

    size <- pmin(length(distance), size)
    sites <- sites[sort(order(distance)[1:size])]

  } else if(!is.null(distance)){

    if(sum(distance <= 0) > 1)
      stop('There must be a unique target with distance zero')

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
  colnames(xd) <- c('site','date','value')

  xd <- stats::na.omit(xd)

  if(pad)
    xd <- CSHShydRology::PadPot(xd, tol = tol)

  return(xd)

}
