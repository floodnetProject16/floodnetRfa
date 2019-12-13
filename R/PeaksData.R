#' @export
#' @rdname DailyPeaksData
PeaksData <- function(x, info){

	## allocate memory
	ans <- vector('list',5)
	names(ans) <- c('peaks', 'sites', 'npeak', 'nyear', 'thresh')

	## Sort data by site
	info <- as.data.frame(info)
	x <- as.data.frame(x)

	colnames(x) <- c('site', 'date', 'value')
	colnames(info) <- c('site', 'thresh', 'nyear')

	x <- x[order(x[,1], x[,2]), ]
	info <- info[order(info[,1]), ]

	## verify the corresponding ID

	sites <- as.character(x[,1])
	ans$sites <- as.character(info[,1])

	if(!all(ans$sites %in% unique(sites)))
		stop('There is missing sites in the hydrometric data.')

	## Format the output
	ans$peaks <- x[sites %in% ans$sites,]
	rownames(ans$peaks) <- NULL

	ans$npeak <- tapply(sites, sites, length)
	names(ans$npeak) <- NULL

	ans$thresh <- info[,2]
	names(ans$thresh) <- NULL

	ans$nyear <- info[,3]
	names(ans$nyear) <- NULL

  return(ans)
}
