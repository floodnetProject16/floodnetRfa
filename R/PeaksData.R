#' @export
#' @rdname DailyPeaksData
PeaksData <- function(x, meta){

	## Case to remove the meta data
	if(is.null(meta)){
		attr(x, 'meta') <- NULL
		return(x)
	}

	## Sort data by site
	meta <- as.data.frame(meta)
	x <- as.data.frame(x)

	colnames(x) <- c('site', 'date', 'value')
	colnames(meta) <- c('site', 'thresh', 'ppy')

	x <- x[order(x[,1], x[,2]), ]
	meta <- meta[order(meta[,1]), ]

	## verify the corresponding ID
	sites <- as.character(x[,1])
	asites <- as.character(meta[,1])

	if(!all(asites %in% unique(sites)))
		stop('There is missing sites in the hydrometric data.')

	## Format the output
	ans <- x[sites %in% asites,]
	rownames(ans) <- NULL

	rownames(meta) <- meta$site
	attr(ans, 'meta') <- meta[,-1]
	attr(ans, 'dtype') <- 'peaksdata'

  return(ans)
}

#' @export
#' @rdname DailyPeaksData
`PeaksMeta<-` <- function(x,value) PeaksData(x,value)

#' @export
#' @rdname DailyPeaksData
PeaksMeta <- function(x) attr(x, 'meta')

