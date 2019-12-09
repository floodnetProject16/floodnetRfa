#' @export
#' @rdname DailyPeaksData
BindPeaksdata <- function(...){

	arg <- list(...)

	ans <- vector('list',5)
	names(ans) <- c('peaks','sites','npeak','nyear','thresh')

	ans$peaks <- do.call(rbind, lapply(arg,getElement, 'peaks'))
	rownames(ans$peaks) <- NULL

	for(vname in names(ans)[-1]){
	  ans[[vname]] <- unlist(lapply(arg, getElement,vname))
	  names(ans[[vname]]) <- NULL
	}

	## Sorting

	ans$peaks <- ans$peak[order(ans$peak[,1], ans$peak[,1]), ]
	sid <- order(ans$sites)

	for(vname in names(ans)[-1]){
	  ans[[vname]] <- ans[[vname]][sid]
	}

	return(ans)

}
