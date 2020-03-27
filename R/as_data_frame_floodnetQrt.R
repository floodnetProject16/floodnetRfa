#' @export
#' @rdname floodnetMdl
as.data.frame.floodnetRoi <-
	function(x,
					 row.names = NULL,
					 optional = FALSE,
					 type = 'q',
					 ...){

	if(type == 'cv'){
		lab <- expand.grid(period = x$period, size = x$cv$size,
											 variable = c('nash','skill'))

		ans <- data.frame(method = 'qrt', lab,
											value = unlist(x$cv[,-1]))
		rownames(ans) <- row.names

	} else if(type == 'q'){

		if(ncol(x$quantile) == 1){
		  ans <- data.frame(site = x$site, method = 'qrt',
		  									size = x$size, period = x$period,
		  									variable = 'quantile', value = x$quantile$quantile)

	  } else {
		  lab <- expand.grid(site = x$site, method = x$method,
											 size = x$size, period = x$period,
											 variable = c('quantile','se','lower','upper'))
	  	ans <- data.frame(lab, value = as.numeric(as.matrix(x$quantile)))

		  ans <- ans[order(ans$site),]
		  rownames(ans) <- row.names
	  }
	}

	if(optional)
		rownames(ans) <- make.names(rownames(ans))

	return(ans)
}

