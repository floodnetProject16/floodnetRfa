#' @export
#' @rdname floodnetMdl
CompareModels <- function(...){
	ans <- list(...)

	clst <- sapply(ans, class)

	if(any(clst != 'floodnetMdl'))
		stop("The inputs must be the outputs of the floodnet functions.")

	class(ans) <- 'floodnetMdls'
	return(ans)
}
