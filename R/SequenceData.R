#' @export
#' @rdname AmaxData
SequenceData <- function(x, freq = 'years', site.name = 'site1', sdate = '1970-01-01'){

  sdate <- as.Date(sdate)
  dt <- seq(sdate, by = freq, length.out = length(x))
  ans <- data.frame(site = site.name, date = dt, value = x)
	return(ans)

}


