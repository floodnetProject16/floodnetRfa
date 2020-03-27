#' Prepare hydrometric data from outside sources
#'
#' Prepare a dataset having three columns (site, date, value) from vectors of
#' sequential data. If only an integer is passed as input, a sample of the same
#' size is simulated from a Gumbel distribution.
#' The data has a mean equal to 100 and a standard deviation
#' equal to 30.
#'
#' @param value Hydrometric data.
#'
#' @param date Date.
#'
#' @param site Site identification number.
#'
#' @param freq Frequency of the input data.
#'
#' @param sdate Starting date.
#'
#' @seealso \link{AmaxData}, \link{DemoData}.
#'
#' @examples
#'
#' ## Example of a yearly sequence, starting today.
#' mydata <- rnorm(3)
#' SequenceData(mydata, site = 'mysite')
#'
#'
#' ## Example of daily sequence starting in 2000-01-01.
#' SequenceData(mydata, freq = 'days', sdate = '2000-01-01')
#'
#'
#' ## Example of an irregular sequence.
#' mydate <- as.Date(c('2000-01-01','2005-05-05','2010-10-10'))
#' SequenceData(mydata, mydate)
#'
#' ## Create a testing set
#' SequenceData(3, site = c('s1','s2'))
#'
#' @export
SequenceData <- function(
	value = 50,
	date = NULL,
	site = 'site1',
	freq = 'years',
	sdate = Sys.Date()){

	## Create a test data
	if(length(value) == 1){

		## Simulate from a Gumbel distribution
		n <- value
		value <- replicate(length(site),
									 -23.3909 * log(-log(runif(as.integer(n)))) + 86.4984)

		value <- as.numeric(value)
		sdate <- as.Date(sdate)
		y <- expand.grid(seq(sdate, by = freq, length.out = n), site)

		date <- y[,1]
		site <- y[,2]

	}

	## Verify the dimension of site
	if(length(site) > 1)
		if(length(site) != length(value))
			stop("The argument site is not of the same length as value")

	## Create a sequence of Dates if not provided
	if(is.null(date)){
	  sdate <- as.Date(sdate)
  	date <- seq(sdate, by = freq, length.out = length(value))

  } else {
    date <- as.Date(date)
  }

	return(data.frame(site = site, date = date, value = value))

}


