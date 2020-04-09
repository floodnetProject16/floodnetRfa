#' Intersite correlation matrix for annual maxima
#'
#' Return the intersite correlation matrix for a list of stations based on
#' their annual maximum discharges.
#'
#' @param db HYDAT database
#' @param x List of stations
#' @param type Method used to evaluate the correlation coefficients. One of
#'   \code{'emp'}, \code{'exp'}, \code{'raw'} or \code{'avg'}. See
#'   \link[CSHShydRology]{Intersite} for more details.
#'
#' @param ... Other parameter passed to \link[CSHShydRology]{Intersite}.
#'
#' @references
#'
#' Durocher, M., Burn, D. H., & Mostofi Zadeh, S. (2018). A nationwide regional
#'   flood frequency analysis at ungauged sites using ROI/GLS with copulas and
#'   super regions. Journal of Hydrology, 567, 191–202.
#'   https://doi.org/10.1016/j.jhydrol.2018.10.011
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## The variable DB_HYDAT contains the path of the HYDAT database.
#' DB_HYDAT <- '.../Hydat.sqlite3'
#'
#' ## Meta-information about stations
#' gaugedSites <- read.csv('.../gauged_sites.csv')
#'
#' sreg <- which(gaugedSites$supreg_km12 == 10)
#' sites <- as.character(gaugedSites$station)[sreg]
#'
#' ## Intersite correlation for a given super region
#' x <- IntersiteCorData(DB_HYDAT, sites, smooth = .6, distance.max = 200)
#'
#' #' ## Intersite correlation for a given super region
#' x <- IntersiteCorData(DB_HYDAT, sites, type = 'emp' )
#'
#' }
#'
IntersiteCorData <- function(db, x, type = 'exp', ...){

	x <- as.character(x)

	## Extract Annual Maximum
	an <- AmaxData(db,x)
  an$year <- as.integer(format(an$date,'%Y'))
  anw <- CSHShydRology::DataWide(value~site+year , an)

  ## Evaluate distance
  if( type == 'exp'){
	  stn <- StationData(db,x)
  	h <- CSHShydRology::GeoDist(stn[,c('lon','lat')])

  	## Fit an intersite correlation model
  	suppressWarnings(
  		ans <- CSHShydRology::Intersite(anw, type = 'exp',
  				 distance = h, ...)$model)

  } else if(type == 'emp'){
		suppressWarnings(
  		ans <- CSHShydRology::Intersite(anw, type = 'emp', ...)$model)

  } else if(type %in% c('raw', 'avg')){
		suppressWarnings(
  		ans <- CSHShydRology::Intersite(anw, type = 'emp',
  																		 defpos = FALSE)$corr)

  	 if(type == 'avg'){
  	 	 ans <- ans[lower.tri(ans)]
  	 	 ans <- mean(ans, na.rm = TRUE)
  	 }

  }

  return(ans)

}
