#' Intersite correlation matrix
#'
#' Return the intersite correlation matrix list of station based on annual
#' maximum discharge extracted from HYDAT.
#'
#' @param db HYDAT database
#' @param x List of stations
#'
#' @details
#'
#' The intersite correlations represent the correlation coefficients of an underlying
#' multivariate Gaussian distribution (Normal Copula).
#' First the spearman correlation is evaluated for each pair of sites with a
#' distance less than 500km.
#' Next the weighted least-squared method is used to evaluate the
#' parameter of an exponential model.
#' Finally, the fitted coefficient of each sites is returned.
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
#' ##Intersite correlation for a given super region
#' IntersiteData(DB_HYDAT, GetSuperRegion('01AF009'))
#' }
IntersiteCorData <- function(db, x){

	x <- as.character(x)
	## Extract Annual Maximum
	an <- AmaxData(db,x)
  an$year <- as.integer(format(an$date,'%Y'))
  anw <- CSHShydRology::DataWide(value~site+year , an)

  ## Evaluate distance
  stn <- StationData(db,x)
  h <- CSHShydRology::GeoDist(stn[,c('lon','lat')])

  ## Fit an intersite correlation model
  suppressWarnings(
  	icor <- CSHShydRology::Intersite(anw, type = 'exp', distance = h,
  																 distance.max = 500))

  ans <- icor$model
  rownames(ans) <- colnames(ans) <- x

  return(ans)

}
