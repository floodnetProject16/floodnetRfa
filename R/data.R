################################################################################
#' Meta-information about HYDAT gauged stations.
#'
#' The dataset contains meta-information about 1114 stations in HYDAT that have
#' at least 20 years of observations and a natural flood regime.
#' It includes catchment descriptors, candidates thresholds for peaks over
#' threshold (POT) analysis, p-values of trend tests and super regions.
#' All that information was collected during the first phase of the Floodnet
#' project.
#'
#' @details
#'
#' The dataset contains basic information about the stations are available.
#' It includes the  idenfication number (\code{station}), province (\code{prov})
#' and geographical coordinates (\code{lon},\code{lat}), drainage area
#' (\code{area}) and mean annual precipitation (\code{map}).
#'
#' The dataset classifies the stations into super regions based on catchment
#' descriptors.
#' The Ward's Method (\code{hc}) and the k-means (\code{km}) are combined with
#' the Euclidean distance to create super regions of size 6 and 12.
#' For example, the results of the classification into 12 super regions using
#' the k-means method is found in column \code{supreg_km12}.
#'
#' The dataset provides seasonality measures based on the annual maximum events.
#' It includes, the regularity (\code{season_radius}) [0,1] and timing (in rad)
#' (\code{season_angle}) of the annual floods.
#' These values represent polar coordinates inside the unitary circle and are
#' converted into a cartesian system of coordinates (\code{season_x, season_y}).
#'
#' The dataset includes candidates thresholds for POT analysis.
#' The candidates in column \code{auto} are the first thresholds that have a
#' p-value greater or equal to 0.25 and an exceedance rate lower than 2.5 peaks
#' per year (PPY).
#' The other suggested thresholds are associated with given PPY.
#' For example, \code{ppy175} is associated with 1.75 PPY.
#'
#' The dataset contains p-values of trend tests to identify the stations with
#' time-dependent flood risk.
#' For AMAX, the p-value of the Mann-Kendall (\code{trend_mk}) and the Pettitt's
#'  test (\code{trend_pt}) are provided.
#' For POT, the Mann-Kendall's test is applied to the exceedances
#' (\code{trend_mx}). Logistic regression is used to test for trends in the
#' exceedance rate. The F-test is evaluated for polynomial trends of order 1 to
#' 3 and the minimum p-value is returned (\code{trend_lg}).
#'
#' @references
#'
#' Mostofi Zadeh, S., & Burn, D. H. (2019). A Super Region Approach to
#' Improve Pooled Flood Frequency Analysis. Canadian Water Resources Journal
#' / Revue Canadienne Des Ressources Hydriques, 0(0), 1–14.
#' https://doi.org/10.1080/07011784.2018.1548946
#'
#' Durocher, M., Zadeh, S. M., Burn, D. H., & Ashkar, F. (2018). Comparison of
#' automatic procedures for selecting flood peaks over threshold based on
#' goodness-of-fit tests. Hydrological Processes, 0(0).
#' https://doi.org/10.1002/hyp.13223
#'
################################################################################
"gaugedSites"


################################################################################
#' Catchment descriptors
#'
#' List of meteorological and physical characteristics of 770  gauged stations.
#'
#' @return
#'
#' The dataset includes the following variables:
#' \itemize{
#'
#'   \item Station identification number: \code{station}.
#'
#'   \item Station geographical coordinates: \code{lon, lat}.
#'
#'   \item Watershed centers: \code{lon_ws, lat_ws}.
#'
#'   \item Drainage area (m3/s): \code{area}.
#'
#'   \item Perimeter (km): \code{peri}.
#'
#'   \item Station elevation (m): \code{elev}.
#'
#'   \item Watershed average elevation (m): \code{elev_ws}.
#'
#'   \item Mean aspect (Degree): \code{aspect}.
#'
#'   \item Mean slope (\%): (\code{slope}).
#'
#'   \item Stream density (\eqn{km[-1]}): \code{stream}.
#'
#'   \item Percentage of water bodies(\%): \code{wb}.
#'
#'   \item Station and watershed mean annual precipitation (mm): \code{map, map_ws}.
#'
#'   \item Station and watershed annual average air temperature: \code{temp,temp_ws}.
#'
#' }
#'
#' @references
#'
#' Durocher, M., Burn, D. H., & Mostofi Zadeh, S. (2018). A nationwide regional
#' flood frequency analysis at ungauged sites using ROI/GLS with copulas and
#' super regions. Journal of Hydrology, 567, 191-202.
#' https://doi.org/10.1016/j.jhydrol.2018.10.011
#'
################################################################################
"descriptors"

################################################################################
#' Map of Canada
#'
#' To produce a quick and simple map of Canada.
#' Original data were from https://www.naturalearthdata.com/.
#'
################################################################################
"map_ca"

