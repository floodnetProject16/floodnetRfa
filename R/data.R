################################################################################
#' Information about 1114 gauged stations in Canada
#'
#' List of stations in HYDAT that has have at least 20 years of observations.
#' The datasets include descriptors, candidates thresholds for POT, p-values
#' of trend tests, super regions of hydrologically similar stations.
#'
#' Apart the station idenfication number (\code{station})
#' provinces (\code{prov}) and geographical coordinates (\code{lon},\code{lat}),
#' the dataset includes two other site characteristics : drainage area (\code{area}),
#' mean annual precipitation (\code{map}).
#'
#' The super regions are created using clustering techniques: Ward's Method
#' (\code{hc}) and k-means (\code{km}).
#' The methods are applied on a subset of 4 standardized variables: \code{lon}, \code{lat},
#' \code{area} and \code{map}.
#' Euclidean distance is used to to create groups of 6 and 12 super regions.
#' For example, the column \code{supreg_km12} have classified the stations in
#' 12 super regions using the k-means method.
#'
#' Seasonality measures based on the annual maximum events are included in the
#' dataset.
#' The regularity (\code{season_radius}) and timing  (\code{season_angle}) of
#' the annual floods in radian are provided directly and in cartesian coordinates
#' (\code{season_x, season_y}).
#'
#' The datasets includes candidates thresholds to help performing POT.
#' The candidates \code{auto} is the first observations that has a p-value
#' greater or equal to 0.25 and more than 2.5 peaks per year.
#' The other suggested threshold are associated with a number of peaks per year (PPY).
#' For example, \code{ppy175} is associated with 1.75 PPY.
#'
#' The p-values of trend tests are includes to help identify stationary stations.
#' For AMAX, the Mann-Kendall (\code{trend_mk}) and the Pettitt's test
#' (\code{trend_pt}) are provided.
#' For the mean excess of the POT, the Mann-Kendall test is included and for the
#' frequency of the peaks the significance of the slope of a logistic regression
#' model is reported (\code{trend_lg}).
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
#' Station characteristics
#'
#' List of meteorological and physical characteristics for 770  gauged stations.
#'
#' The dataset includes the following variables:
#' Station number (\code{station}),
#' Station geographical coordinates (\code{lon,lat}),
#' Coordinates of the center of the watershed (\code{lon_ws,lat_ws}),
#' Drainage area (\code{area}) and perimeter(\code{peri}) of the watershed,
#' Station elevation (\code{elev}) and average elevation of the watershed
#' (\code{elev_ws}),
#' Mean watershed aspect in Degree (\code{aspect}),
#' Mean slope in percent (\code{slope}),
#' Stream density (\code{stream}),
#' Percentage of water bodies (\code{wb}),
#' Mean annual precipitation for the station and watershed (\code{map,map_ws}) and
#' Average annual air temperature for the station and watershed (\code{temp,temp_ws}).
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
#' Original data form https://www.naturalearthdata.com/
#'
################################################################################
"map_ca"
