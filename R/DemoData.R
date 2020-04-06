#' Demonstration data
#'
#' Return hydrometric data available in the package \code{CSHShydRology} but in the format
#' required by the modeling functions in \code{floodnetRfa}.
#' Different datasets can be requested:
#' the annual maxima from station 01AF007 (\code{'amax'}),
#' the daily streamflow (\code{'daily'}) or exceedances (\code{'peaks'})
#' from station 01AD002 and the annual maxima from 38 stations in the Atlantic
#' provinces (\code{'region'}).
#'
#' @param type Type of hydrometric data to return.
#'
#' @seealso \link[CSHShydRology]{flowStJohn}, \link[CSHShydRology]{flowAtlantic}.
#'
#' @export
#'
#' @examples
#'
#' x <- DemoData()
#' head(x)
#'
#' x <- DemoData(type = 'daily')
#' head(x)
#'
#' x <- DemoData(type = 'peaks')
#' head(x)
#' PeaksMeta(x)
#'
#' x <- DemoData(type = 'region')
#' head(x)
#'
#' x <- DemoData(type = 'descriptor')
#' head(x)
#'
DemoData <- function(type = 'amax'){

	## AMAX data, single site
	if(type == 'amax'){
	  ans <- CSHShydRology::flowAtlantic$ams
		ans <- ans[ans$id == '01AF007',]
	 	colnames(ans) <- c('site','date','value')

	## Daily data, single site
	} else if(type == 'daily') {
		ans <- CSHShydRology::flowStJohn[93:32234,]
		ans <- data.frame(site = '01AD002', data = ans[,1], value = ans[,2])

  ## Exceedances, single site
	} else if(type == 'peaks'){
		## 158 pre selected peaks
		id <- c(206,415,562,586,604,949,1313,1667,2033,2181,2409,2773,3136,3462,
						3504,3521,3865,3887,4222,4247,4606,4966,5013,5318,5696,5741,6069,
						6251,6429,6760,6787,6807,7151,7170,7526,7893,8235,8252,8606,8840,
						8960,8979,9344,9359,9681,9708,9729,10068,10109,10134,10179,10210,
						10444,10821,11164,11407,11530,11545,11647,11898,12274,12647,13004,
						13364,13555,13726,14451,14478,14827,15175,15543,15564,15751,15921,
						16289,16405,16666,17014,17038,17381,17757,18085,18102,18124,18186,
						18214,18469,18851,19172,19205,19237,19557,19913,20035,20085,20118,
						20300,20655,21034,21065,21110,21395,21736,21755,21787,22099,22470,
						22837,22866,23222,23402,23570,23585,23948,24302,24672,24687,25042,
						25410,25437,25633,25783,25798,26117,26134,26503,26857,26871,26888,
						27237,27595,27674,27966,28155,28328,28443,28698,28873,28919,29054,
						29242,29428,29798,29895,30142,30158,30505,30684,30877,30899,30924,
						31023,31221,31255,31615,31651,31977,31992)

		ans <- CSHShydRology::flowStJohn[id,]
		ans <- data.frame(site = '01AD002', data = ans[,1], value = ans[,2])
		PeaksMeta(ans) <- data.frame(site = '01AD002', thresh = 1000, ppy = 1.79)

	## AMAX, 38 sites
	} else if(type == 'region'){

		## pre selected station
		stn <- c("01AF007", "01AF009", "01AJ010", "01AK001", "01AK007", "01AN002",
	 				 "01AP002", "01AP004", "01AQ001", "01BC001", "01BD008", "01BE001",
	 				 "01BG005", "01BH005", "01BH010", "01BJ003", "01BJ007", "01BL002",
	 				 "01BL003", "01BO001", "01BP001", "01BQ001", "01BS001", "01BU002",
	 				 "01BV006", "01CC005", "01DG003", "01DL001", "01DR001", "01EC001",
	 				 "01ED005", "01ED007", "01EF001", "01EJ001", "01EO001", "01FA001",
	 				 "01FB001", "01FB003")

	  ans <- CSHShydRology::flowAtlantic$ams
	  ans <- ans[ans$id %in% stn,]
	  colnames(ans) <- c('site','date','value')

	} else if (type == 'descriptor'){

	  ans <- CSHShydRology::flowAtlantic$info

	} else {
	  stop('Wrong input')
	}

	return(ans)
}
