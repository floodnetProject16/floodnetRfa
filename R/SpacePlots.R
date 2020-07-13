#' @export
.spacePlots <- function(gaugedSites){ #, supReg){

	## Extract data
	xd <- gaugedSites[, c('station', 'lon', 'lat')]
										#c("station","description","prov","lon","lat","area","map","trend_mk","trend_pt","trend_lg","trend_mx","supreg_hc6","supreg_hc12","supreg_km6","supreg_km12","season_x","season_y","season_angle","season_radius","auto","ppy250","ppy225","ppy200","ppy175","ppy150","ppy125","ppy100")]
									#old c('station', 'lon', 'lat')]
	xd$area <- log(gaugedSites$area)
	xd$map <- log(gaugedSites$map)
	xd$region <- as.factor(gaugedSites$supreg_km12)
	xd$theta <- gaugedSites$season_angle
	xd$r <- gaugedSites$season_radius

	# Choose superregion (supReg) based on type from fitted model... unsure how to specify if multiple RFAs in list or no RFA in list

	## Function that customize graphics
	.FormatPlot <- function(plt, main) {
		## Define a custom palette
		mycolors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
								 '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

		plt + scale_colour_manual(values = mycolors) +
			theme(legend.pos = '', plot.title = element_text(hjust = 0.5)) +
			ggtitle(main)
	}

	## Maps
	 plt <- MapCA(polygon.args = list(fill = 'white', colour = 'grey')) +
		geom_point(data = xd, aes(x = lon, y = lat, colour = region)) +
		geom_point(data = xd[5,], aes(x = lon, y = lat),

							 colour = 'black', size = 5, shape = 10)

	## Loading required package: sp
	## Regions defined for each Polygons
	mapsPlt <- .FormatPlot(plt, 'Geograpical space')

	## Descriptor space
	pltD <- ggplot() +
		geom_point(data = xd, aes(x = area, y = map, colour = region)) +
		xlab('Drainage area (log)') + ylab('Mean annual precipitation (log)')
	descPlt <- .FormatPlot(pltD, 'Descriptor space')

	## Seasonal plot
	pltS <- SeasonPlot() +
		geom_point(data = xd, aes(x = theta, y = r, colour = region))
	seasonPlt <- .FormatPlot(pltS, 'Seasonal space')



	## Combine plots into values variable
	plots <- list(coordinates = mapsPlt, descriptor = descPlt, seasonal = seasonPlt)
	# plots$coordinates <- mapsPlt
	# plots$descriptor <- descPlt
	# plots$seasonal <- seasonPlt


	return(plots)
}

