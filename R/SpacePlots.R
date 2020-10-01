#' @export
.spacePlots <- function(dbPath, gaugedSites, target, method, supReg){

	# Check if csv or sqlite
	if (substring(dbPath,nchar(dbPath)-2,nchar(dbPath)) == "csv") {
		csv <- read.csv(dbPath)
		db <- floodnetRfa::SequenceData(value = csv$value,  site = csv$site,  date = csv$date)
	} else { #else if not csv, should be sqlite
		db <- dbPath
	}

	# Select the super region
	sid <- gaugedSites[gaugedSites$station == target, supReg] #get target supreg for type and station selected
	region <- as.character(gaugedSites$station[gaugedSites[supReg] == sid]) #get matching stations for supreg

	# Extract all the data in super regions
	amax.region <- AmaxData(db, region)

	# Evaluate the coordinates of the seasonal space
	season.region <- as.data.frame(CSHShydRology::SeasonStat(date~site, amax.region))

	# Evaluate the coordinates of the descriptor space
	desc.region <- gaugedSites[gaugedSites$station %in% region,]

	# Fit the model
	amax.pool <- AmaxData(db, region, target=target)
	fit <- FloodnetPool(amax.pool, target = target)

	# Extract the pooling group
	pool <- rownames(fit$lmom)

	# Coordinates of descriptor space for the pooling group and target
	desc.pool <- gaugedSites[gaugedSites$station %in% pool,]
	desc.target <- gaugedSites[gaugedSites$station == target,]

	# Coordinates Seasonal space for the pooling group and target
	slab <- rownames(season.region)
	season.pool <- season.region[slab %in% pool,]
	season.target <- season.region[slab == target,]

	## Descriptor space graphic
	pltD <- ggplot() +
		geom_point(data = desc.region, aes(x = area, y = map, color = 'grey'),size = .8) +
		geom_point(data = desc.pool, aes(x = area, y = map, color = 'blue')) +
		geom_point(data = desc.target, aes(x = area, y = map, color = 'red'), size = 3) +
		scale_color_identity(guide = 'legend', name = 'Site',
												 labels = c('Pool','Region','Target')) +
		scale_x_continuous(trans = 'log10') + scale_y_continuous(trans = 'log10') +
		xlab('Drainage Area') + ylab("Mean Annual Precipitation") +
		ggtitle("Descriptor Space") +
		theme_light()

	## Seasonal Space graphics
	pltS <- SeasonPlot() +
		geom_point(data = season.region, aes(x = angle, y = radius, color = 'grey'),size = .8) +
		geom_point(data = season.pool, aes(x = angle, y = radius, color = 'blue')) +
		geom_point(data = season.target, aes(x = angle, y = radius, color = 'red'), size = 3) +
		scale_color_identity(guide = 'legend', name = 'Site',
												 labels = c('Pool','Region','Target')) +
		xlab('Drainage Area') + ylab("Mean Annual Precipitation") +
		ggtitle("Descriptor Space") +
		theme_light()
	# if (method == "rfaAmax") {
	# 	# Extract all the data in super regions
	# 	regionData <- AmaxData(db, region)
	# } else {
	# 	#info <- gaugedSites[cond.supreg, c('station', thresh,'area')] #use thresh type selected, or 'auto' if default
	# 	regionData <- floodnetRfa::DailyPeaksData(db,  info, target = station, size = size)
	# }


	# ## Extract data ONLY for the selected sites
	# matchList <- c()
	# stationNames <- c()
	# xd <- data.frame()
	# count = 1
	# for (eachRow in gaugedSites[[1]]){
	# 	if (eachRow %in% siteList){
	# 		matchList <- c(matchList, count)#eachcol[[count]])
	# 		stationNames[[count]] <- eachRow
	# 	}
	# 	count <- count+1
	# }
	#
	# for (eachMatch in matchList) {
	# 	## Extract data
	# 	station <- stationNames[[eachMatch]] #gaugedSites[eachMatch, 'station']) I'm not sure why, but this was not working..
	# 	lon <- gaugedSites[eachMatch, 'lon']
	# 	lat <- gaugedSites[eachMatch, 'lat']
	# 	area <- log(gaugedSites[eachMatch,'area'])
	# 	map <- log(gaugedSites[eachMatch,'map'])
	# 	region <- as.factor(gaugedSites[eachMatch, supReg])
	# 	theta <- gaugedSites[eachMatch,'season_angle']
	# 	r <- gaugedSites[eachMatch,'season_radius']
	#
	# 	row <- cbind.data.frame(station, lon, lat, area, map, region, theta, r)
	# 	xd <- rbind.data.frame(xd, row)
	# }

	# ## Extract data
	# xd <- gaugedSites[, c('station', 'lon', 'lat')]
	# 									#c("station","description","prov","lon","lat","area","map","trend_mk","trend_pt","trend_lg","trend_mx","supreg_hc6","supreg_hc12","supreg_km6","supreg_km12","season_x","season_y","season_angle","season_radius","auto","ppy250","ppy225","ppy200","ppy175","ppy150","ppy125","ppy100")]
	# 								#old c('station', 'lon', 'lat')]
	# xd$area <- log(gaugedSites$area)
	# xd$map <- log(gaugedSites$map)
	# xd$region <- as.factor(gaugedSites$supreg_km12)
	# xd$theta <- gaugedSites$season_angle
	# xd$r <- gaugedSites$season_radius

	# Choose superregion (supReg) based on type from fitted model... unsure how to specify if multiple RFAs in list or no RFA in list

	# ## Function that customize graphics
	# .FormatPlot <- function(plt, main) {
	# 	## Define a custom palette
	# 	mycolors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
	# 							 '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
	#
	# 	plt + scale_colour_manual(values = mycolors) +
	# 		theme(legend.pos = '', plot.title = element_text(hjust = 0.5)) +
	# 		ggtitle(main)
	# }
	#
	# ## Maps
	#  plt <- MapCA(polygon.args = list(fill = 'white', colour = 'grey')) +
	# 	geom_point(data = xd, aes(x = lon, y = lat, colour = region)) +
	# 	geom_point(data = xd[5,], aes(x = lon, y = lat),
	#
	# 						 colour = 'black', size = 5, shape = 10)
	#
	# ## Loading required package: sp
	# ## Regions defined for each Polygons
	# mapsPlt <- .FormatPlot(plt, 'Geograpical space')
	#
	# ## Descriptor space
	# pltD <- ggplot() +
	# 	geom_point(data = xd, aes(x = area, y = map, colour = region)) +
	# 	xlab('Drainage area (log)') + ylab('Mean annual precipitation (log)')
	# descPlt <- .FormatPlot(pltD, 'Descriptor space')
	#
	# ## Seasonal plot
	# pltS <- SeasonPlot() +
	# 	geom_point(data = xd, aes(x = theta, y = r, colour = region))
	# seasonPlt <- .FormatPlot(pltS, 'Seasonal space')



	## Combine plots into values variable
	plots <- list(descriptor = pltD, seasonal = pltS) #coordinates = mapsPlt,
	# plots$coordinates <- mapsPlt
	# plots$descriptor <- descPlt
	# plots$seasonal <- seasonPlt


	return(plots)
}

