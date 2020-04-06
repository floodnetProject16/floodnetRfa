context('Testing examples')

## This is a config file that once loaded create a variable DB_HYDAT that point
#to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

test_that('Example AmaxData', {

   ## Reading AMAX data for one station.
   x <- AmaxData(DB_HYDAT, c('01AD002'))
   head(x, 3)

   ## Reading multiple stations.
   x <- AmaxData(DB_HYDAT, c('01AD002', '01AF009'))
   x[seq(85,95),]

   ## Reading Daily data.
   x <- DailyData(DB_HYDAT, c('01AD002','01AF009'))
   head(x, 3)

   ## A pooling group of size 5 based on seasonality distance.
   x <- AmaxData(DB_HYDAT, GAUGEDSITES$station, target = '01AF009', size = 5)

   ## Extracted site.
   sort(unique(x$site))

   ## Pooling group with different a distance.
   meta <- log(GAUGEDSITES[, c('area','map')])
   h <- as.matrix(dist(scale(meta)))
   x <- AmaxData(DB_HYDAT, GAUGEDSITES$station, distance = h[2,], size = 5)

   ## Extracted site.
   sort(unique(x$site))

  expect_true(TRUE)

})

test_that('Example DailyPeaksData', {

  ## Data.frame containing thresholds and drainage area
  info <- GAUGEDSITES[1:2, c('station','ppy200','area')]

  ## Reading for one station
  x <- DailyPeaksData(DB_HYDAT, info)
  head(x)

  ## Manually extracting the data
  DB_HYDAT %>%
  	DailyData(info$station) %>%
    ExtractPeaksData(info) %>%
    head()

  ## Create a dataset of exceedances manually
  xd <- SequenceData(3, site = unique(info$station))
  PeaksMeta(xd) <- info

  expect_true(TRUE)

})
