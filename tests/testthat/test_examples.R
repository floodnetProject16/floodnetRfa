
context('Exemple tests')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetProject16'))

test_that('Example AmaxData', {

  ## Path the HYDAT database
  db <- DB_HYDAT

  ## Reading AMAX data for one station
  x <- AmaxData(c('01AD002'), db)
  head(x, 3)

  ## Reading multiple stations
  x <- AmaxData(c('01AD002','01AF009'), db, year = FALSE)
  x[seq(85,95),]

  ## Reading Daily data
  x <- DailyData(c('01AD002','01AF009'), db)
  head(x)

  ## Filter the stations to keep only a pooling group of size 5
  sid <- gaugedSites$supreg_km12 == 11
  sites <- gaugedSites$station[sid]

  coord <- gaugedSites[sid, c('lon','lat')]
  rownames(coord) <- sites

  h <- as.matrix(dist(coord))

  x <- AmaxData(sites, db, target = '01AF009', size = 5,  distance = h)
  table(x$station)
  round(sort(h['01AF009',])[1:10],2)

  expect_true(TRUE)

})

test_that('Example DailyPeaksData', {

	## Path the HYDAT database
  db <- DB_HYDAT

  ## Data.frame containing the threshold and drainage area
  info <- gaugedSites[1:2, c('station','ppy200','area')]

  ## Reading AMAX data for one station
  x <- DailyPeaksData(info, db)
  head(x, 3)

  expect_true(TRUE)

})

test_that('Example FloodnetAmax', {

	## Path the HYDAT database
  db <- DB_HYDAT

  ## Read Amax data
  x <- AmaxData(c('01AD002'), db)

  ## Performing the analysis
  FloodnetAmax(site = '01AD002', db = db, period = c(20,50),
  						 nsim = 30, verbose = FALSE)

  expect_true(TRUE)

})

test_that('Example FloodnetPot', {

	## Path the HYDAT database
  db <- DB_HYDAT

  ## Read Amax data
  x <- DailyData('01AD002', db)

  ## Performing the analysis
  FloodnetPot('01AD002', db = db, period = c(20,50), u = 1000,
  						 area = 14400, nsim = 30, verbose = FALSE)

  expect_true(TRUE)

})

test_that('Example FloodnetPool', {

	## Path the HYDAT database
  db <- DB_HYDAT

  ## Compute distance
  coord <- gaugedSites[,c('lon','lat')]
  rownames(coord) <- gaugedSites$station

  ## Read Amax data
  x <- AmaxData(rownames(coord), db, target = '01AF009',
  							size = 15, distance = dist(coord))

  ## Performing the analysis using L-moments
  FloodnetPool(x, '01AF009', distr = 'gev', period = c(20,50),
  						 nsim = 30, verbose = FALSE)

  FloodnetPoolMle(x, '01AF009', distr = 'gev', type = 'cv',
  								period = c(20,50), nsim = 30, verbose = FALSE)

  ## Read Pot data
  info <- gaugedSites[, c('station','auto','area')]
  xd <- DailyPeaksData(info, db, target = '01AF009',
  							size = 15, distance = dist(coord))

  ## Performing the analysis with POT data
  FloodnetPool(xd, '01AF009', period = c(20,50), nsim = 30, verbose = FALSE)

  FloodnetPoolMle(xd, '01AF009', type = 'shape',
  								period = c(20,50), nsim = 30, verbose = FALSE)


  expect_true(TRUE)

})
