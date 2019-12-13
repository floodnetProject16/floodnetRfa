context('Testing FloodnetPot')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetProject16'))

test_that('Verifying FloodnetRoi', {

  ## Path the HYDAT database
  db <- DB_HYDAT

	## Extract catchment descriptors
	xd <- with(descriptors[1:50,],
  data.frame(
  	site = station,
    area = log(area),
    map  = log(map_ws),
    wb   = log(.01 + wb),
    stream = log(.01 + stream),
  	elev = elev_ws,
  	slope = log(.01 + slope)))

	coord <- descriptors[1:100, c('lon','lat')]

	## Put the target site apart
	ref <- '01AF009'
	target.id <- (xd$site == ref)

  target <- xd[target.id,]
  target.coord <- coord[target.id,]

   xd <- xd[-target.id,]
  coord <- coord[-target.id,]

  ## Fit the model
  set.seed(39)
  out <- FloodnetRoi(target = target, sites = xd, db = db, period = 10,
  									 size = 30, nsim = 30, verbose = FALSE,
						out.model = TRUE)

  expect_equal(class(out), "list")
  expect_equal(names(out), c('fit','qua'))

  out <- out$qua
  expect_equal(unique(out$period), 10)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'QRT')

  sname <- c('quantile','se','lower','upper')
  expect_equal(as.character(unique(out$variable)), sname)

  qua <- with(out, value[variable == 'quantile'])
  se <- with(out, value[variable == 'se'])
  lb <- with(out, value[variable == 'lower'])
  ub <- with(out, value[variable == 'upper'])

  expect_true(all(qua >= lb))
  expect_true(all(qua <= ub))
  expect_true(all(se > 0))

  ## one return period
  set.seed(39)
  out <- FloodnetRoi(target = target, sites = xd, db = db, period = 10,
  									 size = seq(20,40,5), nsim = 0,
  									 verbose = FALSE, out.model = TRUE)

})
