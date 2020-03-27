context('Testing FloodnetRoi')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

test_that('Verifying FloodnetRoi', {

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
	target.id <- which(as.character(xd$site) == ref)

  target <- xd[target.id,]
  target.coord <- coord[target.id,]

  xd <- xd[-target.id,]
  coord <- coord[-target.id,]

  ## Fit the model
  set.seed(39)
  an <- AmaxData(DB_HYDAT,xd$site)
  out <- FloodnetRoi(an, target = target, sites = xd, period = 10,
  									 size = 20, nsim = 30, verbose = FALSE,
						out.model = TRUE)

  expect_equal(class(out), "floodnetRoi")
  sname <- c("site","method","period", "quantile","size","data","fit")
  expect_equal(names(out), sname)

  expect_equal(out$period, 10)

  expect_equal(out$site, ref)

  expect_equal(out$method, 'qrt')

  sname <- c('quantile','se','lower','upper')
  expect_equal(colnames(out$quantile), sname)

  qua <- out$quantile
  expect_true(all(qua[,1] >= qua[,3]))
  expect_true(all(qua[,1] <= qua[,4]))
  expect_true(all(qua[,2] > 0))


})
