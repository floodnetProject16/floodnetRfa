context('Testing FloodnetPool')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

ref <- '01AF009'

test_that('Verifying FloodnetPool- output', {

	sreg <- with(gaugedSites,gaugedSites[supreg_km12 == 11,1])
	sdist <- SeasonDistanceData(sreg, DB_HYDAT)

	an <- AmaxData(sreg, db = DB_HYDAT, target = ref, size = 5, distance = sdist)

  out <- FloodnetPool(an, ref, distr = 'gev', verbose = FALSE)

  ## output format
  expect_equal(class(out),'data.frame' )

  sname <- c('site','method', 'distribution', 'period', 'variable', 'value')
  expect_equal(colnames(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'pool_amax')

  sname <- c('quantile','rmse','lower','upper')
  expect_equal(as.character(unique(out$variable)), sname)

  qua <- with(out, value[variable == 'quantile'])
  se <- with(out, value[variable == 'rmse'])
  lb <- with(out, value[variable == 'lower'])
  ub <- with(out, value[variable == 'upper'])

  expect_true(all(qua >= lb))
  expect_true(all(qua <= ub))
  expect_true(all(se > 0))

  ## test output model
  out <- FloodnetPool(an, ref, verbose = FALSE, out.model = TRUE)

  expect_equal(names(out), c('fit','qua'))
  expect_equal(class(out$fit), 'reglmom')

  hat <- predict(out$fit)
  qq <- with(out$qua, value[ variable == 'quantile'])
  expect_equal(hat, qq)


	## only one period
  out <- FloodnetPool(an, ref, period = 100, verbose = FALSE)

	expect_equal(unique(out$period), 100)

	## No bootstrap
	out <- FloodnetPool(an, ref, period = c(20,50),
											nsim = 0, verbose = FALSE)

	expect_equal(as.character(unique(out$variable)), 'quantile')


	## Test confidence interval
	set.seed(13)
	out1 <- FloodnetPool(an, ref, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .99)
	rg1 <- diff(out1[3:4,6])

	out2 <- FloodnetPool(an, ref, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .8)
  rg2 <- diff(out2[3:4,6])

  expect_true(rg2<rg1)



	## test all distribution
  out <-FloodnetPool(an, ref, verbose = FALSE, distr = 'gev', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gev')

  out <- FloodnetPool(an, ref, verbose = FALSE, distr = 'gno', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gno')

  out <- FloodnetPool(an, ref, verbose = FALSE, distr = 'pe3', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'pe3')

  out <- FloodnetPool(an, ref, verbose = FALSE, distr = 'glo', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'glo')

})


test_that('FloodnetPool- POT', {

	info <- with(gaugedSites,gaugedSites[supreg_km12 == 11,
																			 c('station','auto','area')])
	sreg <- info$station
	sdist <- SeasonDistanceData(sreg, DB_HYDAT)

	xd <- DailyPeaksData(info, db = DB_HYDAT, target = ref, size=5, distance = sdist)

  out <- FloodnetPool(xd, ref, verbose = FALSE, out.model = TRUE)

  fit <- out$fit
  qua <- out$qua
  expect_equal(as.character(unique(qua$method)), 'pool_pot')
  expect_equal(as.character(unique(qua$distribution)), 'gpa')

  ppy <- (xd$npeak/xd$nyear)[1]
  u <- xd$thresh[1]
  p <- 1-1/(ppy*c(2,5, 10, 20, 50, 100))

  qq <- with(qua, value[variable == 'quantile'])
  hat <- predict(fit, p)+xd$thresh[1]
  expect_equal(hat,qq)


})
