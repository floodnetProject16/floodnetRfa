context('Testing FloodnetPoolMle')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetProject16'))

ref <- '01AF009'

test_that('Verifying FloodnetPoolMle', {

	sreg <- with(gaugedSites,gaugedSites[supreg_km12 == 11,1])
	sdist <- SeasonDistanceData(sreg, DB_HYDAT)

	an <- AmaxData(sreg, db = DB_HYDAT, target = ref, size = 5, distance = sdist)

  out <- FloodnetPoolMle(an, ref, distr = 'gev', verbose = FALSE,
  											 nsim = 5)

  out.cv <- FloodnetPoolMle(an, ref, distr = 'gno', verbose = FALSE,
  											 nsim = 5, type = 'cv')

  out.mean <- FloodnetPoolMle(an, ref, distr = 'glo', verbose = FALSE,
  											 nsim = 5, type = 'shape')

  ## output format
  expect_equal(class(out),'data.frame' )

  sname <- c('site','method', 'distribution', 'period', 'variable', 'value')
  expect_equal(colnames(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'pool_amax_mean')
  expect_equal(as.character(unique(out.cv$method)), 'pool_amax_cv')
  expect_equal(as.character(unique(out.mean$method)), 'pool_amax_shape')

  sname <- c('quantile','se','lower','upper')
  expect_equal(as.character(unique(out$variable)), sname)

  qua <- with(out, value[variable == 'quantile'])
  se <- with(out, value[variable == 'se'])
  lb <- with(out, value[variable == 'lower'])
  ub <- with(out, value[variable == 'upper'])

  expect_true(all(qua >= lb))
  expect_true(all(qua <= ub))
  expect_true(all(se > 0))

  ## test output model
  out <- FloodnetPoolMle(an, ref, distr = 'pe3', verbose = FALSE,
  											 out.model = TRUE, nsim = 0)

  expect_equal(names(out), c('fit','qua'))
  expect_equal(class(out$fit), 'poolmle')

  hat <- predict(out$fit)[1,]
  qq <- with(out$qua, value[ variable == 'quantile'])
  expect_equivalent(hat, qq)


	## no distribution
	expect_error(FloodnetPoolMle(an, ref, verbose = FALSE))

	## only one period
  out <- FloodnetPoolMle(an, ref, distr = 'gev', nsim = 5,
  											 period = 100, verbose = FALSE)

	expect_equal(unique(out$period), 100)

	## No bootstrap
	out <- FloodnetPool(an, ref, period = c(20,50),
											nsim = 0, verbose = FALSE)

	expect_equal(as.character(unique(out$variable)), 'quantile')


	## Test confidence interval alpha
	set.seed(13)
	out1 <- FloodnetPoolMle(an, ref, period = 100, distr = 'gev',
											nsim = 50, verbose = FALSE, alpha = .01)
	rg1 <- diff(out1[3:4,6])

	out2 <- FloodnetPoolMle(an, ref, period = 100, distr = 'gev',
											nsim = 50, verbose = FALSE, alpha = .2)
  rg2 <- diff(out2[3:4,6])

  expect_true(rg2<rg1)


	## test all distribution
  out <-FloodnetPoolMle(an, ref, verbose = FALSE, distr = 'gev', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gev')

  out <- FloodnetPoolMle(an, ref, verbose = FALSE, distr = 'gno', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gno')

  out <- FloodnetPoolMle(an, ref, verbose = FALSE, distr = 'pe3', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'pe3')

  out <- FloodnetPoolMle(an, ref, verbose = FALSE, distr = 'glo', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'glo')

})


test_that('FloodnetPool- POT', {

	info <- with(gaugedSites,gaugedSites[supreg_km12 == 11,
																			 c('station','auto','area')])
	sreg <- info$station
	sdist <- SeasonDistanceData(sreg, DB_HYDAT)

	xd <- DailyPeaksData(info, db = DB_HYDAT, target = ref, size=5, distance = sdist)

  out <- FloodnetPoolMle(xd, ref, verbose = FALSE, out.model = TRUE, nsim = 5)
  out.shape <- FloodnetPoolMle(xd, ref, verbose = FALSE, out.model = TRUE,
  														 type = 'shape', nsim = 5)

  fit <- out$fit
  qua <- out$qua
  expect_equal(as.character(unique(qua$method)), 'pool_pot_mean')
  expect_equal(as.character(unique(out.shape$qua$method)), 'pool_pot_shape')
  expect_equal(as.character(unique(qua$distribution)), 'gpa')

  ppy <- (xd$npeak/xd$nyear)[1]
  u <- xd$thresh[1]
  p <- 1-1/(ppy*c(2,5, 10, 20, 50, 100))

  qq <- with(qua, value[variable == 'quantile'])
  hat <- predict(fit, p)[1,] + xd$thresh[1]
  expect_equivalent(hat,qq)


})
