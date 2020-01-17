context('Testing FloodnetAmax')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

ref <- '01AF009'

test_that('Verifying FloodnetAmax - output', {

  out <- FloodnetAmax(ref, db = DB_HYDAT, distr = 'gev', verbose = FALSE)

  ## output format
  expect_equal(class(out),'data.frame' )

  sname <- c('site','method', 'distribution', 'period', 'variable', 'value')
  expect_equal(colnames(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'amax')

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
  out <- FloodnetAmax(ref, period = c(20,50), db = DB_HYDAT, distr = 'gev',
											nsim = 0, verbose = FALSE, out.model = TRUE)

  expect_equal(names(out), c('fit','qua'))
  expect_equal(class(out$fit), 'amax')

})

test_that('Verifying FloodnetAmax - option', {

	## only one period
  out <- FloodnetAmax(ref, period = 100, db = DB_HYDAT,
  										distr = 'gev', verbose = FALSE)

	expect_equal(unique(out$period), 100)

	## No bootstrap
	out <- FloodnetAmax(ref, period = c(20,50), db = DB_HYDAT, distr = 'gev',
											nsim = 0, verbose = FALSE)

	expect_equal(as.character(unique(out$variable)), 'quantile')


	## Test confidence interval
	out1 <- FloodnetAmax(ref, period = 100, db = DB_HYDAT, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .99)
	rg1 <- diff(out1[3:4,6])

	out2 <- FloodnetAmax(ref, period = 100, db = DB_HYDAT, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .8)
  rg2 <- diff(out2[3:4,6])

  expect_true(rg2<rg1)


})

test_that('Verifying FloodnetAmax - distributions', {

	## test all distribution
  out <- FloodnetAmax(ref, db = DB_HYDAT, verbose = FALSE, distr = 'gev', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gev')

  out <- FloodnetAmax(ref, db = DB_HYDAT, verbose = FALSE, distr = 'gno', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'gno')

  out <- FloodnetAmax(ref, db = DB_HYDAT, verbose = FALSE, distr = 'pe3', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'pe3')

  out <- FloodnetAmax(ref, db = DB_HYDAT, verbose = FALSE, distr = 'glo', nsim = 0)
  expect_equal(as.character(unique(out$distribution)), 'glo')

  ## Test instantanous peaks
	out <- FloodnetAmax(ref, period = 100, db = DB_HYDAT, distr = 'gev',
											nsim = 0, verbose = FALSE, instant = TRUE)

})

test_that('Verifying FloodnetAmax - source', {

  out1 <- FloodnetAmax(ref, period = 100, db = DB_HYDAT,
  										distr = 'gev', verbose = FALSE, nsim = 0)

  x <- AmaxData(ref, DB_HYDAT)

  out2 <- FloodnetAmax(ref, period = 100, x = x$value, distr = 'gev',
  										 verbose = FALSE, nsim = 0)

  expect_equal(out1,out2)

})

test_that('Verifying FloodnetAmax - warning', {
  x <- AmaxData(ref, DB_HYDAT)
  xmod <- x$value + 50 * (1:nrow(x) > 14) ## add a change point

  f <- tempfile()
  sink(file = f)
  expect_warning(FloodnetAmax(ref, period = 100, x = xmod, distr = 'gev',
  										 verbose = TRUE, nsim = 0))
  sink()
  file.remove(f)
})

