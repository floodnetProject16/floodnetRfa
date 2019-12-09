context('Testing FloodnetPot')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetProject16'))

ref <- '01AD002'
ref.u <- 1000 ## threshold
ref.area <- 14400 ## drainage area

test_that('Verifying FloodnetPot - output', {

  out <- FloodnetPot(ref, db = DB_HYDAT, u = ref.u, area = ref.area, verbose = FALSE)

  ## output format
  expect_equal(class(out),'data.frame' )

  sname <- c('site','method', 'distribution', 'period', 'variable', 'value')
  expect_equal(colnames(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'pot')

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
  out <- FloodnetPot(ref, period = c(20,50), db = DB_HYDAT,
  									 u = ref.u, area = ref.area,
										nsim = 0, verbose = FALSE, out.model = TRUE)

  expect_equal(names(out), c('fit','qua'))
  expect_equal(class(out$fit), 'fpot')

})

test_that('Verifying FloodnetPot - option', {

	## only one period
  out <- FloodnetPot(ref, period = 100, db = DB_HYDAT, verbose = FALSE,
  									 nsim = 5, u = ref.u, area = ref.area,)

	expect_equal(unique(out$period), 100)

	## No bootstrap
	out <- FloodnetPot(ref, period = c(20,50), db = DB_HYDAT,
											u = ref.u, area = ref.area,
											nsim = 0, verbose = FALSE)

	expect_equal(as.character(unique(out$variable)), 'quantile')


	## Test confidence interval alpha
	out1 <- FloodnetPot(ref, period = 100, db = DB_HYDAT,
											 u = ref.u, area = ref.area,
											nsim = 500, verbose = FALSE, alpha = .01)
	rg1 <- diff(out1[3:4,6])

	out2 <- FloodnetPot(ref, period = 100, db = DB_HYDAT,
											 u = ref.u, area = ref.area,
											nsim = 500, verbose = FALSE, alpha = .2)
  rg2 <- diff(out2[3:4,6])

  expect_true(rg2<rg1)



})

test_that('Verifying FloodnetFpot - tol.year', {

  ## Verify tol.year. For 01AD002 the first year of data are in October 1926.
  ## Normally 1926 is incomplet and so not used in the POT.
  ## Here we add peaks to see if it well removed or not
  daily <- DailyData(ref, DB_HYDAT)[,-1]
  daily[1,2] <- 2000

  out <- FloodnetPot(ref, period = 100, x = daily,
											 u = ref.u, area = ref.area, tol.year = 365,
											 nsim = 0, verbose = FALSE, out.model = TRUE)

  ymin <- min(as.integer(format(out$fit$time,'%Y')))
  expect_true(ymin > 1926)

  out <- FloodnetPot(ref, period = 100, x = daily,
											 u = ref.u, area = ref.area, tol.year = 50,
											 nsim = 0, verbose = FALSE, out.model = TRUE)

  ymin <- min(as.integer(format(out$fit$time,'%Y')))

  expect_true(ymin == 1926)

})


test_that('Verifying FloodnetPot - search', {

	## test all distribution
  out <- FloodnetPot(ref, db = DB_HYDAT, verbose = FALSE, area = ref.area, nsim = 0,
  									 out.model = TRUE)

  expect_equal(names(out), c('fit','qua','u'))

})


