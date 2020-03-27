context('Testing FloodnetPot')

set.seed(1)
ref.u <- 200 ## threshold
ref.area <- 200 ## drainage area
info <- data.frame('s', ref.u, ref.area)

ref <- SequenceData(365*32, site = 's' , freq = 'days', sdate = '1970-7-1')

test_that('Verifying FloodnetPot - output', {

  out <- FloodnetPot(ref, u = ref.u, area = ref.area, verbose = FALSE)

  ## output format
  expect_equal(class(out),'floodnetMdl')

  sname <- c('site', 'method', 'distr', 'period', 'quantile', 'param',
  					 'obs', 'time', 'rlevels', 'thresh', 'ppy', 'trend', 'gof')
  expect_equal(names(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), 's')

  expect_equal(as.character(unique(out$method)), 'pot')

  sname <- c('pred','se','lower','upper')
  expect_equal(colnames(out$quantile), sname)
  expect_equal(nrow(out$quantile), length(out$period))

  sname <- c('param','se')
  expect_equal(colnames(out$param), sname)

  sname <- c('alpha','kappa')
	expect_equal(rownames(out$param), sname)

  qua <- out$quantile
  expect_true(all(qua[,1] >= qua[,3]))
  expect_true(all(qua[,1] <= qua[,4]))
  expect_true(all(qua[,2] > 0))

})

test_that('Verifying FloodnetPot - option', {

	## only one period
  out <- FloodnetPot(ref, period = 100,  verbose = FALSE,
  									 nsim = 5, u = ref.u, area = ref.area)

	expect_equal(unique(out$period), 100)


	## Test confidence interval
	out1 <- FloodnetPot(ref, period = 100,
											 u = ref.u, area = ref.area,
											nsim = 500, verbose = FALSE, level = .99)
	rg1 <- diff(as.numeric(out1$quantile[1,3:4]))

	out2 <- FloodnetPot(ref, period = 100,
											 u = ref.u, area = ref.area,
											nsim = 500, verbose = FALSE, level = .8)
  rg2 <- diff(as.numeric(out2$quantile[1,3:4]))

  expect_true(rg2<rg1)

})

test_that('Verifying FloodnetFpot - tol.year', {

  out <- FloodnetPot(ref, u = ref.u-20, area = ref.area,  tol.year = 360,
  									 verbose = FALSE, out.model = TRUE)

  rgyear <- function(z) range(as.integer(format(z, '%Y')))

  expect_equal(rgyear(out$fit$time), c(1971,2001))

  out <- FloodnetPot(ref, u = ref.u-20, area = ref.area,  tol.year = 10,
  									 verbose = FALSE, out.model = TRUE)

  expect_equal(rgyear(out$fit$time), c(1970,2002))

})


test_that('Verifying FloodnetPot - search', {


	## test output model
  out <- FloodnetPot(ref,  area = ref.area, verbose = FALSE,
  									 out.model = TRUE)

  expect_true(all(c('fit','u') %in% names(out)))
  expect_equal(class(out$fit), 'fpot')

  sname <- c('u','ppy','ad','mrl','kap')
  expect_equal(colnames(out$u), sname)

})


