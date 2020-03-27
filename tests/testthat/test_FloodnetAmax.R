context('Testing FloodnetAmax')

set.seed(1)
an <- SequenceData(50)

test_that('Verifying FloodnetAmax - output', {

  out <- FloodnetAmax(an, distr = 'gev', verbose = FALSE, nsim = 500)

  ## output format
  expect_equal(class(out),'floodnetMdl')

  sname <- c('site', 'method', 'distr', 'period', 'quantile', 'param',
  					 'obs', 'time', 'rlevels', 'thresh', 'ppy', 'trend', 'gof')
  expect_equal(names(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), 'site1')

  expect_equal(as.character(unique(out$method)), 'amax')

  sname <- c('pred','se','lower','upper')
  expect_equal(colnames(out$quantile), sname)
  expect_equal(nrow(out$quantile), length(out$period))

  sname <- c('param','se')
  expect_equal(colnames(out$param), sname)
  sname <- c('xi', 'alpha', 'kappa')
  expect_equal(rownames(out$param), sname)

  qua <- out$quantile
  expect_true(all(qua[,1] >= qua[,3]))
  expect_true(all(qua[,1] <= qua[,4]))
  expect_true(all(qua[,2] > 0))

  ## test output model
  out <- FloodnetAmax(an, distr = 'gev',
											nsim = 0, verbose = FALSE, out.model = TRUE)

  expect_true('fit' %in% names(out))
  expect_equal(class(out$fit), 'amax')

  hat <- predict(out$fit)
  expect_equal(hat, out$quantile[,1])

})

test_that('Verifying FloodnetAmax - option', {

	## only one period
  out <- FloodnetAmax(an, period = 100,
  										distr = 'gev', verbose = FALSE)

	expect_equal(unique(out$period), 100)

	## Test confidence interval
	set.seed(1)
	out1 <- FloodnetAmax(an, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .99)
	rg1 <- diff(as.numeric(out1$quantile[1,3:4]))

	out2 <- FloodnetAmax(an, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .8)
  rg2 <- diff(as.numeric(out2$quantile[1,3:4]))

  expect_true(rg2<rg1)


})

test_that('Verifying FloodnetAmax - distributions', {

	## test all distribution
  out <- FloodnetAmax(an,  verbose = FALSE, distr = 'gev', nsim = 0)
  expect_equal(as.character(unique(out$distr)), 'gev')

  out <- FloodnetAmax(an,  verbose = FALSE, distr = 'gno', nsim = 0)
  expect_equal(as.character(unique(out$distr)), 'gno')

  out <- FloodnetAmax(an,  verbose = FALSE, distr = 'pe3', nsim = 0)
  expect_equal(as.character(unique(out$distr)), 'pe3')

  out <- FloodnetAmax(an,  verbose = FALSE, distr = 'glo', nsim = 0)
  expect_equal(as.character(unique(out$distr)), 'glo')

})


test_that('Verifying FloodnetAmax - warning', {
  xmod <- an$value + 50 * (1:nrow(an) > 14) ## add a change point

  #f <- tempfile()
  #sink(file = f)
  expect_warning(FloodnetAmax(an, period = 100, distr = 'gev',
  										 verbose = TRUE, nsim = 0))
  #sink()
  #file.remove(f)
})

