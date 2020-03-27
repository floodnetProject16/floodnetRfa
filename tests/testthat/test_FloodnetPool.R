context('Testing FloodnetPool')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

ref <- '01AF009'

test_that('Verifying FloodnetPool- output', {

	sreg <- with(gaugedSites,gaugedSites[supreg_km12 == 11,1])
	sdist <- SeasonDistanceData(DB_HYDAT, sreg)

	an <- AmaxData(DB_HYDAT, sreg, target = ref, size = 5)

  out <- FloodnetPool(an, target = ref, distr = 'gev',
  										verbose = FALSE, tol.H = Inf)

  ## output format
  expect_equal(class(out),'floodnetMdl')

  sname <- c('site', 'method', 'distr', 'period', 'quantile', 'param',
  					 'obs', 'time', 'rlevels', 'thresh', 'ppy', 'trend', 'gof','lmom')
  expect_equal(names(out), sname)

  period <- c(2, 5, 10, 20, 50, 100)
  expect_equal(unique(out$period), period)

  expect_equal(as.character(unique(out$site)), ref)

  expect_equal(as.character(unique(out$method)), 'pool_amax')

  sname <- c('pred','rmse','lower','upper')
  expect_equal(colnames(out$quantile), sname)
  expect_equal(nrow(out$quantile), length(out$period))

  sname <- c('param','se')
  expect_equal(colnames(out$param), sname)
  sname <- c('IF', 'xi', 'alpha', 'kappa')
  expect_equal(rownames(out$param), sname)

  qua <- out$quantile
  expect_true(all(qua[,1] >= qua[,3]))
  expect_true(all(qua[,1] <= qua[,4]))
  expect_true(all(qua[,2] > 0))


  ## test output model
  out <- FloodnetPool(an, ref, verbose = FALSE, tol.H = Inf, out.model = TRUE)

  expect_true('fit' %in% names(out))
  expect_equal(class(out$fit), 'reglmom')

  hat <- predict(out$fit)
  expect_equal(hat, out$quantile[,1])


	## only one period
  out <- FloodnetPool(an, ref, period = 100, verbose = FALSE)

	expect_equal(unique(out$period), 100)

	## Test confidence interval
	set.seed(13)
	out1 <- FloodnetPool(an, ref, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .99)

	rg1 <- diff(as.numeric(out1$quantile[1,3:4]))

	out2 <- FloodnetPool(an, ref, period = 100, distr = 'gev',
											nsim = 500, verbose = FALSE, level = .8)

  rg2 <- diff(as.numeric(out2$quantile[1,3:4]))

  expect_true(rg2<rg1)



	## test all distribution
  distr.list <- c('gev','gno','glo','pe3')
  for(d in distr.list){
	  out <- FloodnetPool(an, ref, verbose = FALSE, distr = d)
  	expect_equal(as.character(unique(out$distr)), d)
  }


})


test_that('FloodnetPool- POT', {

	sreg <- GetSuperRegion(ref, 'pot')

	out <- DB_HYDAT %>%
		DailyPeaksData(sreg, target = ref, size=5) %>%
    FloodnetPool(ref, verbose = FALSE)

  expect_equal(out$method, 'pool_pot')
  expect_equal(out$distr, 'gpa')

})
