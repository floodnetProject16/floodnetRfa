context('Testing DailyData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

test_that('Verifying DailyData', {

  sites <- c('01AD002','01AF009')

  out <- DailyData(DB_HYDAT, sites)

  expect_equal(colnames(out), c('site','date','value'))
  expect_equal(unique(out$site), sites)
  expect_equal(rownames(out), as.character(1:nrow(out)))

  expect_true(is(out$site, 'character'))
  expect_true(is(out$date, 'Date'))
  expect_true(is(out$value, 'numeric'))

  ## padding

  out1 <- DailyData(DB_HYDAT, sites,pad = TRUE, tol = 1)
  out2 <- DailyData(DB_HYDAT, sites,pad = TRUE, tol = 365)
  expect_true(any(out1[1,] != out2[1,]))


  ###################################
  ## Pooling groups
  ###################################

  sites <- c("01AD002", "01AD003", "01AE001", "01AF007", "01AF009", "01AG002",
					 "01AJ003", "01AJ004", "01AJ010", "01AJ011")

  ## create a distance matrix
  set.seed(2)
  coord <- replicate(2,rnorm(10))
  rownames(coord) <- sites
  h <- dist(coord)
  hm <- as.matrix(h)

  ## found the nearest sites manually
  sname <- sort(order(hm[,3])[1:3])
  sname <- colnames(hm)[sname]

  ## Extract info for pooling group
  out <- DailyData(DB_HYDAT, sites, size = 3, distance = hm[3,])
  out.sites <- sort(unique(out$site))

  expect_equal(sname, out.sites)

  ## Only a target is passed
  out1 <- DailyData(DB_HYDAT, sites, target = sites[3])

  sdis <- SeasonDistanceData(DB_HYDAT, sites, target = 3)
  out2 <- DailyData(DB_HYDAT, sites, distance = sdis)

  expect_equal(out1, out2)

})
