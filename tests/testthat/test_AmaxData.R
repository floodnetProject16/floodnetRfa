context('Testing AmaxData')

## This is a config file that once loaded create a variable DB_HYDAT, which
## point to the location of a local version of HYDAT database
source(system.file('config', package = 'floodnetRfa'))

test_that('Verifying AmaxData', {

sites <- c('01AD002','01AF009')

## test pipe
out <- DB_HYDAT %>% AmaxData(sites)

out <- AmaxData(DB_HYDAT, sites)

expect_equal(colnames(out), c('site','date','value'))
expect_equal(unique(out$site), sites)
expect_equal(rownames(out), as.character(1:nrow(out)))

expect_true(is(out$site, 'character'))
expect_true(is(out$date, 'Date'))
expect_true(is(out$value, 'numeric'))

###################################
## Pooling groups
###################################

sites <- c("01AD002", "01AD003", "01AE001", "01AF007", "01AF009", "01AG002",
					 "01AJ003", "01AJ004", "01AJ010", "01AJ011")

## create a distance matrix
set.seed(1)
coord <- replicate(2,rnorm(10))
rownames(coord) <- sites
h <- dist(coord)
hm <- as.matrix(h)

## found the nearest sites manually
sname <- sort(order(hm[,3])[1:3])
sname <- colnames(hm)[sname]

## Extract info for pooling group
out <- AmaxData(DB_HYDAT, sites, size = 3, distance = hm[3,])
out.sites <- sort(unique(out$site))

expect_equal(sname, out.sites)

## A target is passed
out1 <- AmaxData(DB_HYDAT, sites, target = sites[3], size = 3)

sdis <- SeasonDistanceData(DB_HYDAT, sites = sites, target = sites[3])
out2 <- AmaxData(DB_HYDAT, sites, distance = sdis, size = 3)

expect_equal(out1,out2)

})


test_that('Verifying AmaxData', {

  ## This station is known to have a NA in HYDAT for annual maximum
  ## Must verify that the code pass
  sites <- c('01AP004')

  con <- RSQLite::dbConnect(RSQLite::SQLite(), DB_HYDAT)
  an <- HYDAT::AnnualPeakData(con, get_flow = TRUE, as.character(sites))
  RSQLite::dbDisconnect(con)

  an <- an[an$peak == 'MAXIMUM',]
  nyear <- sum(!is.na(an$value))

	out <-  AmaxData(DB_HYDAT, sites)

	expect_equal(nrow(out), nyear)

})
