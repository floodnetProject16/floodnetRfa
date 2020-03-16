context('Testing AmaxData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file('config', package = 'floodnetRfa'))

test_that('Verifying AmaxData', {

sites <- c('01AD002','01AF009')

out <- AmaxData(sites, DB_HYDAT)

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
out <- AmaxData(sites,DB_HYDAT, size = 3, distance = hm[3,])
out.sites <- sort(unique(out$site))

expect_equal(sname, out.sites)

## A distance is passed
out1 <- AmaxData(sites,DB_HYDAT, target = sites[3], size = 3)
expect_equal(out1,out)

## only target is passed
out1 <- AmaxData(sites,DB_HYDAT, target = sites[3], size = 3)

sdis <- SeasonDistanceData(sites, db =  DB_HYDAT, target = 3)
out2 <- AmaxData(sites,DB_HYDAT, distance = sdis, size = 3)

expect_equal(out1,out2)

})
