context('Testing AmaxData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

test_that('Verifying AmaxData', {

sites <- c('01AD002','01AF009')

out <- AmaxData(sites, DB_HYDAT)

expect_equal(colnames(out), c('station','year','value'))
expect_equal(unique(out$station), sites)
expect_equal(rownames(out), as.character(1:nrow(out)))

expect_true(class(out$station) == 'character' )
expect_true(class(out$year) == 'integer' )
expect_true(class(out$value) == 'numeric' )

out <- AmaxData(sites, DB_HYDAT, year = FALSE)
expect_true(class(out$date) == 'Date')

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
out <- AmaxData(sites,DB_HYDAT, target = sites[3], size = 3, distance = hm)
out.sites <- sort(unique(out$station))

expect_equal(sname, out.sites)

## A "dist object" is passed
out1 <- AmaxData(sites,DB_HYDAT, target = sites[3], size = 3, distance = h)
expect_equal(out1,out)

## only vector is passed
out1 <- AmaxData(sites,DB_HYDAT, target = sites[3], size = 3, distance = hm[sites[3],])
expect_equal(out1,out)

})
