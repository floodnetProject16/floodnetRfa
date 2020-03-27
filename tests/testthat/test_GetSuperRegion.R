context('Testing GetSuperRegion')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

info <- gaugedSites[1:3, c('station','auto','area')]

test_that('Verifying GetSuperRegion', {

	## dummy test
	expect_equal(0,0)

})
