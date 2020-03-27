context('Testing DailyPeaksData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

info <- gaugedSites[1:3, c('station','auto','area')]

test_that('Verifying DailyPeaksData', {

	xd <- DailyData(DB_HYDAT, info[,1])
	out0 <- ExtractPeaksData(xd, info)
	out2 <- DailyPeaksData(DB_HYDAT, info)
  expect_equal(out2,out0)

  ## Just make sure that it run without problem when pad is TRUE
  xd <- DailyPeaksData(DB_HYDAT, info,  pad = TRUE)

  ###################################
  ## Pooling groups
  ###################################

  sites <- c("01AD002", "01AD003", "01AE001", "01AF007", "01AF009", "01AG002",
					 "01AJ003", "01AJ004", "01AJ010", "01AJ011")

  info <- gaugedSites[gaugedSites$station %in% sites, c('station','auto','area')]

  ## create a distance matrix
  hm <- SeasonDistanceData(DB_HYDAT, info$station)

  ## Extract info for pooling group
  out <- DailyPeaksData(DB_HYDAT, info, target =  sites[3], size = 3)

  ## A "dist object" is passed
  out1 <- DailyPeaksData(DB_HYDAT, info, distance = hm[3,], size = 3)

  expect_equal(out1,out)

})
