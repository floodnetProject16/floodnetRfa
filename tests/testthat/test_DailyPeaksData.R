context('Testing DailyPeaksData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

info <- gaugedSites[1:3, c('station','auto','area')]

test_that('Verifying DailyPeaksData', {

	xd <- DailyData(info[,1], DB_HYDAT)
	out0 <- ExtractPeaksData(xd,info)
	out2 <- DailyPeaksData(info,DB_HYDAT)
  expect_equal(out2,out0)

  ## verify the output format
  expect_equal(names(out0),c('peaks','sites','npeak','nyear','thresh'))
  expect_equal(colnames(out0$peaks),c('station','date','value'))
  expect_equal(class(out0$peaks),'data.frame')
  expect_equal(out0$sites, sort(unique(info$station)))
  expect_equal(out0$thresh, info[,2])
  expect_equal(length(out0$nyear), 3)
  expect_equal(length(out0$npeak), 3)

	## Drainage area not passed, but read in HYDAT
	info0 <- info[,-3]
	out <- try(DailyPeaksData(info0,DB_HYDAT), silent = TRUE)
	expect_false(class(out) == 'try-error')

	## Drainage area not passed, but read in HYDAT
	info0 <- info
	info[3,3] <- NA
	out <- try(DailyPeaksData(info,DB_HYDAT), silent = TRUE)
	expect_false(class(out) == 'try-error')
  expect_equal(unique(out$peaks$station), info$station)

  ## Just make sure that it run without problem when pad is TRUE
  xd <- DailyPeaksData(info, DB_HYDAT, pad = TRUE)

  ###################################
  ## Pooling groups
  ###################################

  sites <- c("01AD002", "01AD003", "01AE001", "01AF007", "01AF009", "01AG002",
					 "01AJ003", "01AJ004", "01AJ010", "01AJ011")

  info <- gaugedSites[gaugedSites$station %in% sites, c('station','auto','area')]

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
  out <- DailyPeaksData(info, DB_HYDAT, target = sites[3], size = 3,
  											distance = hm)

  expect_equal(sname, out$sites)

  ## A "dist object" is passed
  out1 <- DailyPeaksData(info,DB_HYDAT, target = sites[3], size = 3,
  											 distance = h)
  expect_equal(out1,out)

  ## only vector is passed
  out1 <- DailyPeaksData(info, DB_HYDAT, target = sites[3],
  											 size = 3, distance = hm[sites[3],])

  expect_equal(out1,out)

})
