context('Testing ExtractPeaksData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

info <- gaugedSites[1:3, c('station','auto','area')]

test_that('Verifying ExtractPeaksData', {

	xd <- DailyData(info$station, DB_HYDAT)

	s1 <- xd$station[1]
	y1 <- as.integer(format(xd$date[1],'%Y'))-1

	sdate <- as.Date(paste0(y1,'/1/20'))
	edate <- as.Date(paste0(y1,'/12/31'))
	x1 <- data.frame(station = s1, date = seq(sdate, edate, 'days'), value = 1)

	x1$value[200] <- max(xd$value) ## a peaks

	xd <- rbind(x1,xd)

	out0 <- ExtractPeaksData(xd, info)

	expect_equal(names(out0), c('peaks','sites','npeak','nyear','thresh'))

	expect_equal(as.character(unique(out0$peaks$station)), info$station)
  expect_equal(class(out0$peaks$date), 'Date')

  expect_equal(length(out0$npeak),3)
	expect_equal(length(out0$nyear),3)
	expect_equal(length(out0$nyear),3)

  out1 <- ExtractPeaksData(xd[sample(1:nrow(xd)),], info, tol = 346)
  expect_equal(out1,out0)

  ## date must be sorted
  dat <- tapply(out0$peaks$date, out0$peaks$station, diff)
  dat <- sapply(dat, min)
  expect_true( all(dat > 0))

  ## if not sorted
  out <- try(ExtractPeaksData(xd[sample(1:nrow(xd)),],
  													 info, tol = 346, sorted = TRUE), silent = TRUE)

  dat <- tapply(out$peaks$date, out$peaks$station, diff)
  dat <- sapply(dat, min)
  expect_true(!all(dat > 0))

  ## Using padding

  out <- ExtractPeaksData(xd, info, pad = TRUE, tol = 1)
  expect_equal(format(out$peaks$date[1], '%Y'), as.character(y1))

})

