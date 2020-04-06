context('Testing ExtractPeaksData')

## This is a config file that once loaded create a variable DB_HYDAT that point
## to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

info <- GAUGEDSITES[1:3, c('station','auto','area')]

test_that('Verifying ExtractPeaksData', {

	# Import data from HYDAT and extract peaks
	xd <- DailyData(DB_HYDAT, info$station)

	s1 <- xd$site[1]
	y1 <- as.integer(format(xd$date[1],'%Y'))-1

	sdate <- as.Date(paste0(y1,'/1/20'))
	edate <- as.Date(paste0(y1,'/12/31'))
	x1 <- data.frame(site = s1, date = seq(sdate, edate, 'days'), value = 1)

	x1$value[200] <- max(xd$value) ## a peaks

	xd <- rbind(x1, xd)

	out0 <- ExtractPeaksData(xd, info)

	## verify peaks info
	expect_equal(as.character(unique(out0$site)), as.character(info$station))
  expect_equal(class(out0$date), 'Date')

  ## verify meta info
	expect_equal(names(out0), c('site','date','value'))
	meta <- PeaksMeta(out0)

	expect_equal(dim(meta), c(3,2))
	expect_equal(rownames(meta), as.character(info$station))
	expect_equal(colnames(meta), c('thresh','ppy'))

	## verify that sorting
  out1 <- ExtractPeaksData(xd[sample(1:nrow(xd)),], info, tol = 346)
  expect_equal(out1,out0)

  ## date must be sorted
  dat <- tapply(out0$date, out0$site, diff)
  dat <- sapply(dat, min)
  expect_true(all(dat > 0))

  ## if not sorted
  out <- try(ExtractPeaksData(xd[sample(1:nrow(xd)),],
  													 info, tol = 346, sorted = TRUE), silent = TRUE)

  dat <- tapply(out$date, out$site, diff)
  dat <- sapply(dat, min)
  expect_true(all(dat > 0))

  ## Using padding

  out <- ExtractPeaksData(xd, info, pad = TRUE, tol = 1)
  expect_equal(format(out$date[1], '%Y'), as.character(y1))

})

