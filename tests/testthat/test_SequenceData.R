context('Testing SequenceData')

test_that('Verifying SequenceData', {

	x <- rnorm(3)

	yout <- SequenceData(x)
	dout <- SequenceData(x, freq = 'days', site = 'site2')

	## Verify column names
	expect_equal(colnames(yout), c('site','date','value'))
	expect_equal(colnames(dout), c('site','date','value'))

	## verify the frequency
	expect( diff(yout$date)[1], 365)
	expect( diff(dout$date)[1], 1)

	## verify site names
	expect_true(all(yout$site == 'site1'))
	expect_true(all(dout$site == 'site2'))

	d <- as.Date(c('1960-1-1','1970-2-2','2000-3-3'))

	sout <- SequenceData(x,d)

	expect_equivalent(d, sout$date)
	expect_equivalent(x, sout$value)

	##
	dout <- SequenceData(x, freq = 'days', site = 'site2', sdate = '2000-01-01')
	expect_equivalent(dout$date[1], as.Date('2000-01-01'))

})
