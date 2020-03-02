context('Testing SequenceData')

test_that('Verifying SequenceData', {

	x <- rnorm(3)

	yout <- SequenceData(x)
	dout <- SequenceData(x, freq = 'days', site.name = 'site2')

	## Verify column names
	expect_equal(colnames(yout), c('site','date','value'))
	expect_equal(colnames(dout), c('site','date','value'))

	## verify the frequency
	expect( diff(yout$date)[1], 365)
	expect( diff(dout$date)[1], 1)

	## verify site names
	expect_true(all(yout$site == 'site1'))
	expect_true(all(dout$site == 'site2'))

})
