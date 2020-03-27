context('Testing SeasonDistance')

test_that('Verifying SeasonDistance', {

## Quick testing of demo data
expect_error(DemoData('fdss'))

an <- DemoData('region')
h <- SeasonDistance(an)
sn <- unique(an[,1])

expect_equal(dim(h), c(38,38))
expect_equivalent(colnames(h), as.character(sn))

h2 <- SeasonDistance(an, target = '01AF009')
expect_equivalent(h2, h[2,])

expect_equivalent(round(h2[1:3],4), c(0.1487, 0, 0.3869))

})
