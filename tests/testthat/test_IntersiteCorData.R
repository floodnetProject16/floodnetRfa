context('Testing IntersiteCorData')

## This is a config file that once loaded create a variable DB_HYDAT that point to the location of a downloaded version of HYDAT database
source(system.file("config", package = 'floodnetRfa'))

test_that('Verifying intersite', {

  sites <-  c("01AD002","01BJ007","02BB003","02JE015","02LG005","02OJ001",
  						"02OJ007","02RD002","02RD003","02RF001","02RF002", "02RF006",
  						"02UC002","02VB004","02VC001","02WB003","02XA003","03AB002",
  						"03AC002","03AC004","03AD001","03BA003","03BB002","03BC002",
  						"03BD002","03BE001","03BF001","03CB004","03DA002","03DD002",
  						"03DD003","03EA001","03EC001","03ED001","03ED004","03FC007",
  						"03FC008","03HA001","03JB001","03KA001","03KC004","03MC001",
  						"03MD001","03NF001","03OC003","03PB002","03QC001","04CA002",
  						"04CB001","04DA001","04DB001","04DC001","04DC002","04FA001",
  						"04FA003","04FB001","04FC001","04JA002","04JC002","04JD005",
  						"04JF001","04KA001","04LJ001","04LM001","04MF001","04NB001",
							"05QA001")

 stn <- StationData(DB_HYDAT, sites)
 h <- CSHShydRology::GeoDist(stn[,c('lon','lat')])
 v <- h[lower.tri(h)]

 ## Intersite correlation for a given super region
 x <- IntersiteCorData(DB_HYDAT, sites, smooth = .6, distance.max = 500,
 											distance.bin = 14)
 o <- chol(x)

 expect_equal(class(x), "matrix")

 # Verify that the output is a monotone decreasing function
 dx <- diff(x[lower.tri(x)][order(v)])
 expect_true(all(dx < 0))

 ## Intersite correlation for a given super region
 x <- IntersiteCorData(DB_HYDAT, sites, type = 'emp')
 o <- chol(x)

 # Verify that the output is not monotone decreasing function
 dx <- diff(x[lower.tri(x)][order(v)])
 expect_false(all(dx < 0))


 ## Test for raw coefficient
 x <- IntersiteCorData(DB_HYDAT, sites, type = 'raw' )
 expect_true(any(is.na(x)))
 expect_error(o <- chol(x))

 x <- IntersiteCorData(DB_HYDAT, sites, type = 'avg' )
 expect_equal(length(x), 1)

})
