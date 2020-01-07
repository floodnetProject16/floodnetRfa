## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.align = 'center')

mycols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
						'#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

palette(mycols)

source(system.file("config", package = 'floodnetProject16'))

## ------------------------------------------------------------------------
library(floodnetProject16)
library(CSHShydRology)

## Station of interest and path of the HYDAT database
mystation <- '01AF009'
db <- DB_HYDAT

## ------------------------------------------------------------------------
set.seed(1)
FloodnetAmax(site = mystation, db = db, period = c(10,100))

## ------------------------------------------------------------------------
set.seed(1)

an <- AmaxData('01AF009', db)
out <- FloodnetAmax(period = 100, x = an$value, out.model = TRUE, nsim = 0)
names(out)

## ----fig.height = 4, fig.width = 6---------------------------------------
plot(out$fit, ci = TRUE)

## ------------------------------------------------------------------------
## Create a change point to existing data
an.mod <- an$value + 50 * (1:nrow(an) > 14) 

set.seed(1)
out <- FloodnetAmax(x = an.mod, period = 100)

## ------------------------------------------------------------------------
set.seed(1)
FloodnetPot(site = mystation, db = db, period = 100, u = 20, area = 184.1)

## ------------------------------------------------------------------------
set.seed(1)

daily <- DailyData(mystation, db)[,-1]

FloodnetPot(x = daily, period = 100, u = 20, area = 184.1, nsim = 0)


## ------------------------------------------------------------------------
gaugedSites[5, c('station','description', 'area','auto','ppy250')]

## ------------------------------------------------------------------------
set.seed(1)
out <- FloodnetPot(period = 100, site = mystation, db = db, 
                                     area = 184.1, out.model = TRUE)

GofTest(out$fit, method = 'ad')


## ---- echo = FALSE, fig.height = 4, fig.width = 6------------------------
plot(out$fit, ci = TRUE)

## ---- fig.width=6, fig.height=10-----------------------------------------
uval <- out$u[out$u[,"ppy"] > 1.2,]
par(mfrow = c(3,1))

Fplot <- function(vname, mainLab, ylab){ 
  plot(uval[,'u'], uval[, vname], type = 'l', main = mainLab ,
       xlab = 'Threshold', ylab = ylab)
  abline(v = 15.9, col = 6, lwd = 2)
  abline(v = 20, col = 2, lwd = 2)
}

Fplot('mrl', 'Mean Residual Life', 'MRL')
legend('topright', col = c(6,2), lty = rep(1,2), 
             legend = c('auto','manual'))
Fplot('ad', 'Anderson-Darling', 'p-value')
Fplot('kap', 'Shape parameter', 'Kappa')

## ---- fig.height=10, fig.width = 6, echo = FALSE-------------------------
layout(matrix(c(1,2), 2,1))

sp::plot(map_ca)
axis(1)
axis(2)

points(lat~lon, gaugedSites, pch = 16, col = supreg_km12,
         main = 'Geographical space', ylim = c(42,72))

legend('top', horiz = TRUE, col = 1:12, legend = 1:12, pch = 16,
             cex = .6)

legend('bottomleft', pch = 10, legend = 'Target')

with(gaugedSites[5, ], 
         points(lon,lat, cex = 3, col = 'black', pch = 10))

plot(log(map)~ log(area), gaugedSites, pch = 16, col = supreg_km12,
         main = 'Descriptor space')

with(gaugedSites[5, ], 
         points(log(area), log(map), cex = 3, col = 'black', pch = 10))


## ------------------------------------------------------------------------
## Filter nonstationary sites from the super region of the target
target.supreg <- with(gaugedSites, supreg_km12[station == mystation])
cond_supreg <- with(gaugedSites, supreg_km12 == target.supreg)

pval.mk <- gaugedSites$trend_mk ## Mann-Kendall
pval.pt <- gaugedSites$trend_pt ## Pettitt
cond.trend <- pval.mk >= .05 & pval.pt >= .05
mysites <- gaugedSites[cond_supreg & cond.trend,'station']

addmargins(table(cond_supreg, cond.trend))

## ------------------------------------------------------------------------
season.dist <- SeasonDistanceData(mysites, db)

xd <- AmaxData(mysites, db, target = mystation, size = 25,
                             distance = season.dist)

## ------------------------------------------------------------------------
set.seed(1)
out <- FloodnetPool(x = xd, target = mystation, 
                                 period = 100, distr = 'gev', out.model = TRUE, verbose = FALSE)
print(out$fit)


## ------------------------------------------------------------------------
print(out$qua)

## ------------------------------------------------------------------------
set.seed(1)
FloodnetPoolMle(x = xd, target = mystation, period = 100,
                                type = 'mean', nsim = 500)

FloodnetPoolMle(x = xd, target = mystation, period = 100,
                                type = 'cv', nsim = 0)

FloodnetPoolMle(x = xd, target = mystation, period = 100, 
                                type = 'shape', nsim = 0)


## ------------------------------------------------------------------------
## Filter nonstationary sites from the super region of the target
pval.mx <- gaugedSites$trend_mx ## Mann-Kendall
pval.lg <- gaugedSites$trend_lg ## logistic regression
cond.trend <- pval.lg >= .05 & pval.mx >= .05

info <- gaugedSites[cond_supreg & cond.trend, c('station','auto','area')]

season.dist <- SeasonDistanceData(info$station, db)

head(info,3)

## ------------------------------------------------------------------------
addmargins(table(cond_supreg, cond.trend))

## ------------------------------------------------------------------------
xd <- DailyPeaksData(info, db, target = '01AF009', 
                                         size = 25, distance = season.dist)


## ------------------------------------------------------------------------
set.seed(1)
out <- FloodnetPool(xd, target = mystation, period = 100, verbose = TRUE,
                         out.model = TRUE)

out$qua

## ------------------------------------------------------------------------
ppy <- with(xd, npeak[1]/nyear[1])
xp <- xd$peaks

set.seed(1)
out <- FloodnetPool(xp, target = mystation, distr = 'gpa', tol.H = Inf,
                                        period = 100 * ppy, verbose = FALSE, corr = .5)

## ------------------------------------------------------------------------
set.seed(1)
FloodnetPoolMle(xd, target = mystation, period = 100, nsim = 500, type = 'shape')

## ------------------------------------------------------------------------
xd <- with(descriptors,
  data.frame(
  	site = station,
    area = log(area),
    map  = log(map_ws),
    wb   = log(.01 + wb),
    stream = log(.01 + stream),
  	elev = elev_ws,
  	slope = log(.01 + slope)))

coord <- descriptors[, c('lon', 'lat')]


## ------------------------------------------------------------------------
target.id <- (xd$site == '01AF009')

target <- xd[target.id,]
xd <- xd[-target.id,]

## ------------------------------------------------------------------------
set.seed(1)
out <- FloodnetRoi(target = target, sites = xd,
                        db = db, period = 100, size = seq(25, 200, 10), 
                        nsim = 0, verbose = FALSE, out.model = TRUE)

## ---- echo = FALSE, fig.height = 4, fig.width = 6------------------------
plot(out$cv, 'mad', ylab = 'Mean Absolute Deviation (MAD)', 
		 main = 'Cross-validation')

## ------------------------------------------------------------------------
set.seed(1)
FloodnetRoi(target = target, sites = xd, db = db, 
						period = 100, size = 85, nsim = 100)

## ------------------------------------------------------------------------
## Extract the coordinates
coord <- descriptors[, c('lon', 'lat')]
coord <- as.data.frame(cmdscale(GeoDist(coord), 2))

target.coord <- coord[target.id,]
coord <- coord[-target.id,]

## ------------------------------------------------------------------------
set.seed(1)
out <- FloodnetRoi(target = target, sites = xd,
                        target.coord = target.coord, sites.coord = coord,
                        size = 209:211, db = db, period = 100, 
                        nsim = 0, verbose = FALSE, out.model = TRUE)

head(out$cv)

