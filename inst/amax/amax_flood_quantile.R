library(floodnetProject16)
library(foreach)
library(doParallel)

rm(list = ls())

## Path of the HYDAT database
HYDAT <- "inst/extdata/Hydat.sqlite3"

## Size of the bootstrap sample
NSIM <- 2000

RES <- 560

## Number of parallel processus
NCPU <- 4

## Temporary files used during parallel computing
CACHE <- 'C:/Users/mdurocher/Downloads/cache/cache_amax'

## Filename for monitoring the computation
PROGRESS <- 'progress.log'

## Final output file with the flood quantiles
OUTFILE <- 'inst/amax/amax_flood_quantile.csv'

################################################
## ## Perform the POT analysis for all sites
################################################

cache.file <- file.path(CACHE, PROGRESS)

## Load the list of stations to analyse
stations <- gaugedSites$station

## Monitoring file for parallel computing
t0 <- Sys.time()
write(format(t0), file = cache.file)

## Set-up parallel computing
cl <- makeCluster(NCPU)
registerDoParallel(cl)

statu <- foreach(ii = seq_along(stations),
                 .errorhandling = 'pass',
                 .packages = c('CSHShydRology', 'floodnetProject16')) %dopar%{

	site <- stations[ii]

	write(paste0('(', ii ,') Analyzing using AMAX ', site),
	      file = cache.file, append = TRUE)

	pool <- try(FloodnetPoolAmax(target = site, db = HYDAT, verbose = FALSE,
													 out.model = TRUE))

	if(class(pool) == 'try-error')
		return('Exit with error - RFA')

	reg.fit <- pool$fit
	reg.h <- reg.fit$stat[1]
	reg.nk <- nrow(reg.fit$lmom)

	pool <- pool$qua
	reg.hat <- with(pool, value[variable == 'quantile'])
	reg.ub <- with(pool, value[variable == 'upper'])
	reg.lb <- with(pool, value[variable == 'lower'])
	reg.distr <- toupper(as.character(pool[1,3]))

	##########################
	## Read data
	##########################

	con <- RSQLite::dbConnect(RSQLite::SQLite(), HYDAT)
	xd <- HYDAT::AnnualPeakData(con, get_flow = TRUE, site)
  RSQLite::dbDisconnect(con)

  xd <- xd[xd$peak == 'MAXIMUM', c(3,6)]
  xd <- na.omit(xd)
  nn <- nrow(xd)

  #############################
  ## Estimate flood quantiles
  #############################

	fit <- try(FitAmax(xd$value, varcov = FALSE, tol.gev = 2))

	if(class(fit) == 'try-error')
    return('Exit with error - at-site')

	## Estimate the flood quantiles
	rt <- c(2, 5, 10, 20, 50, 100)
	x0 <- 1-1/rt
	xat <- 1-1/c(1.01, rev(1+1/seq(2,20,.5)), 2^seq(1,10, .5))
	xat <- sort(unique(c(x0,xat)))

	qua <- predict(fit, xat, ci = 'boot', nsim = NSIM, out.matrix = TRUE)
	qboot <- qua$qua
	qua <- qua$pred
	qua$se <- apply(qboot,2,sd)
	qua0 <- qua[which(xat %in% x0), c(1,4,2,3)]

	####################
	## Return level plot
  ####################

	yl <- range(c(reg.lb, reg.ub, qua[,2], qua0[,3]))

	palette(c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))

	fig.file <- paste0(paste0(site, '.png'))
	png(file = file.path(CACHE, fig.file), height = RES, width = 2*RES)

	par(mfrow = c(1,2))

	## at-site
	m <- paste0(ii, ' - ', site)
	plot(fit, main = m, cex = .5, col.ci = 2, pch = 16, ylim = yl)
	lines(-log(-log(xat)), qua$lower, lty = 2, col = 2)
	lines(-log(-log(xat)), qua$upper, lty = 2, col = 2)

	zat <- -log(-log(1-1/c(2, 5, 10, 20, 50, 100)))

	points(zat, reg.hat, col = 1, pch = 15)
	arrows(x0 = zat, y0 = reg.lb, y1 = reg.ub, cex = .8, col = 1,
				 angle = 90, code = 3, length = .1,)

	lg <- c(
		paste0('At-site = ', toupper(fit$distr)),
	  paste0('Region = ', reg.distr),
	  paste0('Nb. obs = ', nn),
	  paste0('Nb. site = ', reg.nk),
	  paste0('H = ', round(reg.h,2)))

	legend('topleft', legend = lg)


	plot(reg.fit)

	dev.off()

	##############################
	## Quantile output
	##############################
	out <- data.frame(site, 'amax', 'gev', period = c(2,5,10,20,50,100), qua0)
	colnames(out) <- c('site', 'method', 'distribution', 'period',
										 'quantile', 'se', 'lower', 'upper')

	out <- tidyr::gather(out, variable, value, -(1:4))

	amax.file <- paste0(site, '.csv')
	write.csv(rbind(out, pool), file = file.path(CACHE, amax.file),
						row.names = FALSE)

	return('Exit normally')
}

stopCluster(cl)

write(format(t0-Sys.time()), file = cache.file, append = TRUE)

print(unlist(statu))

#################################################
## Combine the output of the parallel computing
#################################################

file.lst <- list.files(CACHE, pattern = '*.csv')
Fz <- function(z) read.csv(file = file.path(CACHE,z))
all.file <- lapply(file.lst,Fz)
flood.quantile <- do.call(rbind, all.file)

write.csv(flood.quantile, file = OUTFILE, row.names = FALSE)

## verify if some file site were not analyzed
gsite <- as.character(gaugedSites$station)
rsite <- as.character(unique(flood.quantile$site))
fail.id <- which( !(gsite %in% rsite) )

gaugedSites[fail.id, c(1,3:7)]
