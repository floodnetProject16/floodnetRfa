print('Flood frequency analysis: Floodnet Project 1.6')

suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))

## Path of the HYDAT database
HYDAT <- "../extdata/Hydat.sqlite3"

## Load the list of stations to analyse
stations <- read.csv('station.txt', header = FALSE)[,1]

## Size of the bootstrap sample
NSIM <- 2000

## Number of parallel processes
NCPU <- 4

## Monitoring file for parallel computing
t0 <- Sys.time()
write(format(t0), file = "progress.txt")

## Set-up parallel computing
cl <- makeCluster(NCPU)
registerDoParallel(cl)

statu <- foreach(ii = seq_along(stations)) %dopar%{
	
	site <- stations[ii]
	
	####################
	## Carry out AMAX ##
	####################
	
	write(paste0('Analyzing using AMAX ', site), 
	      file = "progress.txt", append = TRUE)
	
	out <- floodnetProject16::FloodnetAmax(site = site, db = HYDAT, 
	                                      quiet = TRUE, nsim = NSIM)
	
	amax.file <- paste0('amax',site,'.csv')                              
	write.csv(out, file = file.path('cache', amax.file),
	          row.names = FALSE)
	
	###################
	## Carry out POT ##
	###################
	
	write(paste0('Analyzing using POT ', site), 
	      file = "progress.txt", append = TRUE)
	      
	out <- floodnetProject16::FloodnetPot(site = site, db = HYDAT, 
	                                      quiet = TRUE, nsim = NSIM)
	
	pot.file <- paste0('pot',site,'.csv')                                      
	write.csv(out, file = file.path('cache', pot.file), 
	          row.names = FALSE)
	
}

stopCluster(cl)

write(format(t0-Sys.time()), file = "progress.txt", append = TRUE)
