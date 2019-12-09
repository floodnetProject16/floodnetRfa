## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

mycols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
						'#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

palette(mycols)

## ------------------------------------------------------------------------
library(floodnetProject16)
library(CSHShydRology)

## Station of interest and path to HYDAT
station.number <- '01AF009'
db <- 'inst/extdata/Hydat.sqlite3' 

