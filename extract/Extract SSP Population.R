setwd('G://My Drive/SSPs/Population/')

library(raster)
library(lubridate)
library(abind)
library(zoo)

fs <- list.files(pattern='.tif$', recursive=T)

ref <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrow=360, ncol=720)

getMatrix <- function(ssp, year){
  dat <- raster(paste0(toupper(ssp), '/Total/GeoTIFF/', ssp, '_', year, '.tif'))
  ext <- extend(dat, ref)
  ag <- as.matrix(aggregate(ext, fact=4, fun=sum))
  ag[is.nan(ag)] <- 0
  
  ag
}

for (ssp in c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5')){
  alldata <- list()
  for (year in c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090)){
    mat <- getMatrix(ssp, year)
    
    alldata[[length(alldata) + 1]] <- mat
    for (i in 1:(12*10 - 1)){
      alldata[[length(alldata) + 1]] <- matrix(nrow=360, ncol=720)
    }
  }
  alldata[[length(alldata) + 1]] <- getMatrix(ssp, 2100)
  
  population <- abind(alldata, along = 3)
  
  #For loops are sloppy, but apply() rotates the matrix?
  for (i in 1:360){
    for (j in 1:720){
      population[i, j, ] <- na.approx(population[i, j, ])
    }
  }
  
  save(population, file = paste0('G://My Drive/SSPs/Population/', toupper(ssp), '_POP.Rdata'))
  
  print(ssp)
}



