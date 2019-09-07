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
  for (time in as.character(seq(ymd('2010-01-01'), ymd('2100-12-01'), by='month'))){
    if (month(ymd(time))==6 & year(ymd(time)) %% 10 == 0){
      mat <- getMatrix(ssp, year)
    } else{
      matrix(nrow=360, ncol=720)
    }
    
    alldata[[length(alldata) + 1]] <- mat
  }
  
  pop <- abind(alldata, along = 3)
  
  #For loops are sloppy, but apply() rotates the matrix?
  for (i in 1:360){
    for (j in 1:720){
      pop[i, j, ] <- na.approx(pop[i, j, ])
    }
  }
  
  dimnames(pop)[[3]] <- as.character(seq(ymd('2010-01-01'), ymd('2100-12-31'), by='month'))
  
  pop <- pop[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-31'), by='month'))]
  
  save(pop, file = paste0('G://My Drive/SSPs/Population/', toupper(ssp), '_POP.Rdata'))
  
  print(ssp)
}



