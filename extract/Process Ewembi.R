setwd('/home/mattcoop/climatedisk')

library(ncdf4)
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)
library(doParallel)
library(foreach)

fs <- list.files(pattern='nc$')

cl <- makeCluster(8, outfile = '')
registerDoParallel(cl)

foreach(f=fs, .packages=c('ncdf4', 'raster', 'rgdal', 'lubridate')) %dopar% {
  var <- substr(f, 1, gregexpr('_', f)[[1]][1] - 1)
  start <- substr(f, nchar(f) - 11, nchar(f) - 8)
  end <- substr(f, nchar(f) - 6, nchar(f) - 3)
  
  dat <- nc_open(f)
  
  mat <- ncvar_get(dat, var)
  time <- ncvar_get(dat, 'time')

  range <- seq(ymd(paste0(start, '-01-01')), ymd(paste0(end, '-12-31')), by='day')
  
  if(length(time) != length(range)){
    stop("Bad time sequence")
  }
  
  for (y in seq(start, end)){
    for (m in seq(1, 12)){
      sel <- mat[ , , year(range)==y & month(range)==m]
      
      if (var == 'pr'){
        #Units is kg^2/m^2/s
        #Need to mulitply by 86400 before aggregating to get mm/day 
        res <- apply(sel*86400, MARGIN=c(1, 2), FUN=sum)
      } else{
        res <- apply(sel - 273.15, MARGIN=c(1, 2), FUN=mean)
      }
      
      r <- raster(t(res), xmn=-180, xmx=180, ymn=-90, ymx=90)
      
      file <- paste0(var, '_', y, '-', substr(100 + m, 2, 3))
      
      writeRaster(r, paste0('ewembi/', file, '.tif'), format='GTiff')
      
      print(file)
      
    }
  }
}

stopCluster(cl)


