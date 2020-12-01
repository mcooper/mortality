library(tidyverse)
library(raster)
library(countrycode)

setwd('~/mortalityblob/mortality-dhs/')

geo <- read.csv('Mortality_geodata.csv') %>%
  filter(countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa',
         !(latitude < 0.1 & latitude > -0.1 & longitude < 0.1 & longitude > -0.1)) %>%
  dplyr::select(latitude, longitude, code)

sp <- SpatialPointsDataFrame(coords=geo[ , c('longitude', 'latitude')], 
                              data=geo)

aez <- raster('spatial/AEZ_DHS.tif')  %>%
  crop(extent(-18, 53, -36, 25))

Mode <- function(x, na.rm=T) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sp@data$aez <- NA

while(sum(is.na(sp@data$aez)) > 0){
  aez <- focal(aez, w=matrix(rep(1, 25), ncol=5), fun=Mode, na.rm=T, NAonly=T)
  sp@data$aez <- raster::extract(aez, sp)
}

geo <- sp@data

geo$aez[geo$aez == 0] <- "Mediterranean"
geo$aez[geo$aez == 1] <- "Desert"
geo$aez[geo$aez == 2] <- "Desert"      
geo$aez[geo$aez == 3] <- "Desert"      
geo$aez[geo$aez == 4] <- "Forest"      
geo$aez[geo$aez == 5] <- "Savanna"     
geo$aez[geo$aez == 6] <- "Savanna"     
geo$aez[geo$aez == 7] <- "Highlands"   
geo$aez[geo$aez == 8] <- "SemiForest"  
geo$aez[geo$aez == 9] <- "SemiForest"

write.csv(geo, 'Mortality_AEZs.csv', row.names=F)
