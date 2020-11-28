library(tidyverse)
library(raster)
library(countrycode)
library(rgdal)

setwd('~/mortalityblob/mortality-dhs/')

geo <- read.csv('Mortality_geodata.csv') %>%
  filter(countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa',
         !(latitude < 0.1 & latitude > -0.1 & longitude < 0.1 & longitude > -0.1)) %>%
  dplyr::select(-X) %>%
  mutate(earliest_year = 1900 + floor((earliest_date - 1)/12),
         latest_year = 1900 + floor((latest_date - 1)/12))

shdi <- read.csv('SHDI Complete 4.0 (1).csv')

gdlsp <- readOGR('.', 'GDL Shapefiles V4') %>%
  crop(extent(-18, 53, -36, 25))

sp <- SpatialPointsDataFrame(coords=geo[ , c('longitude', 'latitude')], 
                              data=geo)

crs(sp) <- crs(gdlsp)

sp$GDLCODE <- over(sp, gdlsp)$GDLcode

final <- data.frame()
for (y in 1983:2018){
  sely <- y
  if (y < 1990){
    sely <- 1990
  }

  shdi_sel <- shdi %>%
    filter(year == sely) %>%
    dplyr::select(GDLCODE, shdi, gnic)
  
  sp_sel <- sp@data %>% 
    filter(earliest_year <= y & latest_year <=y)

  sp_sel <- merge(sp_sel, shdi_sel, all.x=T, all.y=F)






}


