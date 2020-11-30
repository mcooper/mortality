library(tidyverse)
library(raster)
library(countrycode)
library(rgdal)
library(rgeos)

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

spgood <- sp[!is.na(sp$GDLCODE), ]
spbad <- sp[is.na(sp$GDLCODE), ]

res <- gDistance(spbad, gdlsp, byid=TRUE)
spbad$GDLCODE <- gdlsp$GDLcode[apply(res, MARGIN=2, FUN=which.min)]

sp <- bind_rows(spgood@data, spbad@data)
sp <- SpatialPointsDataFrame(coords = sp[ , c('longitude', 'latitude')],
                             data = sp)

final <- data.frame()
for (y in 1983:2018){
  sely <- y
  if (y < 1990){
    sely <- 1990
  }

  shdi_sel <- shdi %>%
    filter(year == sely) %>%
    dplyr::select(GDLCODE, shdi, healthindex, incindex, edindex)
  
  sp_sel <- sp@data %>% 
    filter(earliest_year <= y & latest_year >=y) %>%
    dplyr::select(latitude, longitude, code, GDLCODE) %>%
    mutate(year=y)

  sp_sel <- merge(sp_sel, shdi_sel, all.x=T, all.y=F)

  final <- bind_rows(final, sp_sel)
}

#For Those with no data, use HDI from Kummu Et Al.
goodfinal <- final %>% filter(!is.na(shdi))
badfinal <- final %>% filter(is.na(shdi))
badfinalsp <- SpatialPointsDataFrame(coords = badfinal[ , c('longitude', 'latitude')],
                                     data = badfinal)

hdi <- stack('HDI_1990_2015_v2.nc')
Mode <- function(x, na.rm=T) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (y in 1990:2015){
  print(y)
  hdir <- hdi[[paste0('X', y)]] %>%
    crop(extent(-18, 53, -36, 25))

  
  if (y == 1990){
    bfsel <- badfinalsp[badfinalsp$year <= 1990, ]
  }else{
    bfsel <- badfinalsp[badfinalsp$year == y, ]
  }

  while (sum(is.na(bfsel$shdi)) > 0){
    bfsel$shdi <- extract(hdir, bfsel)
    hdir <- focal(hdir, w=matrix(rep(1, 25), ncol=5), fun=Mode, na.rm=T, NAonly=T)
  }

  bfsel$healthindex <- bfsel$shdi
  bfsel$incindex <- bfsel$shdi
  bfsel$edindex <- bfsel$shdi

  goodfinal <- bind_rows(goodfinal, bfsel@data)
}


write.csv(goodfinal %>% dplyr::select(-GDLCODE, latitude, longitude),
          'Mortality_HDI.csv', row.names=F)











