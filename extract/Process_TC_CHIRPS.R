# Command Line Preparation
# sudo mkdir /mnt/TerraClimate
# sudo chown mattcoop /mnt/TerraClimate
# sudo mkdir /mnt/SPEIres
# sudo chown mattcoop /mnt/SPEIres

# # Then generate a download list in R
# tc <- expand.grid(list(var=c('pet', 'tmax', 'tmin'),
#                        m=substr(101:112, 2, 3),
#                        y=1981:2018))
# tc$url <- paste0('https://restart001.blob.core.windows.net/mortalityblob/TerraClimate/TerraClimate_', tc$var, '_', tc$y, '.', tc$m, '.01.tif')
# ch <- expand.grid(list(var=c('ppt'),
#                        m=substr(101:112, 2, 3),
#                        y=1981:2019))
# ch$url <- paste0('https://restart001.blob.core.windows.net/mortalityblob/chirps/chirps-v2.0.', ch$y, '.', ch$m, '.tif')
# comb <- rbind(ch, tc)
# cat(comb$url, sep='\n', file='~/comb_download')
 
# # Then download all of the tifs in the file
# cd /mnt/TerraClimate
# wget -i /home/mattcoop/comb_download
# 
# # If it stops
# fs <- list.files('/mnt/TerraClimate')
# df <- df[!basename(df$url) %in% fs, ]
# cat(df$url, sep='\n', file='/home/mattcoop/comb_download')
# 
# wget -i /home/mattcoop/comb_download

library(tidyverse)
library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/mnt/TerraClimate')

dat <- read.csv('~/mortalityblob/mortality-dhs/Mortality_geodata.csv') %>%
  filter(!(latitude==0 & longitude==0))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

######################################################
# Alias points with codes
# Not sure how to do this with TerraClimate and CHIRPS
# It looks like TerraClimate is *generally* more inland than chirps, so index to that.
##################################################
r <- raster('TerraClimate_tmax_1981.01.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[is.na(r)] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- sp %>% group_by(tmpcode) %>%
  summarize(x=mean(longitude),
            y=mean(latitude)) 	

############################
#   METRICS #
# SPEI (ppt, pet)
#


#Read in CHIRPS data
ppt_vrt_file <- extension(rasterTmpFile(), 'ivrt')
ppt_files <- list.files('.', pattern='^chirps.*$')
gdalbuildvrt(paste0('./', ppt_files), ppt_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in pet data
pet_vrt_file <- extension(rasterTmpFile(), 'ivrt')
pet_files <- list.files('.', pattern='^TerraClimate_pet_.*tif$')
gdalbuildvrt(paste0('./', pet_files), pet_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in tmax data
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files('.', pattern='^TerraClimate_tmax_.*tif$')
gdalbuildvrt(paste0('./', tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in tmin data
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files('.', pattern='^TerraClimate_tmin_.*tif$')
gdalbuildvrt(paste0('./', tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

extract <- function(vrt, x, y){
  dat <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  return(dat)
}

cores <- detectCores()
cl <- makeCluster(cores, outfile = '')
registerDoParallel(cl)

foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
  #Extract all data
  ppt <- extract(ppt_vrt_file, rll$x[n], rll$y[n])
  tmax <- extract(tmax_vrt_file, rll$x[n], rll$y[n])
  tmin <- extract(tmin_vrt_file, rll$x[n], rll$y[n])
  pet <- extract(pet_vrt_file, rll$x[n], rll$y[n])

  if(any(ppt < -1)){
    return(NULL)
  }
  
  s.tc <- ppt - pet
  s.mm <- ppt - as.vector(hargreaves(tmin, tmax, lat=rll$y[n], Pre=ppt)) 

  #get spei values
  spei.tc.1 <- as.numeric(spei(s.tc, 1, na.rm=TRUE)$fitted)
  spei.tc.2 <- as.numeric(spei(s.tc, 2, na.rm=TRUE)$fitted)
  spei.tc.3 <- as.numeric(spei(s.tc, 3, na.rm=TRUE)$fitted)
  spei.tc.6 <- as.numeric(spei(s.tc, 6, na.rm=TRUE)$fitted)
  spei.tc.12 <- as.numeric(spei(s.tc, 12, na.rm=TRUE)$fitted)
  spei.tc.24 <- as.numeric(spei(s.tc, 24, na.rm=TRUE)$fitted)
  spei.tc.36 <- as.numeric(spei(s.tc, 36, na.rm=TRUE)$fitted)
  spei.tc.48 <- as.numeric(spei(s.tc, 48, na.rm=TRUE)$fitted)
  
  #get spei values
  spei.mm.1 <- as.numeric(spei(s.mm, 1, na.rm=TRUE)$fitted)
  spei.mm.2 <- as.numeric(spei(s.mm, 2, na.rm=TRUE)$fitted)
  spei.mm.3 <- as.numeric(spei(s.mm, 3, na.rm=TRUE)$fitted)
  spei.mm.6 <- as.numeric(spei(s.mm, 6, na.rm=TRUE)$fitted)
  spei.mm.12 <- as.numeric(spei(s.mm, 12, na.rm=TRUE)$fitted)
  spei.mm.24 <- as.numeric(spei(s.mm, 24, na.rm=TRUE)$fitted)
  spei.mm.36 <- as.numeric(spei(s.mm, 36, na.rm=TRUE)$fitted)
  spei.mm.48 <- as.numeric(spei(s.mm, 48, na.rm=TRUE)$fitted)
  
  interview <- data.frame(date_cmc=seq(973, 1428),
                          spei.tc.1, spei.tc.2, spei.tc.3, spei.tc.6,
                          spei.tc.12, spei.tc.24, spei.tc.36, spei.tc.48,  
                          spei.mm.1, spei.mm.2, spei.mm.3, spei.mm.6, 
                          spei.mm.12, spei.mm.24, spei.mm.36, spei.mm.48)

  spsel <- sp[which(sp$tmpcode==rll$tmpcode[n]), ]

  all <- data.frame()
  for (i in 1:nrow(spsel)){
    new <- data.frame(code=spsel$code[i], date_cmc=seq(spsel$earliest_date[i], spsel$latest_date[i]))
    new <- merge(new, interview, all.x=T, all.y=F)
    all <- bind_rows(all, new)
  }
  
  #Reduce data size by eliminating uncessary precision:
  all <- all %>%
    mutate_if(is.numeric, function(x){round(x, 2)})
  
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(all, paste0('/mnt/SPEIres/', n), row.names=F)
}

setwd('/mnt/SPEIres/')

precip <- list.files()%>%
  lapply(read.csv) %>%
  bind_rows

#Write
write_csv(precip, '~/mortalityblob/mortality-dhs/Mortality_SPI_TC_CHIRPS.csv')

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')



