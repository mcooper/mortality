library(tidyverse)
library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

# Command Line Preparation
# sudo mkdir /mnt/TerraClimate
# sudo chown mattcoop /mnt/TerraClimate
# sudo mkdir /mnt/SPEIres
# sudo chown mattcoop /mnt/SPEIres
##rsync -avvvhW --no-compress --progress --include=*tif --exclude=*nc ~/mortalityblob/TerraClimate/ /mnt/TerraClimate

setwd('/mnt/TerraClimate')

dat <- read.csv('~/mortalityblob/mortality-dhs/Mortality_geodata.csv') %>%
  filter(!(latitude==0 & longitude==0))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('TerraClimate_ppt_1959.01.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[is.na(r)] <- NA

sp@data$tmpcode <- extract(codes, sp)

rll <- sp@data %>% group_by(tmpcode) %>%
  summarize(x=mean(longitude),
            y=mean(latitude)) 	

############################
#   METRICS #
# SPEI (ppt, pet)
# TMAXZ (tmax)
# TAVEZ (tmax, tmin)
# WBGT (t*, RH (t*, vpd), srad, ws)
#


#Read in ppt data
ppt_vrt_file <- extension(rasterTmpFile(), 'ivrt')
ppt_files <- list.files('.', pattern='^TerraClimate_ppt_.*tif$')
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

#Read in vpd data
vpd_vrt_file <- extension(rasterTmpFile(), 'ivrt')
vpd_files <- list.files('.', pattern='^TerraClimate_vpd_.*tif$')
gdalbuildvrt(paste0('./', vpd_files), vpd_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in srad data
srad_vrt_file <- extension(rasterTmpFile(), 'ivrt')
srad_files <- list.files('.', pattern='^TerraClimate_srad_.*tif$')
gdalbuildvrt(paste0('./', srad_files), srad_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in ws data
ws_vrt_file <- extension(rasterTmpFile(), 'ivrt')
ws_files <- list.files('.', pattern='^TerraClimate_ws_.*tif$')
gdalbuildvrt(paste0('./', ws_files), ws_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

getRH <- function(t, vpd){
  #https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit 
  #The second answer worked better
  #"From Dennis Hartman "Global Physical Climatology" (p 350)"
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + t)))
  rh <- 100 - 100*(vpd/es)
  rh
}

extract <- function(vrt, x, y){
  dat <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  return(dat)
}

getTempZ <- function(temp){
	month_means <- apply(matrix(temp, ncol=12, byrow=T), MARGIN=2, FUN=mean, na.rm=T)
	month_sd <- apply(matrix(temp, ncol=12, byrow=T), MARGIN=2, FUN=sd, na.rm=T)
	(temp - month_means)/month_sd
}

getWetBulbGlobeTemp <- function(temp, rh, srad, ws){
  #Formula from Ono and Tonouchi
  #https://doi.org/10.1016/j.envres.2018.09.032
  0.735*temp + 0.0374*rh + 0.00292*temp*rh + 7.619*srad - 4.557*srad^2 - 0.0572*ws - 4.064
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
  vpd <- extract(vpd_vrt_file, rll$x[n], rll$y[n])
  srad <- extract(srad_vrt_file, rll$x[n], rll$y[n])
  ws <- extract(ws_vrt_file, rll$x[n], rll$y[n])
  
  s <- ppt - pet
  
  #Get SPEI values
  spei1 <- as.numeric(spei(s, 1, na.rm=TRUE)$fitted)
  spei2 <- as.numeric(spei(s, 2, na.rm=TRUE)$fitted)
  spei3 <- as.numeric(spei(s, 3, na.rm=TRUE)$fitted)
  spei6 <- as.numeric(spei(s, 6, na.rm=TRUE)$fitted)
  spei12 <- as.numeric(spei(s, 12, na.rm=TRUE)$fitted)
  spei24 <- as.numeric(spei(s, 24, na.rm=TRUE)$fitted)
  spei36 <- as.numeric(spei(s, 36, na.rm=TRUE)$fitted)
  spei48 <- as.numeric(spei(s, 48, na.rm=TRUE)$fitted)

  #Get max and mean max temps
  tmax1 <- tmax
  tmax2 <- rollmean(tmax, k=2, fill=NA, na.rm=T, align='right')
  tmax3 <- rollmean(tmax, k=3, fill=NA, na.rm=T, align='right')

  #Get mean ave temps 
  tave1 <- (tmax - tmin)/2
  tave2 <- rollmean(tave1, k=2, fill=NA, na.rm=T, align='right')
  tave3 <- rollmean(tave1, k=3, fill=NA, na.rm=T, align='right')

  #Get Web Bulb Globe Temps for Max temps
  rh <- getRH(tmax1, vpd)
  wbgt1 <- getWetBulbGlobeTemp(tmax1, rh, srad/1000, ws)
  wbgt2 <- rollmean(wbgt1, k=2, fill=NA, na.rm=T, align='right')
  wbgt3 <- rollmean(wbgt1, k=3, fill=NA, na.rm=T, align='right')

  #Get temp Z Scores
  tmaxZ1 <- getTempZ(tmax1)
  tmaxZ2 <- getTempZ(tmax2)
  tmaxZ3 <- getTempZ(tmax3)
  taveZ1 <- getTempZ(tave1)
  taveZ2 <- getTempZ(tave2)
  taveZ3 <- getTempZ(tave3)
  wbgtZ1 <- getTempZ(wbgt1)
  wbgtZ2 <- getTempZ(wbgt2)
  wbgtZ3 <- getTempZ(wbgt3)

  #Long Term Norms
  mean_annual_precip <- mean(ppt)*12
  mean_tmin <- mean(tmin)
  mean_tmax <- mean(tmax)
  
  interview <- data.frame(date_cmc=seq(697, 1428), mean_annual_precip, mean_tmin, mean_tmax,
                          spei1, spei2, spei3, spei6, spei12, spei24, spei36, spei48, tmax1, tmax2,
                          tmax3, tave1, tave2, tave3, wbgt1, wbgt2, wbgt3, tmaxZ1, tmaxZ2,
                          tmaxZ3, taveZ1, taveZ2, taveZ3, wbgtZ1, wbgtZ2, wbgtZ3)
  
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
write.csv(precip, '~/mortalityblob/mortality-dhs/Mortality_SPI_Temps_TerraClimate.csv', row.names=F)

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')



