# First run 
# cdo -f nc copy era5.grib era5.nc

library(ncdf4)
library(raster)
library(lubridate)

##########################################
# First Export: Precip & PET
##########################################

era <- nc_open('~/mortalityblob/era5/era5.nc')

lat <- ncvar_get(era, varid="lat")
lon <- ncvar_get(era, varid="lon")
time <- ncvar_get(era, varid="time")
pet <- ncvar_get(era, varid="pev")

for (t in time){
  sel <- t(pet[ , , which(time == t)])

  n <- cbind(sel[ , 1801:3600], sel[, 1:1800])

  date <- ymd_hms('1981-1-1 00:00:00') + hours(t)

  print(date)

  writeRaster(raster(n, xmx=180, xmn=-180, ymx=90, ymn=-90),
              filename=paste0('~/mortalityblob/era5/pet', substr(date, 1, 7), '.tif'),
              format='GTiff')
}

rm(pet)

ppt <- ncvar_get(era, varid="tp")

for (t in time){
  sel <- t(ppt[ , , which(time == t)])

  n <- cbind(sel[ , 1801:3600], sel[, 1:1800])

  date <- ymd_hms('1981-1-1 00:00:00') + hours(t)

  print(date)

  writeRaster(raster(n, xmx=180, xmn=-180, ymx=90, ymn=-90),
              filename=paste0('~/mortalityblob/era5/ppt', substr(date, 1, 7), '.tif'),
              format='GTiff')
}

rm(ls())


##########################################
# Second Extract: Precipitation
##########################################

era <- nc_open('~/mortalityblob/era5/temps.nc')

lat <- ncvar_get(era, varid="lat")
lon <- ncvar_get(era, varid="lon")
time <- ncvar_get(era, varid="time")

#Dew point temp
dpt <- ncvar_get(era, varid="2d")
for (t in time){
  sel <- t(dpt[ , , which(time == t)])

  n <- cbind(sel[ , 1801:3600], sel[, 1:1800])

  date <- ymd_hms('1981-1-1 00:00:00') + hours(t)

  print(date)

  writeRaster(raster(n, xmx=180, xmn=-180, ymx=90, ymn=-90),
              filename=paste0('~/mortalityblob/era5/dpt', substr(date, 1, 7), '.tif'),
              format='GTiff')
}

rm(dpt)

#Skin Temp
skt <- ncvar_get(era, varid="skt")
for (t in time){
  sel <- t(skt[ , , which(time == t)])

  n <- cbind(sel[ , 1801:3600], sel[, 1:1800])

  date <- ymd_hms('1981-1-1 00:00:00') + hours(t)

  print(date)

  writeRaster(raster(n, xmx=180, xmn=-180, ymx=90, ymn=-90),
              filename=paste0('~/mortalityblob/era5/skt', substr(date, 1, 7), '.tif'),
              format='GTiff')
}

rm(skt)

#Temperature
tmp <- ncvar_get(era, varid="2t")
for (t in time){
  sel <- t(tmp[ , , which(time == t)])

  n <- cbind(sel[ , 1801:3600], sel[, 1:1800])

  date <- ymd_hms('1981-1-1 00:00:00') + hours(t)

  print(date)

  writeRaster(raster(n, xmx=180, xmn=-180, ymx=90, ymn=-90),
              filename=paste0('~/mortalityblob/era5/tmp', substr(date, 1, 7), '.tif'),
              format='GTiff')
}

