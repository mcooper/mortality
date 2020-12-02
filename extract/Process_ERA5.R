# First run 
# cdo -f nc copy era5.grib era5.nc

library(ncdf4)

era <- nc_open('~/mortalityblob/era5/era5.nc')

lat <- ncvar_get(era, varid="lat")
lon <- ncvar_get(era, varid="lon")
time <- ncvar_get(era, varid="time")
pet <- ncvar_get(era, varid="pev")
pcp <- ncvar_get(era, varid="tp")
