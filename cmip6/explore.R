#########################################
# Downloaded from https://esgf-node.llnl.gov/search/cmip6/
#########################################
library(ncdf4)
library(lubridate)
library(raster)
setwd('~/mortalityblob/cmip6/')

fs <- list.files()

f <- fs[length(fs)-1]

n <- nc_open(f)
lon <- ncvar_get(n, 'lon')
lat <- ncvar_get(n, 'lat')
tim <- ncvar_get(n, 'time')

start <- ymd(paste0(substr(f, 47, 50), '-', substr(f, 51, 52), '-01'))
end <- ymd(paste0(substr(f, 54, 57), '-', substr(f, 58, 59), '-01'))

months <- seq(start, end, 'month')

pr <- ncvar_get(n, 'pr')

plot(t(raster(pr[c(73:144, 1:72), , months==ymd('2005-08-01')])))

r <- raster('../chirps/chirps-v2.0.2005.08.tif')

r[r < 0] <- NA

plot(r)
