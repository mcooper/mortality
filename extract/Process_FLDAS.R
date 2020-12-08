library(ncdf4)
library(raster)
library(rgdal)

setwd('~/mortalityblob/fldas')

fs <- list.files(pattern='nc$')

for (f in fs[333:length(fs)]){
  print(f)
  y <- substr(f, 22, 25)
  m <- substr(f, 26, 27)
  d <- nc_open(f)

  #Get mean temp Tair_f_tavg
  t <- ncvar_get(d, 'Tair_f_tavg')
  r <- raster(apply(t(t), 2, rev), xmx=180, xmn=-180, ymx=90, ymn=-60) - 273.15 #convert K -> C
  writeRaster(r, filename=paste0('tavg_', y, m, '.tif'), format='GTiff')

  #Get total pcp Rainf_f_tavg
  t <- ncvar_get(d, 'Rainf_f_tavg')
  p <- raster(apply(t(t), 2, rev), xmx=180, xmn=-180, ymx=90, ymn=-60)
  p <- p*86400*30.4167 #conver kg/m2/s -> mm/month
  writeRaster(p, filename=paste0('prec_', y, m, '.tif'), format='GTiff')
}
