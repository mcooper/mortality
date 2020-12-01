library(raster)
library(doParallel)

setwd('~/mortalityblob/mortalitycovars/')

rasterOptions(tmpdir='/mnt/rastertmp')

cl <- makeCluster(20, outfile='')
registerDoParallel(cl)


foreach(i=2000:2020, .packages=c('raster')){
  r1 <- raster(paste0('global_f_0_', i, '_1km.tif'))
  r2 <- raster(paste0('global_f_1_', i, '_1km.tif'))
  r3 <- raster(paste0('global_m_0_', i, '_1km.tif'))
  r4 <- raster(paste0('global_m_1_', i, '_1km.tif'))
  
  r <- r1 + r2 + r3 + r4
  
  r <- aggregate(r, fact=20, fun=sum, na.rm=T)

  writeRaster(r, filename=paste0('population_', i, '.tif'), format='GTiff')
}

system('~/telegram.sh "Done with Tmp Rasters"')
