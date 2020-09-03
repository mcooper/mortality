library(ncdf4)
library(tidyverse)
library(raster)
library(foreach)
library(doParallel)

setwd('/home/mattcoop/mortalityblob/TerraClimate')

#Already finished files
done_tiffs <- gsub('.\\d{2}.\\d{2}.tif', '', list.files(pattern='.tif')) %>%
  table %>%
  data.frame() %>%
  filter(Freq == 12) %>%
  .$.

#Get NCDFs
ncdfs <- list.files(pattern='.nc')

#Filter to NCDFs that were not in the already finished tiffs
ncdfs <- ncdfs[!gsub('.nc', '', ncdfs) %in% done_tiffs]

#Define conversion functions
convert_ncdf <- function(f){
  ncdf <- stack(f)

  for (i in names(ncdf)){
    sel <- ncdf[[i]]
    file <- paste0(gsub('\\d{4}.nc', '', f), gsub('X', '', i), '.tif')
    writeRaster(sel, file, format='GTiff')
  }
  print(f)
}

cl <- makeCluster(64, outfile = '')
registerDoParallel(cl)

for (f in ncdfs){
  convert_ncdf(f)
}


