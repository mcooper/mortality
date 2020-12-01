library(raster)

setwd('~/mortalityblob/mortalitycovars/')

for (i in 2000:2020){
  r <- stack(list.files(pattern=as.character(i)))
  
  r <- stackApply(r, indices=c(1, 2, 3, 4), fun=sum, na.rm=T)
  
  r <- aggregate(r, fact=10, fun=sum, na.rm=T)

}
