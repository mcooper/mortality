setwd('/home/mattcoop/mortalityblob/ewembi')

library(raster)
library(abind)
library(lubridate)
library(SPEI)

#Get reference to mask NAs
load('../scenarios_monthly/rcp26_GFDL-ESM2M_pr.RData')
ref <- t(pr[ , , 1])

pr <- list()
tasmax <- list()
tasmin <- list()

dates <- as.character(seq(ymd('1979-01-01'), ymd('2016-12-01'), by='month'))

for (d in dates){
  suff <- paste0(substr(d, 1, 7), '.tif')
  
  p <- as.matrix(raster(paste0('pr_', suff)))
  tmx <- as.matrix(raster(paste0('tasmax_', suff)))
  tmn <- as.matrix(raster(paste0('tasmin_', suff)))
  
  p[is.na(ref)] <- NA
  tmx[is.na(ref)] <- NA
  tmn[is.na(ref)] <- NA

  pr[[length(pr) + 1]] <- p
  tasmax[[length(tasmax) + 1]] <- tmx
  tasmin[[length(tasmin) + 1]] <- tmn
  
  print(d)
}

pr_a <- abind(pr, along = 3)
tasmax_a <- abind(tasmax, along = 3)
tasmin_a <- abind(tasmin, along = 3)

lat <- matrix(rep(seq(89.75, -89.75, length.out=360), 720), nrow = 360, ncol=720)

pet <- array(dim=c(360, 720, length(dates)))
for (r in 1:360){
  print(r)
  for (c in 1:720){
    if (is.na(ref[r, c])){
      next
    }
    
    pet[r, c, ] <- as.vector(hargreaves(Tmin=tasmin_a[r, c, ],
                                    Tmax=tasmax_a[r, c, ],
                                    lat = lat[r, c],
                                    Pre = pr_a[r, c, ]))
  }
}

ewembi_wb <- pr_a - pet

dimnames(ewembi_wb)[[3]] <- dates

save(ewembi_wb, file = '../ewembi_waterbalance.Rdata')

spei3 <- array(dim=c(360, 720, length(dates)))
dimnames(spei3)[[3]] <- dates

spei12 <- array(dim=c(360, 720, length(dates)))
dimnames(spei12)[[3]] <- dates

spei24 <- array(dim=c(360, 720, length(dates)))
dimnames(spei24)[[3]] <- dates

spei36 <- array(dim=c(360, 720, length(dates)))
dimnames(spei36)[[3]] <- dates

for (r in 1:360){
  print(r)
  for (c in 1:720){
    if (is.na(ewembi_wb[r, c, 1])){
	  next
    }
  
    spei3[r, c, ] <- as.vector(spei(ewembi_wb[r, c, ], scale=3)$fitted)
    spei12[r, c, ] <- as.vector(spei(ewembi_wb[r, c, ], scale=12)$fitted)
    spei24[r, c, ] <- as.vector(spei(ewembi_wb[r, c, ], scale=24)$fitted)
    spei36[r, c, ] <- as.vector(spei(ewembi_wb[r, c, ], scale=36)$fitted)
	
  }
}

save(spei3, file = '../spei/ewembi_spei3.Rdata')
save(spei12, file = '../spei/ewembi_spei36.Rdata')
save(spei24, file = '../spei/ewembi_spei36.Rdata')
save(spei36, file = '../spei/ewembi_spei36.Rdata')


