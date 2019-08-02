setwd('/home/mattcoop/climatedisk2/climatedisk/ewembi')

library(raster)
library(abind)
library(lubridate)
library(SPEI)

#Get reference to mask NAs
load('../scenarios_monthly/rcp26_GFDL-ESM2M_pr.RData')
ref <- t(pr[ , , 1])

pr <- list()

dates <- as.character(seq(ymd('1979-01-01'), ymd('2016-12-01'), by='month'))

for (d in dates){
  suff <- paste0(substr(d, 1, 7), '.tif')
  
  p <- as.matrix(raster(paste0('pr_', suff)))
  
  p[is.na(ref)] <- NA

  pr[[length(pr) + 1]] <- p
  
  print(d)
}

pr_a <- abind(pr, along = 3)

dimnames(pr_a)[[3]] <- dates

load('../scenarios_monthly/rcp85_MIROC5_pr.RData')

inv_pr <- array(dim=c(360, 720, 1128))
dimnames(inv_pr)[[3]] <- dimnames(pr)[[3]]

for (r in 1:360){
  for (c in 1:720){
    inv_pr[r, c, ] <- pr[c, r, ]
  }
}
  
comb <- abind(pr_a[ , , as.character(seq(ymd('1979-01-01'), ymd('2005-12-01'), by='months'))], 
			  inv_pr[ , , as.character(seq(ymd('2006-01-01'), ymd('2099-12-01'), by='months'))], 
			along=3)

spi3 <- array(dim=c(360, 720, dim(comb)[3]))
dimnames(spi3)[[3]] <- dimnames(comb)[[3]]

spi36 <- array(dim=c(360, 720, dim(comb)[3]))
dimnames(spi36)[[3]] <- dimnames(comb)[[3]]

for (r in 1:360){
  for (c in 1:720){
    if (is.na(comb[r, c, 1])){
	  next
    }
  
    spi3[r, c, ] <- as.vector(spi(comb[r, c, ], scale=3)$fitted)
    spi36[r, c, ] <- as.vector(spi(comb[r, c, ], scale=36)$fitted)

  }
}

spi3 <- spi3[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-01'), by='months'))]
spi36 <- spi36[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-01'), by='months'))]

save(spi3,  file=paste0('/home/mattcoop/climatedisk2/climatedisk/spei/rcp85_MIROC5spi3.Rdata'))
save(spi36, file=paste0('/home/mattcoop/climatedisk2/climatedisk/spei/rcp85_MIROC5spi36.Rdata'))
