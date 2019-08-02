setwd('/home/mattcoop/climatedisk2/climatedisk/scenarios_monthly')

library(tidyverse)
library(SPEI)
library(lubridate)
library(foreach)
library(doParallel)
library(abind)

#Historic Water Balance
load('../ewembi_waterbalance.Rdata')

#Latitude for PET calculation
lat <- matrix(rep(seq(89.75, -89.75, length.out=360), 720), nrow = 360, ncol=720)

e <- expand.grid(list(rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp85'), model=c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')),
                 stringsAsFactors = F)

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

foreach(i=10:nrow(e), .packages=c('SPEI', 'lubridate', 'tidyverse', 'abind')) %dopar% {
  
  load(paste0(e$rcp[i], '_', e$model[i], '_pr.RData'))
  load(paste0(e$rcp[i], '_', e$model[i], '_tas.RData'))
  
  #Get water balance
  wb <- array(dim=c(360, 720, dim(pr)[3]))
  for (r in 1:360){
    for (c in 1:720){
      if (is.na(tas[c, r, 1])){
        next
      }
      
      pet <- as.vector(thornthwaite(Tave=tas[c, r, ] - 273.15, #Note, tas and pr were rotate, so invert r and c indices
                                    lat = lat[r, c]))
      
      pet[is.nan(pet)] <- 0
      
      if(sum(is.na(pet)) > 0){
        break
      }
      
      wb[r, c, ] <- pr[c, r, ] - pet
    }
  }
  
  cat("Done with ET on", e$rcp[i], " for model ", e$model[i], '\n')
  
  dimnames(wb)[[3]] <- dimnames(pr)[[3]]
  
  comb <- abind(ewembi_wb[ , , as.character(seq(ymd('1979-01-01'), ymd('2005-12-01'), by='months'))], 
                wb[ , , as.character(seq(ymd('2006-01-01'), ymd('2099-12-01'), by='months'))], 
                along=3)
  
  spei3 <- array(dim=c(360, 720, dim(comb)[3]))
  dimnames(spei3)[[3]] <- dimnames(comb)[[3]]
  
  spei36 <- array(dim=c(360, 720, dim(comb)[3]))
  dimnames(spei36)[[3]] <- dimnames(comb)[[3]]
  
  for (r in 1:360){
    for (c in 1:720){
      if (is.na(comb[r, c, 1])){
        next
      }
      
      spei3[r, c, ] <- as.vector(spei(comb[r, c, ], scale=3)$fitted)
      spei36[r, c, ] <- as.vector(spei(comb[r, c, ], scale=36)$fitted)

    }
  }
  
  spei3 <- spei3[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-01'), by='months'))]
  spei36 <- spei36[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-01'), by='months'))]
  
  save(spei3,  file=paste0('/home/mattcoop/climatedisk2/climatedisk/spei/', e$rcp[i], "_", e$model[i], 'spei3.Rdata'))
  save(spei36, file=paste0('/home/mattcoop/climatedisk2/climatedisk/spei/', e$rcp[i], "_", e$model[i], 'spei36.Rdata'))
  
  cat("Done with SPEI on", e$rcp[i], " for model ", e$model[i], '\n')
  
}

system('telegram.sh "Done with SPEI Processing!"')


