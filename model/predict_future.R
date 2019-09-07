library(mgcv)
library(abind)

#Load model
load('/home/mattcoop/mortalityblob/mod-results/mod_gam_spei_gdp_speifilter_nocovars.Rdata')

#Based on Figure 2 Here: https://www.geosci-model-dev.net/9/3461/2016/gmd-9-3461-2016.pdf
ssp_rcp <- data.frame(ssp=c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5'),
                      rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp60', 'rcp85'),
                      stringsAsFactors = F)

for (i in 4:nrow(ssp_rcp)){
  ssp <- ssp_rcp$ssp[i]
  rcp <- ssp_rcp$rcp[i]
  
  print(ssp)
  
  load(paste0('/home/mattcoop/climatedisk2/climatedisk/gdp/', toupper(ssp), '_GDP.RData'))
  
  gdp <- log(60000) - log(gdp)
  
  gdp[gdp < 0] <- 0
  
  gdp <- round(gdp, 1)
  
  reslist <- list()
  for (model in c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')){
    print(model)
    
    load(paste0('/home/mattcoop/climatedisk2/climatedisk/spei/', rcp, '_', model, 'spei3.Rdata'))
    load(paste0('/home/mattcoop/climatedisk2/climatedisk/spei/', rcp, '_', model, 'spei36.Rdata'))
    
    spei3[spei3 > 10] <- 10
    spei3[spei3 < -10] <- -10
    
    spei36[spei36 > 10] <- 10
    spei36[spei36 < -10] <- -10
    
    spei3 <- round(spei3, 1)
    spei36 <- round(spei36, 1)
    
    res <- array(predict(mod, list(GDP=gdp, spei3=spei3, spei36=spei36)), dim=dim(spei3))
    
    reslist[[model]] <- res

  }
  
  final <- reslist[[1]] + reslist[[2]] + reslist[[3]] + reslist[[4]]
  
  final <- final/4
  
  save(final, file=paste0('/home/mattcoop/climatedisk2/climatedisk/pred/', ssp, '_', rcp, '_pred.Rdata'))
}
