setwd('/home/mattcoop/climatedisk2/climatedisk/scenarios')

library(ncdf4)
library(lubridate)
library(tidyverse)
library(abind)

options(stringsAsFactors=FALSE)

read_isimip <- function(file){
  
  filenamesplit <- strsplit(file, '/')[[1]]
  filename <- filenamesplit[length(filenamesplit)]
  
  parsed <- strsplit(filename, '_')[[1]]
  
  var <- parsed[1]
  
  if (grepl('ewembi', filename)){
    start <- ymd(paste0(parsed[length(parsed) - 1], '-01-01'))
    end <- ymd(paste0(gsub('.nc', '', parsed[length(parsed)]), '-12-31'))
  } else{
    dates <- strsplit(parsed[length(parsed)], '-')[[1]]
    start <- ymd(dates[1])
    end <- ymd(gsub('.nc4', '', dates[2]))
  }
  
  dat <- nc_open(file) %>%
    ncvar_get(var)
  
  dimnames(dat)[[3]] <- as.character(seq(start, end, by='day'))
  
  dat
}

e <- expand.grid(list(rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp85'), model=c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')))

for (i in 1:nrow(e)){
  
  rcp <- e$rcp[i]
  model <- e$model[i]
  
  pr <- list()
  for (f in list.files(path = paste0(rcp, '/', model), pattern='^pr', full.names = T)){
    print(f)
    
    dat <- read_isimip(f)
    
    #If there is ocean data (in MIROC5), mask it out
    if (!grepl('landonly', f)){
      r <- gsub('EWEMBI_20', 'EWEMBI_landonly_20', gsub('MIROC5', 'HadGEM2-ES', f))
      
      ref <- read_isimip(r)
      
      dat[is.na(ref)] <- NA
      
      rm(ref)
    }
    
    group <- as.factor(floor_date(ymd(dimnames(dat)[[3]]), unit = 'months'))
    
    res <- aperm(apply(dat, c(1,2), by, group, sum), c(2,3,1))
    
    pr[[length(pr) + 1]] <- res
  }
  
  pr <- Reduce(abind, x = pr)
  save(pr, file=paste0('../scenarios_monthly/', rcp, '_', model, '_pr.RData'))
  system(paste0('~/telegram.sh "Done with ', rcp, '_', model, '_pr"'))
  rm(pr)
  
  tas <- list()
  for (f in list.files(path = paste0(rcp, '/', model), pattern='^tas', full.names = T)){
    print(f)
    
    dat <- read_isimip(f)
    
    #If there is ocean data (in MIROC5), mask it out
    if (!grepl('landonly', f)){
      r <- gsub('EWEMBI_20', 'EWEMBI_landonly_20', gsub('MIROC5', 'HadGEM2-ES', f))
      
      ref <- read_isimip(r)
      
      dat[is.na(ref)] <- NA
      
      rm(ref)
    }
    
    group <- as.factor(floor_date(ymd(dimnames(dat)[[3]]), unit = 'months'))
    
    res <- aperm(apply(dat,c(1,2), by, group, mean), c(2,3,1))
    
    tas[[length(tas) + 1]] <- res
  }
  
  tas <- Reduce(abind, x = tas)
  save(tas, file=paste0('../scenarios_monthly/', rcp, '_', model, '_tas.RData'))
  system(paste0('~/telegram.sh "Done with ', rcp, '_', model, '_tas"'))
  rm(tas)
  
}
# 
# #An alternative way to aggregate, from data.table
# #Takes a lot more memory
# start <- Sys.time()
# dimnames(dat)[[3]] <- floor_date(ymd(dimnames(dat)[[3]]), unit = 'months')
# dimnames(dat)[[1]] <- 1:720
# dimnames(dat)[[2]] <- 1:360
# 
# dt <- as.data.table(dat)
# dt <- dt[ , sum(value), by=list(V1, V2, V3)]
# 
# res = array(dim = c(720, 360, length(unique(dimnames(dat)[[3]]))))
# invisible(dt[, res[V1, V2, V3] <<- value, .(V1,V2,V3)])
# end <- Sys.time()
# 
