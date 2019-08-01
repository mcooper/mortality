setwd('/home/mattcoop/climatedisk2/climatedisk/scenarios')

library(ncdf4)
library(lubridate)
library(tidyverse)
library(abind)
library(foreach)
library(doParallel)

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

e <- expand.grid(list(rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp85'), model=c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')),
                 stringsAsFactors = F)

cl <- makeCluster(6, outfile = '')
registerDoParallel(cl)

foreach(i=c(3, 5:nrow(e)), .packages=c('ncdf4', 'lubridate', 'tidyverse', 'abind')) %dopar% {
  
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
    
    group <- as.character(floor_date(ymd(dimnames(dat)[[3]]), unit = 'months'))
    
    comb <- list()
    for (g in unique(group)){
      sel <- apply(dat[ , , group==g], MARGIN=c(1, 2), FUN=sum)
      comb[[length(comb) + 1]] <- sel
    }
    
    res <- abind(comb, along=3)
    
    dimnames(res)[[3]] <- unique(group)
    
    pr[[length(pr) + 1]] <- res
  }
  
  pr <- abind(pr, along = 3)
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
    
    group <- as.character(floor_date(ymd(dimnames(dat)[[3]]), unit = 'months'))
    
    comb <- list()
    for (g in unique(group)){
      sel <- apply(dat[ , , group==g], MARGIN=c(1, 2), FUN=mean)
      comb[[length(comb) + 1]] <- sel
    }
    
    res <- abind(comb, along=3)
    
    dimnames(res)[[3]] <- unique(group)
    
    tas[[length(tas) + 1]] <- res
  }
  
  tas <- abind(tas, along=3)
  save(tas, file=paste0('../scenarios_monthly/', rcp, '_', model, '_tas.RData'))
  system(paste0('~/telegram.sh "Done with ', rcp, '_', model, '_tas"'))
  rm(tas)
  
}
