#Get Wealth data if it is missing
wealth <- files$WI[files$BR==f]
if (wealth!=''){
  wi <- read_dta(wealth)
  
  #Determine:
  #whether whhid is made from 3 values (v001-v002-v003) or just two (v001-v002)
  #where the breakpoints betwen the concatenated values are
  index <- NULL
  zero_ix <- NULL
  for (i in 1:12){
    val <- sum(substr(wi$whhid, i, i)==' ')
    index <- c(index, val)
  }
  index <- c(index, -1)
  
  for (i in 2:12){
    if (index[i] == 0 & index[i + 1] != 0){
      zero_ix <- c(zero_ix, i)
    }
  }
  
  v001_tmp <- paste0('                   ', dat$v001)
  dat$whhid <- substr(v001_tmp, nchar(v001_tmp)- (zero_ix[1] - 1), nchar(v001_tmp))
  
  v002_tmp <- paste0('                ', dat$v002)
  dat$whhid <- paste0(dat$whhid, substr(v002_tmp, (nchar(v002_tmp) - (zero_ix[2] - zero_ix[1])) + 1, nchar(v002_tmp)))
  
  if (length(zero_ix)==3){
    v003_tmp <- paste0('                ', dat$v003)
    dat$whhid <- paste0(dat$whhid, substr(v003_tmp, (nchar(v003_tmp) - (zero_ix[3] - zero_ix[2])) + 1, nchar(v003_tmp)))
  }
  
  if (names(table(nchar(dat$whhid))) != "12"){
    cat('Variable whhid lengths in ', wealth, '\n')
  }
  
  names(wi) <- c('whhid', 'v191', 'v190')
  
  wi$v191 <- as.character(wi$v191)
  
  wi$v190_int <- as.integer(wi$v190)
  wi$v190_chr <- as.character(as_factor(wi$v190))
  wi$v190 <- NULL
  
  if (sum(!dat$whhid %in% wi$whhid) > 0){
    #Check again for matching, if not, cat error
    cat('Mismatches in wealth data for ', wealth, ' - ', sum(!dat$whhid %in% wi$whhid), 'mismatches\n') 
  }
  
  dat <- merge(dat, wi, all.x=T, all.y=F)
}
