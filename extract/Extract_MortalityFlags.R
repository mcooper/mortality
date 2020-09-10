setwd('~/mortalityblob/dhsraw')

library(tidyverse)
library(haven)
library(labelled)
library(foreign)

files <- read.csv('~/mortality/scope/UseFiles.csv', stringsAsFactors = F) %>%
  filter(GE != '' & BR != '')

fs <- files$BR

all <- data.frame()
for (f in fs){
  
  cat(round((which(f==fs)/length(fs))*100, 1), f, '\n')
  
  dat <- read_dta(f)
  
  cols <- c('bord', #Birth Order
            'b13',  #Death flag
            'v005', #sample weight
            'v001', #cluster number
            'v002', #household number
            'v003' #respondent line number
  )
  
  dat <- dat[ , cols[cols %in% names(dat)]]
  
  dat$bord <- if('bord' %in% names(dat)) as.integer(dat$bord)
  dat$b13 <- if('b13' %in% names(dat)) as.integer(dat$b13)
  dat$v001 <- if('v001' %in% names(dat)) as.character(dat$v001)
  dat$v002 <- if('v002' %in% names(dat)) as.character(dat$v002)
  dat$v003 <- if('v003' %in% names(dat)) as.character(dat$v003)
  
  if ('b13' %in% names(dat)){
    dat$b13_int <- as.integer(dat$b13)
    dat$b13_chr <- as.character(as_factor(dat$b13))
    dat$b13 <- NULL
  }
  
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                           ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                                  ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                         ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4, 99))))
  
  dat$surveycode <- paste(cc, num, subversion, sep='-')
  dat$code <- do.call(paste, c(dat[ , c('surveycode', 'v001')], sep='-'))
  
  all <- bind_rows(all, dat)
}

##########################
#Recode Variables
###########################
all$birth_order <- all$bord
all$bord <- NULL

all$hh_code <- paste0(all$code, '-', all$v002)

all$resp_code <- paste0(all$hh_code, '-', all$v003)

all$v001 <- NULL
all$v002 <- NULL
all$v003 <- NULL

resp <- all %>%
  select(code, surveycode, resp_code) %>%
  unique

all$ind_code <- paste0(all$resp_code, '-', all$birth_order)

#Manually deal with duplicate individual codes
dups <- table(all$ind_code[duplicated(all$ind_code) | duplicated(all$ind_code, fromLast=T)])
for (i in 1:length(dups)){
  all$ind_code[all$ind_code==names(dups[i])] <- paste0(all$ind_code[all$ind_code==names(dups[i])], letters[1:dups[i]])
}

all <- all %>%
  select(ind_code, mortality_flag_int=b13_int, mortality_flag_chr=b13_chr) %>%
  filter(!is.na(mortality_flag_int) | !is.na(mortality_flag_chr)) %>%
  unique

write.csv(all, '~/mortalityblob/mortality-dhs/Mortality_individual_flags.csv', row.names=F)

