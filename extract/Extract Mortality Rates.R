setwd('~/climatedisk/DHS')

library(tidyverse)
library(haven)
library(labelled)
library(foreign)
library(foreach)
library(doParallel)

files <- read.csv('~/UseFiles.csv', stringsAsFactors = F) %>%
  filter(GE != '' & BR != '') %>%
  arrange(desc(WI))

fs <- files$BR

cl <- makeCluster(32, outfile = '')
Sys.sleep(60)
registerDoParallel(cl)

all <- foreach(f=fs, .packages=c('tidyverse', 'haven', 'labelled', 'foreign'), .combine=bind_rows) %dopar% {
  
  cat(round((which(f==fs)/length(fs))*100, 1), f, '\n')
  
  dat <- read_dta(f)
  
  cols <- c('bord', #Birth Order
            'b3',   #CMC of date of birth
            'b4',   #sex of child
            'b5',   #whether child was alive or dead at time of interview
            'b7',   #age at death
            'v106', #Highest education level
            'v107', #Highest year of education
            'v012', #Mother's age
            'v113', #Main source of drinking water
            'v116', #Type of toilet facility
            'v008', #CMC of date of interview
            'v005', #sample weight
            'v001', #cluster number
            'v002', #household number
            'v003', #respondent line number
            'v101', #Region of residence
            'v104', #number of years in location
            'v190', #Wealth index
            'v191'  #Wealth index factor score
  )
  
  dat <- dat[ , cols[cols %in% names(dat)]]
  
  dat$bord <- if('bord' %in% names(dat)) as.integer(dat$bord)
  dat$b3 <- if('b3' %in% names(dat)) as.integer(dat$b3)
  dat$b7 <- if('b7' %in% names(dat)) as.integer(dat$b7)
  dat$v107 <- if('v107' %in% names(dat)) as.integer(dat$v107)
  dat$v008 <- if('v008' %in% names(dat)) as.integer(dat$v008)
  dat$v005 <- if('v005' %in% names(dat)) as.integer(dat$v005)
  dat$v005 <- if('v012' %in% names(dat)) as.integer(dat$v012)
  dat$v001 <- if('v001' %in% names(dat)) as.character(dat$v001)
  dat$v002 <- if('v002' %in% names(dat)) as.character(dat$v002)
  dat$v003 <- if('v003' %in% names(dat)) as.character(dat$v003)
  dat$v104 <- if('v104' %in% names(dat)) as.character(dat$v104)
  dat$v191 <- if('v191' %in% names(dat)) as.character(dat$v191)
  
  if ('b4' %in% names(dat)){
    dat$b4_int <- as.integer(dat$b4)
    dat$b4_chr <- as.character(as_factor(dat$b4))
    dat$b4 <- NULL
  }
  
  if ('b5' %in% names(dat)){
    dat$b5_int <- as.integer(dat$b5)
    dat$b5_chr <- as.character(as_factor(dat$b5))
    dat$b5 <- NULL
  }
  
  if ('v113' %in% names(dat)){
    dat$v113_int <- as.integer(dat$v113)
    dat$v113_chr <- as.character(as_factor(dat$v113))
    dat$v113 <- NULL
  }
  
  if ('v116' %in% names(dat)){
    dat$v116_int <- as.integer(dat$v116)
    dat$v116_chr <- as.character(as_factor(dat$v116))
    dat$v116 <- NULL
  }
   
  if ('v101' %in% names(dat)){
    dat$v101_int <- as.integer(dat$v101)
    dat$v101_chr <- as.character(as_factor(dat$v101))
    dat$v101 <- NULL
  }
  
  if ('v190' %in% names(dat)){
    dat$v190_int <- as.integer(dat$v190)
    dat$v190_chr <- as.character(as_factor(dat$v190))
    dat$v190 <- NULL
  }
  
  if ('v106' %in% names(dat)){
    dat$v106_int <- as.integer(dat$v106)
    dat$v106_chr <- as.character(as_factor(dat$v106))
    dat$v106 <- NULL
  }
  
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                           ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                                  ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                         ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4, 99))))
  
  dat$surveycode <- paste(cc, num, subversion, sep='-')
  dat$code <- do.call(paste, c(dat[ , c('surveycode', 'v001')], sep='-'))
  
  #Now get spatial data
  spheadervars <- c('DHSCLUST', 'LATNUM', 'LONGNUM')
  
  spdat <- read.dbf(gsub('.shp', '.dbf', files$GE[files$BR==f]), as.is=TRUE)
  if (!all(spheadervars %in% names(spdat))){
    cat(files$GE[files$BR==f], 'is missing necessary column names\n')
  }else{
    spdat <- spdat[ , spheadervars]
    spdat$num <- substr(files$GE[files$BR==f], 5, 5)
    spdat$cc <- toupper(substr(files$GE[files$BR==f], 1, 2))
    spdat$subversion <- ifelse(toupper(substr(files$GE[files$BR==f], 6, 6)) %in% as.character(seq(0, 9)), 1,
                             ifelse(toupper(substr(files$GE[files$BR==f], 6, 6)) %in% LETTERS[1:8], 2, 
                                    ifelse(toupper(substr(files$GE[files$BR==f], 6, 6)) %in% LETTERS[9:17], 3, 
                                           ifelse(toupper(substr(files$GE[files$BR==f], 6, 6)) %in% LETTERS[18:26], 4, 99))))
    spdat$code <- paste(spdat$cc, spdat$num, spdat$subversion, spdat$DHSCLUST, sep='-')
    
    spdat <- spdat %>%
      select(latitude=LATNUM, longitude=LONGNUM, code=code)
  }
  
  initialsize <- nrow(dat)
  dat <- merge(dat, spdat)
  if (initialsize != nrow(dat)){
    cat("Mismatches in Spatial and Household data.  Initial size:", initialsize, " now:", nrow(dat), '\n')
  }
  
  dat
}

all <- all %>%
  mutate(country = substr(surveycode, 1, 2),
         b3 = ifelse(country=='NP', b3 - 681, b3),
         v008 = ifelse(country=='NP', v008 - 681, v008))

write.csv(all, '~/Mortality_raw.csv', row.names=F)

all$birth_order <- all$bord
all$bord <- NULL

all$birthdate_cmc <- all$b3
all$b3 <- NULL

all$male <- all$b4_int == 1
all$b4_int <- NULL
all$b4_chr <- NULL

all$alive <- all$b5_int == 1
all$b5_int <- NULL
all$b5_chr <- NULL

all$age_at_death <- all$b7
all$b7 <- NULL

all$mother_years_ed <- all$v107
all$mother_years_ed[all$mother_years_ed > 20] <- NA
all$mother_years_ed[is.na(all$v107) & all$v106_int==0] <- 0
all$v107 <- NULL
all$v106_int <- NULL
all$v106_chr <- NULL

all$mothers_age <- all$v012
all$v012 <- NULL

all$drinking_source_int <- all$v113_int
all$drinking_source_chr <- all$v113_chr
all$v113_int <- NULL
all$v113_chr <- NULL

all$toilet_type_int <- all$v116_int
all$toilet_type_chr <- all$v116_chr
all$v116_int <- NULL
all$v116_chr <- NULL

all$region_int <- all$v101_int
all$region_chr <- all$v101_chr
all$v101_int <- NULL
all$v101_chr <- NULL

all$interview_cmc <- all$v008
all$v008 <- NULL

all$sample_weight <- all$v005
all$v005 <- NULL

all$wealth_quintile <- all$v190_int
all$v190_int <- NULL
all$v190_chr <- NULL

all$wealth_factor <- all$v191
all$v191 <- NULL

all$years_in_loc <- as.integer(all$v104)
all$v104 <- NULL

all$age <- all$interview_cmc - all$birthdate_cmc

all$hh_code <- paste0(all$code, '-', all$v002)

all$resp_code <- paste0(all$hh_code, '-', all$v003)

all$v001 <- NULL
all$v002 <- NULL
all$v003 <- NULL

resp <- all %>%
  select(code, surveycode, latitude, longitude, country, drinking_source_int, drinking_source_chr,
         toilet_type_int, toilet_type_chr, region_int, region_chr, interview_cmc, sample_weight,
         wealth_quintile, wealth_factor, years_in_loc, resp_code) %>%
  unique

#There are some duplicated respondent codes in Niger, Mali, and Cameroon
#Manually split then
dups <- table(resp$resp_code[duplicated(resp$resp_code) | duplicated(resp$resp_code, fromLast=T)])
resp$resp_code_new <- resp$resp_code
for (i in 1:length(dups)){
  resp$resp_code_new[resp$resp_code==names(dups[i])] <- paste0(resp$resp_code[resp$resp_code==names(dups[i])], letters[1:dups[i]])
}

#Add back new uniqueified resp_code
all <- merge(all, resp, all.x=T, all.y=F)

all$resp_code <- all$resp_code_new
all$resp_code_new <- NULL

resp$resp_code <- resp$resp_code_new
resp$resp_code_new <- NULL

write.csv(resp, '~/Mortality_respondentdata.csv', row.names=F)

all$ind_code <- paste0(all$resp_code, '-', all$birth_order)

#Manually deal with duplicate individual codes
dups <- table(all$ind_code[duplicated(all$ind_code) | duplicated(all$ind_code, fromLast=T)])
for (i in 1:length(dups)){
  all$ind_code[all$ind_code==names(dups[i])] <- paste0(all$ind_code[all$ind_code==names(dups[i])], letters[1:dups[i]])
}

#remove fields that area already in respondent-level data
all <- all %>%
  select(-latitude, -longitude, -country, -drinking_source_int, -drinking_source_chr,
         -toilet_type_int, -toilet_type_chr, -region_int, -region_chr, -sample_weight,
         -wealth_quintile, -wealth_factor)

write.csv(all, '~/Mortality_individualdata.csv', row.names=F)

foreach(i=1:nrow(all), .packages=c('tidyverse')) %dopar% {
  
  cat(round(i/nrow(all)*100, 4), '\n')
  
  mothers_age <- ifelse(is.na(all$mothers_age[i]), NA, (all$mothers_age[i]*12):(all$mothers_age[i]*12 - all$age[i]))
  months_in_loc <- ifelse(is.na(all$years_in_loc[i]), NA, (all$years_in_loc[i]*12):(all$years_in_loc[i]*12 - all$age[i]))

  df <- data.frame(ind_code=all$ind_code[i],
                 date=all$interview_cmc[i]:all$birthdate_cmc[i],
                 age=all$age[i]:0,
                 mother_years_ed=all$mother_years_ed[i]*12,
                 mothers_age=mothers_age,
                 months_in_loc=months_in_loc,
                 months_before_survey=0:all$age[i]
                 )
  
  if (!is.na(all$mother_years_ed[i])){
    df$mother_years_ed[df$mother_years_ed > (df$mothers_age - 60)] <- df$mothers_age[(df$mother_years_ed > (df$mothers_age - 60))] - 60
  }
  
  if (!all$alive[i]){
    df$alive <- df$age <= all$age_at_death[i]
    
    df <- df %>%
      filter(df$age <= all$age_at_death[i])
    
  } else{
    df$alive <- TRUE
  }
  
  df$mother_years_ed <- floor(df$mother_years_ed/12)
  df$mothers_age <- floor(df$mothers_age/12)
  
  write.table(df, paste0('~/child-months/', i), row.names=F, col.names=F, sep=',')
  
  if (i == nrow(all)){
    system('/home/mattcoop/telegram.sh "Done processing DHS"')
  }
}


