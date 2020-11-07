library(tidyverse)
library(data.table)
library(speedglm)

setwd('~/mortalityblob')

#############################
# Analyze full Subset MM
#############################
system('/home/mattcoop/telegram.sh "Reading in Data"')

data <- fread('mortality-dhs/Mortality-combined.csv') %>%
  dplyr::select(-matches('1$|2$|6$|12$')) %>%
  filter(!is.infinite(spei.mm.3), !is.infinite(spei.mm.24),
         !is.infinite(spei.tc.3), !is.infinite(spei.tc.24)) %>%
  mutate(spei.mm.3.seg = cut(spei.mm.3, c(-100, 0, 100)),
         spei.mm.24.seg = cut(spei.mm.24, c(-100, 0, 100)),
	       spei.tc.3.seg = cut(spei.tc.3, c(-100, 0, 100)),
         spei.tc.24.seg = cut(spei.tc.24, c(-100, 0, 100)),
	       age_cat = cut(age, c(-1, 11.9, 1000)))

system('/home/mattcoop/telegram.sh "Starting"')

###spei.mm.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3*age_cat,
           family=binomial('logit'), data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_full.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.24.seg*spei.mm.24*age_cat,
           family=binomial('logit'), data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.24_full.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.24 done!"')


###########################
# Analyze 5yb Subset
###########################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.mm.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3*age_cat,
           family=binomial('logit'), data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.24.seg*spei.mm.24*age_cat,
           family=binomial('logit'), data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.24_5yb.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.24 done!"')

#############################
# Analyze full Subset TC
#############################

###spei.tc.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3*age_cat,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_full.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.24.seg*spei.tc.24*age_cat,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.24_full.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.24 done!"')

#############################
# Analyze 5yb Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.tc.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3*age_cat,
           family=binomial('logit'), data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.24.seg*spei.tc.24*age_cat,
           family=binomial('logit'), data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.24_5yb.simpseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.24 done!"')

####################################
# Do the whole thing again
# Except with a survey fixed effect
#####################################

data$survey_code <- substr(data$code, 1, 6)

###spei.mm.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3*age_cat + survey_code,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_full.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.24.seg*spei.mm.24*age_cat + survey_code,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.24_full.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.24 done!"')


###########################
# Analyze 5yb Subset
###########################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.mm.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3*age_cat + survey_code,
           family=binomial('logit'), data=sel, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.24.seg*spei.mm.24*age_cat + survey_code,
           family=binomial('logit'), data=sel, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.24_5yb.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.24 done!"')

#############################
# Analyze full Subset TC
#############################

###spei.tc.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3*age_cat + survey_code,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_full.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.24.seg*spei.tc.24*age_cat + survey_code,
           family=binomial('logit'), data=data, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.24_full.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.24 done!"')

#############################
# Analyze 5yb Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.tc.3
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3*age_cat + survey_code,
           family=binomial('logit'), data=sel, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.24
mod <- speedglm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.24.seg*spei.tc.24*age_cat + survey_code,
           family=binomial('logit'), data=sel, sparse=FALSE)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.24_5yb.simpseg_fe.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.24 done!"')
