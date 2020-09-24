library(tidyverse)
library(data.table)

setwd('~/mortalityblob')

#############################
# Analyze full Subset
#############################
system('/home/mattcoop/telegram.sh "Reading in Data"')

data <- fread('mortality-dhs/Mortality-combined.csv') %>%
  select(-matches('tc|1$|2$|\\.6$|12$|24$|48$')) %>%
  filter(!is.infinite(spei.mm.3) & !is.infinite(spei.mm.36))

system('/home/mattcoop/telegram.sh "Starting"')

###spei.mm.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3,
           family='binomial', data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_full.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.36.seg*spei.mm.3,
           family='binomial', data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_full.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 2yo Subset
#############################

sel <- data %>%
  filter(age < 24)

###spei.mm.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.36.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 5yb Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.mm.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.36.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_5yb.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60 & age < 24)

###spei.mm.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.3.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.mm.36.seg*spei.mm.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_5yb2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-combined.csv') %>%
  select(-matches('mm|1$|2$|\\.6$|12$|24$|48$')) %>%
  filter(!is.infinite(spei.tc.3) & !is.infinite(spei.tc.36))

###spei.tc.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3,
           family='binomial', data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_full.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.36.seg*spei.tc.36,
           family='binomial', data=data)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_full.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 2yo Subset
#############################
sel <- data %>%
  filter(age < 24)

###spei.tc.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.36.seg*spei.tc.36,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 5yb Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60)

###spei.tc.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.36.seg*spei.tc.36,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_5yb.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

sel <- data %>%
  filter(months_before_survey < 60 & age < 24)

###spei.tc.3
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.3.seg*spei.tc.3,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           spei.tc.36.seg*spei.tc.36,
           family='binomial', data=sel)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_5yb2yo.allseg.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')


