library(tidyverse)
library(lme4)

data <- read_csv('/home/mattcoop/child-months/Mortality-combined.csv') %>%
  select(ind_code, alive, age, mother_years_ed, mothers_age, birth_order, male, cc, code) %>%
  na.omit %>%
  mutate(mortality = !alive,
         survey_code = substr(code, 1, 4))

mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male, 
           family='binomial', data=data)

data$glm_res <- residuals(mod)

system('/home/mattcoop/telegram.sh "Done with GLM Residuals"')

mod2 <- glmer(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + (1|cc) + (1|survey_code), 
           family='binomial', data=data)

data$glmm_res <- residuals(mod2)

system('/home/mattcoop/telegram.sh "Done with GLMM Residuals"')

sel <- data %>% select(ind_code, glm_res, glmm_res)

sel[ , c('glm_re', 'glmm_res')] <- round(sel[ , c('glm_res', 'glmm_res')], 5)

write.csv(sel, '/home/mattcoop/child-months/Mortality-residuals.csv', row.names=FALSE)

system('/home/mattcoop/telegram.sh "Results Written"')


