library(tidyverse)
library(lme4)

data <- read_csv('/home/mattcoop/child-months/Mortality-combined.csv') %>%
  select(alive, age, mother_years_ed, mothers_age, birth_order, male, cc, code) %>%
  na.omit %>%
  mutate(mortality = !alive,
         survey_code = substr(code, 1, 4))

mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male, 
           family='binomial', data=data)

data$glm_res <- resdiuals(mod)

system('/home/mattcoop/telegram.sh "Done with GLM Residuals"')

mod2 <- glmer(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + (1|cc) + (1|survey_code), 
           family='binomial', data=data)

data$glmm_res <- resdiuals(mod2)

system('/home/mattcoop/telegram.sh "Done with GLMM Residuals"')

write.csv(data, '/home/mattcoop/child-months/Mortality-combined-residuals.csv')

