library(tidyverse)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv') %>%
  select(ind_code, alive, age, mother_years_ed, mothers_age, birth_order, male, cc, code, gdp, spei3, spei36) %>%
  na.omit %>%
  mutate(mortality = !alive,
         survey_code = substr(code, 1, 4))

mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male, 
           family='binomial', data=data)

data$glm_res <- round(residuals(mod), 6)

system('/home/mattcoop/telegram.sh "Done with GLM Residuals"')

sel <- data %>% select(ind_code, glm_res, gdp, spei3, spei36)

write.csv(sel, '/home/mattcoop/mortalityblob/Mortality-residuals.csv', row.names=FALSE)

system('/home/mattcoop/telegram.sh "Results Written"')


