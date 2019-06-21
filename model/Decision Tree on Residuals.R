library(tidyverse)

data <- read.csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

mod <- glm(mortality ~ age + mother_years_ed + mothers_age + birth_order + male, 
           family='binomial', data=data)

data$res <- resdiuals(mod)

