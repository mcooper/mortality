library(data.table)
library(fixest)
library(tidyverse)

setwd('~/mortalityblob/mortality-dhs/')

cov <- c("spei.ch.3","spei.ch.6","spei.ch.12","spei.ch.24","spei.ch.36","spei.ch.48","spei.fl.1","spei.fl.2","spei.fl.3","spei.fl.6","spei.fl.12","spei.fl.24","spei.fl.36","spei.fl.48","spei.er.1","spei.er.2","spei.er.3","spei.er.6","spei.er.12","spei.er.24","spei.er.36","spei.er.48","spei.tc.1","spei.tc.2","spei.tc.3","spei.tc.6","spei.tc.12","spei.tc.24","spei.tc.36","spei.tc.48")

df <- data.frame(spei_range=seq(-2, 2, 0.1))
df$age <- 0
df$mother_years_ed <- 10
df$mothers_age <- 20
df$birth_order <- 2
df$male <- TRUE
df$code <- "MA-4-1-341"

for (i in cov){
  print(paste(Sys.time(), i, which(cov == i)/length(cov)))

  dat <- fread('Mortality-combined.csv', 
               drop=cov[!cov %in% i])

  #Define Segmenting Function
  piece.formula <- function(var.name, knots) {
    formula.sign <- rep(" - ", length(knots))
    formula.sign[knots < 0] <- " + "
    paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  }

  formula <- paste0("mortality ~ ", 
                       piece.formula(i, c(-1, -0.5, 0, 0.5, 1)), ' + ',
                       'age + mother_years_ed + mothers_age + birth_order + male | code')

  mod <- feols(as.formula(formula), data=dat)

  df[ , i] <- df$spei_range
  df[ , paste0(i, '_res')] <- predict(mod, df)

  rm(dat, mod)
  gc()
}
write.csv(df, '~/mortalityblob/mortality-dhs/outputs/comparison.csv', row.names=F)

df <- df %>%
  select(matches('spei_range|res$')) %>%
  gather(mod, value, -spei_range) %>%
  mutate(data_source = substr(mod, 6, 7),
         scale = as.numeric(str_extract(mod, '\\d+')))

ggplot(df) + 
  geom_line(aes(x=spei_range, y=value)) + 
  facet_grid(data_source ~ scale)

ggsave('~/mortalityblob/mortality-dhs/outputs/comparison.png')


