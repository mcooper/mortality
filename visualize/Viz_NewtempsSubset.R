library(mgcv)
library(tidyverse)

setwd('~/mortalityblob/mod-results/')

fs <- list.files(pattern='9019')

for (f in fs){
  mod <- readRDS(f) 
  
  df <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE,
                   var=seq(0, 31, 1), offset=-2.9623923851105)
  
  names(df)[names(df)=='var'] <- gsub('mod.|.subset.RDS', '', f)
  
  p_res <- predict(mod, df, se.fit=T, type='response') %>%
    data.frame %>%
    mutate(max = fit + se.fit*2,
           min = fit - se.fit*2,
           x = 0:31)
  
  percentile <- gsub('mod_ct.9019.|.subset.RDS', '', f)

  p <- as.numeric(percentile)

  ggplot(p_res) + 
    geom_ribbon(aes(x=x, ymin=min, ymax=max), fill='#cccccc') + 
    geom_line(aes(x=x, y=fit)) + 
    labs(x='Count of days greater than ' + p + "th %ile",
         y='Probability of death') + 
    theme_bw()
    
  ggsave('~/gd/Mortality/TempCountGT' + p + '.png', width=6, height=6)    

}

fs <- list.files(pattern='tmax')

for (f in fs){
  mod <- readRDS(f) 
  
  df <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE,
                   var=seq(0, 31, 1), offset=-2.9623923851105, x=0, y=0)
  
  names(df)[names(df)=='var'] <- gsub('mod.|.subset.RDS', '', f)
  
  p_res <- predict(mod, df, se.fit=T, type='response') %>%
    data.frame %>%
    mutate(max = fit + se.fit*2,
           min = fit - se.fit*2,
           x = 0:31)
  
  percentile <- gsub('mod_ct.tmax.|.subset.RDS', '', f)

  p <- as.numeric(percentile)

  ggplot(p_res) + 
    geom_ribbon(aes(x=x, ymin=min, ymax=max), fill='#cccccc') + 
    geom_line(aes(x=x, y=fit)) + 
    labs(x='Count of days greater than ' + p + "C",
         y='Probability of death') + 
    theme_bw()
    
  ggsave('~/gd/Mortality/TempCountGT' + p + '.png', width=6, height=6)    

}
