library(tidyverse)
library(glmnet)

setwd('~/mortalityblob/glmnet/')

#############################
# Continent No-AEZ
#############################
fs <- list.files(pattern='noloc.Rdata$|afr.Rdata')

resdf <- data.frame(spei=seq(-2.5, 2.5, 0.1))

for (f in fs){
  load(f)

  df <- data.frame(as.matrix(mod$beta)) %>%
    mutate(var = row.names(.)) %>%
    filter(grepl('spei', var))

  spei <- df$s0[grepl('^spei', df$var)]
  spei_p1 <- df$s0[grepl('\\+ 1', df$var)]
  spei_m0 <- df$s0[grepl('- 0', df$var)]
  spei_m1 <- df$s0[grepl('- 1', df$var)]
  
  res <- resdf$spei*spei
  res <- res + pmax(resdf$spei + 1, 0)*spei_p1
  res <- res + pmax(resdf$spei - 0, 0)*spei_m0
  res <- res + pmax(resdf$spei - 1, 0)*spei_m1

  resdf[ , f] <- res
}

plotdf <- resdf %>%
  gather(model, value, -spei) %>%
  mutate(speiwindow = substr(model, 1, 6),
         precipvar = substr(model, 8, 9),
         allloc = !grepl('noloc', model))

ggplot(plotdf) + 
  geom_line(aes(x=spei, y=value, color=allloc)) + 
  facet_grid(speiwindow ~ precipvar)

#############################
# AEZs 
#############################
fs <- list.files(pattern='spei(03|24)\\....afr.aez')

resdf <- expand.grid(list(spei=seq(-2.5, 2.5, 0.1),
                          aez=c('Forest', 'Highlands', 'Mediterranean', 'Savanna',
                                'SemiForest', 'Desert')))

for (f in fs){
  load(f)

  df <- data.frame(as.matrix(mod$beta)) %>%
    mutate(var = row.names(.)) %>%
    filter(grepl('spei', var))

  spei <- df$s0[grepl('^spei', df$var)]
  spei_p1 <- df$s0[grepl('^I.*\\+ 1', df$var)]
  spei_m0 <- df$s0[grepl('^I.*- 0', df$var)]
  spei_m1 <- df$s0[grepl('^I.*- 1', df$var)]
  
  res <- resdf$spei*spei
  res <- res + pmax(resdf$spei + 1, 0)*spei_p1
  res <- res + pmax(resdf$spei - 0, 0)*spei_m0
  res <- res + pmax(resdf$spei - 1, 0)*spei_m1

  for (a in c('Forest', 'Highlands', 'Mediterranean', 'Savanna','SemiForest')){
    df <- data.frame(as.matrix(mod$beta)) %>%
      mutate(var = row.names(.)) %>%
      filter(grepl(paste0('aez', a, '.*spei'), var))

    ix <- resdf$aez == a

    spei_a <- df$s0[grepl(':spei', df$var)]
    spei_p1_a <- df$s0[grepl('\\+ 1', df$var)]
    spei_m0_a <- df$s0[grepl('- 0', df$var)]
    spei_m1_a <- df$s0[grepl('- 1', df$var)]

    res[ix] <- res[ix] + resdf$spei[ix]*spei_a
    res[ix] <- res[ix] + pmax(resdf$spei[ix] + 1, 0)*spei_p1_a
    res[ix] <- res[ix] + pmax(resdf$spei[ix] - 0, 0)*spei_m0_a
    res[ix] <- res[ix] + pmax(resdf$spei[ix] - 1, 0)*spei_m1_a

  }

  resdf[ , f] <- res

}


plotdf <- resdf %>%
  gather(model, value, -spei, -aez) %>%
  mutate(speiwindow = substr(model, 1, 6),
         precipvar = substr(model, 8, 9)) %>%
  filter(aez != 'Mediterranean')

#Standardize
plotdf <- plotdf %>%
  group_by(model, aez) %>%
  mutate(value = value - value[spei == 0])

ggplot(plotdf) + 
  geom_line(aes(x=spei, y=value, color=aez)) + 
  facet_grid(speiwindow ~ precipvar)

#############################
# Combined Model With AEZs 
#############################
fs <- list.files(pattern='0324')

resdf <- expand.grid(list(spei=seq(-2.5, 2.5, 0.1),
                          aez=c('Forest', 'Highlands', 'Mediterranean', 'Savanna',
                                'SemiForest', 'Desert')))

for (f in fs){
  load(f)

  for (s in c('3', '24')){

    df <- data.frame(as.matrix(mod$beta)) %>%
      mutate(var = row.names(.)) %>%
      filter(grepl(paste0('spei....', s), var))

    spei <- df$s0[grepl('^spei', df$var)]
    spei_p1 <- df$s0[grepl('^I.*\\+ 1', df$var)]
    spei_m0 <- df$s0[grepl('^I.*- 0', df$var)]
    spei_m1 <- df$s0[grepl('^I.*- 1', df$var)]
    
    res <- resdf$spei*spei
    res <- res + pmax(resdf$spei + 1, 0)*spei_p1
    res <- res + pmax(resdf$spei - 0, 0)*spei_m0
    res <- res + pmax(resdf$spei - 1, 0)*spei_m1

    for (a in c('Forest', 'Highlands', 'Mediterranean', 'Savanna','SemiForest')){
      df <- data.frame(as.matrix(mod$beta)) %>%
        mutate(var = row.names(.)) %>%
        filter(grepl(paste0('aez', a, '.*spei....', s), var))

      ix <- resdf$aez == a

      spei_a <- df$s0[grepl(':spei', df$var)]
      spei_p1_a <- df$s0[grepl('\\+ 1', df$var)]
      spei_m0_a <- df$s0[grepl('- 0', df$var)]
      spei_m1_a <- df$s0[grepl('- 1', df$var)]

      res[ix] <- res[ix] + resdf$spei[ix]*spei_a
      res[ix] <- res[ix] + pmax(resdf$spei[ix] + 1, 0)*spei_p1_a
      res[ix] <- res[ix] + pmax(resdf$spei[ix] - 0, 0)*spei_m0_a
      res[ix] <- res[ix] + pmax(resdf$spei[ix] - 1, 0)*spei_m1_a

    }

    resdf[ , paste0(f, s)] <- res

  }
}


plotdf <- resdf %>%
  gather(model, value, -spei, -aez) %>%
  mutate(speiwindow = substr(model, 26, nchar(model)),
         precipvar = substr(model, 10, 11)) %>%
  filter(aez != 'Mediterranean')

#Standardize
plotdf <- plotdf %>%
  group_by(model, aez) %>%
  mutate(value = value - value[spei == 0])

ggplot(plotdf) + 
  geom_line(aes(x=spei, y=value, color=aez)) + 
  facet_grid(speiwindow ~ precipvar)



