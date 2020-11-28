library(tidyverse)
library(speedglm)

setwd('~/mortalityblob/mod-results/chirps-terraclimate')

options(stringsAsFactors=F)

fs <- data.frame(file=list.files(pattern='simpseg')) %>%
  mutate(data=case_when(grepl('_5yb\\.', file) ~ '5yb',
                        grepl('_2yo', file) ~ '2yo',
                        grepl('_5yb2yo', file) ~ '5yb2yo',
                        TRUE ~ 'full'),
         spei=ifelse(grepl('24', file), 24, 3),
         precip=case_when(grepl('mm', file) ~ 'mm',
                          grepl('tc', file) ~ 'tc',
                          TRUE ~ 'all_tc'))

preddf <- data.frame(spei = seq(-4, 4, by=0.1)) %>%
  mutate(spei.mm.24 = spei,
         spei.mm.3 = spei,
         spei.tc.24 = spei,
         spei.tc.3 = spei,
         spei.mm.3.seg = cut(spei.mm.3, c(-100, 0, 100)),
         spei.tc.3.seg = cut(spei.tc.3, c(-100, 0, 100)),
         spei.tc.24.seg = cut(spei.tc.24, c(-100, 0, 100)),
         spei.mm.24.seg = cut(spei.mm.24, c(-100, 0, 100)),
         age = 1,
         mother_years_ed = 0,
         mothers_age = 20,
         birth_order = 2,
         male = TRUE,
         date = 1400,
         offset = -2.9674248)

preddf <- bind_rows(preddf %>% mutate(age_cat = 1),
                    preddf %>% mutate(age_cat = 24)) %>%
  mutate(age_cat = cut(age_cat, c(-1, 11.9, 1000)))

allres <- data.frame()
for (i in 1:nrow(fs)){
  cat(i, fs$file[i], '\n')
  load(fs$file[i])

  res <- predict(mod, preddf, type='response')

  m <- merge(fs[i, ], data.frame(spei_val=preddf$spei, prediction=res, age_cat=preddf$age_cat))

  allres <- bind_rows(allres, m)
}

#First compare splines and segments
ggplot(allres %>% filter(spei==3)) + 
  geom_line(aes(x=spei_val, y=prediction, color=age_cat)) + 
  facet_grid(data ~ precip, scales='free')
ggplot(allres %>% filter(spei==24)) + 
  geom_line(aes(x=spei_val, y=prediction, color=age_cat)) + 
  facet_grid(data ~ precip, scales='free')
