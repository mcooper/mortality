library(tidyverse)
library(mgcv)
library(speedglm)

setwd('~/mortalityblob/mod-results/chirps-terraclimate')

options(stringsAsFactors=F)

fs <- data.frame(file=list.files(pattern='mm|tc')) %>%
  mutate(seg=ifelse(grepl('seg', file), 'Segmented', 'Smooth'),
         data=case_when(grepl('_5yb\\.', file) ~ '5yb',
                        grepl('_2yo', file) ~ '2yo',
                        grepl('_5yb2yo', file) ~ '5yb2yo',
                        TRUE ~ 'full'),
         spei=ifelse(grepl('36', file), 36, 3),
         sub=ifelse(grepl('all', file), 'all', 'subset'),
         precip=case_when(grepl('mm', file) ~ 'mm',
                          grepl('tc', file) ~ 'tc',
                          TRUE ~ 'all_tc'))

preddf <- data.frame(spei = seq(-4, 4, by=0.1)) %>%
  mutate(spei.mm.36 = spei,
         spei.mm.3 = spei,
         spei.tc.36 = spei,
         spei.tc.3 = spei,
         spei.mm.3.seg = cut(spei.mm.3, c(-2, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 2)),
         spei.mm.36.seg = cut(spei.mm.36, c(-2, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 2)),
         spei.tc.3.seg = cut(spei.tc.3, c(-2, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 2)),
         spei.tc.36.seg = cut(spei.tc.36, c(-2, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 2)),
         age = 1,
         mother_years_ed = 0,
         mothers_age = 20,
         birth_order = 2,
         male = TRUE,
         date = 1400,
         offset = -2.9674248)

allres <- data.frame()
for (i in 3:nrow(fs)){
  cat(i, fs$file[i], '\n')
  load(fs$file[i])

  res <- predict(mod, preddf, type='response')

  m <- merge(fs[i, ], data.frame(spei_val=preddf$spei, prediction=res))

  allres <- bind_rows(allres, m)
}

write.csv(allres, 'model-predictions.csv', row.names=F)
allres <- read.csv('model-predictions.csv')

#First compare splines and segments
ggplot(allres %>% filter(spei==3, spei_val > -2.5, spei_val < 2.5)) + 
  geom_line(aes(x=spei_val, y=prediction, linetype=seg)) + 
  facet_wrap(data ~ sub + precip, scales='free')
ggplot(allres %>% filter(spei==36, spei_val > -2.5, spei_val < 2.5)) + 
  geom_line(aes(x=spei_val, y=prediction, linetype=seg)) + 
  facet_wrap(data ~ sub + precip, scales='free')

#Segments are super jagged, I like splines more
#Also focus on more local values
sel <- allres %>% filter(seg == 'Smooth', spei_val > -2.5, spei_val < 2.5)

#Compare all variables
ggplot(sel %>% filter(spei==3)) + 
  geom_line(aes(x=spei_val, y=prediction)) + 
  facet_wrap(data ~ sub + precip, scales='free')
ggplot(sel %>% filter(spei==36)) + 
  geom_line(aes(x=spei_val, y=prediction)) + 
  facet_wrap(data ~ sub + precip, scales='free')

#All and Subset are extremely similar
#But not exactly so
sel <- sel %>% filter(sub == 'all')

#Compare timescale versus precip type
ggplot(sel %>% filter(spei==3)) + 
  geom_line(aes(x=spei_val, y=prediction)) + 
  facet_wrap(data ~ precip, scales='free')
ggplot(sel %>% filter(spei==36)) + 
  geom_line(aes(x=spei_val, y=prediction)) + 
  facet_wrap(data ~ precip, scales='free')

#Current thoughts:
# * Its too messy, just use a linear effect with speedglm
# * Also try just categorical effects
# * The most important interactive effect is age.
# * Lots of horeshoes on both sides



