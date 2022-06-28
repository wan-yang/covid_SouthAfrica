# for S Africa and maybe other countries
# combine results from multiple runs - diff eval method
state.names = c('S1', 'E1','I1',
                'death1', 'newIobs1','newItot1','beta',
                'Tei','Tir','Trs','Td.mean','Td.sd',
                'p.mob','alpha','ifr')
  
files = list.files(dir_res, full.names = T)
files = files[grepl('.RData',files)]
files = files[grepl('train.proj', files)]

# compare performance across runs?


res.train <-  lapply(files, function(x) {
  
  try(load(x))
  
  id <- gsub(dir_res,'',x) %>% 
    strsplit('_') %>% unlist
  
  # get %S
  tmp = train.proj$states_stats[state == 'S1', ]
  tmp[, c("mean","median","iqr.lwr","iqr.upr","ci95.lwr","ci95.upr")] = tmp[, c("mean","median","iqr.lwr","iqr.upr","ci95.lwr","ci95.upr")] / N * 100
  tmp$state = 'Susceptibility'
  
  # cumulative infection rate
  tmp2 = train.proj$cumIperc_stats
  tmp2$state = 'cumItot'; tmp2$sd = NULL;
  
  # cumulative immue loss
  tmp3 = train.proj$immLoss_stats
  
  d <- rbind(
    data.table(state = 'Rt',train.proj$Rt_stats),
    data.table(state = 'Rtx',train.proj$Rtx_stats),
    data.table(state = 'R0',train.proj$R0_stats),
    data.table(tmp),
    data.table(tmp2),
    data.table(tmp3),
    data.table(train.proj$states_stats),
    fill = T
  ) %>%
    melt(id.vars = c('state','Week.start'), variable.factor = F)
  
  # d$fcast.start.month = train.proj$fcast.start.month
  d$wave = train.proj$wave
  d$fcast.start.week = train.proj$fcast.start.week #  train.proj$fcast_stats$Week.start %>% min %>% as.Date
  d$loc = train.proj$loc # id[3] %>% strsplit('\\//') %>% unlist %>% tail(1)
  d$fcast.tm = id[6]
  # d$seasonality = id[5]
  d$run <- tail(id,1) %>% strsplit('\\.') %>% unlist %>% head(1) %>% gsub(pattern = 'r', replacement =  '') %>% as.integer()
  
  
  
  d
}) %>%
  rbindlist() %>%
  (function(d) d[, j = list(value = round(mean(value), 6)), by = list(loc, state, variable, wave, fcast.tm, fcast.start.week, Week.start)]) # %>%


res.train$state = factor(res.train$state, levels=c("IimmLoss", "VimmLoss", 'Rt','R0','Rtx', 'Susceptibility', 'cumItot', state.names), 
                         labels = c('recoveree immune loss', 'vaccinee immune loss', 'Rt','R0','Rtx','Susceptibility', 'Cumulative infection rate', 'Susceptible', 'Exposed','Infectious',
                                    'death', 'case','infection','transmission rate',
                                    'latent period','infectious period','immunity period','Td.mean','Td.sd',
                                    'p.mob','infection detection rate','IFR') 
                         )

save(res.train, file = paste0(dir_res, 'res.train',date.tag,'.RData'))
rm(res.train)
print('res.train done')

res.proj <-  lapply(files, function(x) {
  
  try(load(x))
  
  id <- gsub(dir_res,'',x) %>% 
    strsplit('_') %>% unlist
  
  d <- train.proj$fcast_stats %>%
    melt(id.vars = c('measure','Week.start'), variable.factor = F)
  
  # d$fcast.start.month = train.proj$fcast.start.month
  d$wave = train.proj$wave
  d$fcast.start.week = train.proj$fcast.start.week #  train.proj$fcast_stats$Week.start %>% min %>% as.Date
  d$loc = train.proj$loc # id[3] %>% strsplit('\\//') %>% unlist %>% tail(1)
  d$fcast.tm = id[6]
  # d$seasonality = id[5]
  d$run <- tail(id,1) %>% strsplit('\\.') %>% unlist %>% head(1) %>% gsub(pattern = 'r', replacement =  '') %>% as.integer()
  
  d
}) %>%
  rbindlist() %>%
  (function(d) d[, j = list(value = round(mean(value), 0)), by = list(loc, measure, variable, wave, fcast.tm, fcast.start.week, Week.start)]) # %>%


save(res.proj, file = paste0(dir_res, 'res.proj',date.tag,'.RData'))
rm(res.proj)
print('res.proj done')

