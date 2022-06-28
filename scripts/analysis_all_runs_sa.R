# script to output summary results
# and tables

library(tidyverse)
library(data.table)
library(magrittr)
library(ggplot2)
library(xlsx)
library(MMWRweek)
library(readr); library(readxl); library(writexl); library(stringi)
library(lemon)
library(tgp)
library(scales) # for transparency
library(R0)
library(rdrop2)
library(msm) # for the deltamethod function to compute variance
library(openxlsx)



date.tag = '_2022-03-07' # 

dir_data = '../data/'
dir_code = '../scripts/'
dir_res = '../results/'

# scripts
source(paste0(dir_code,'Fn_util_sa_plots.R'))

mob.type = 'business'

DAT.EPI = read.csv(paste0(dir_data, 'da_case_death_sa_subnational',date.tag,'.csv')) %>% data.table()
DAT.EPI$date = DAT.EPI$date %>% as.Date
# da = da[date >= date.start]

# read mobility data
DAT.MOB = read.csv(paste0(dir_data, 'da_mobility_sa_subnational',date.tag,'.csv')) %>% data.table()
DAT.MOB = DAT.MOB[data.type == mob.type]

# read vaccination data
DAT.VAC = read.csv(paste0(dir_data,'da_vx_perM_sa_subnational_lagged',date.tag,'.csv')) %>% data.table()

# read seasonal trend
DAT.SN = read.csv(paste0(dir_data, 'est.sn.2000t2020_sa_subnational.csv')) %>% data.table()

# the list of states 
# locs = DAT.EPI$country %>% unique()
url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/za_province_pop.csv'
pop = read_csv(url(url.t), col_names =F) %>% data.table() 
provinces = data.table(code = c('WC', 'NC', 'EC', 'GP', 'KZN', 'MP', 'FS', 'LP', 'NW'), 
                       label = c('Western Cape', 'Northern Cape', 'Eastern Cape', 'Gauteng', 'KwaZulu-Natal', 
                                 'Mpumalanga', 'Free State', 'Limpopo', 'North West'))

names(pop) = c('location','population')
pop[location == 'Northwest']$location = "North West"

# ranked by population size
loc.names = pop$location 
locs = loc.names

N = 1e6; # per 1 M
epi.model = 'SEIRSV' # susceptible-exposed-infectious-recovered-susc

# load combined model outputs
load(paste0(dir_res, 'res.summary',date.tag,'.RData'))
res.train00 = res.train

# LARGE FILE SIZE, NOT INCLUDED
load(paste0(dir_res, 'res.cumIperc_ens',date.tag,'.RData'))   



if(F){
  # simple quality check
  
  tda2 = res.train[state %in% c('Susceptibility', 'Rtx', 'infection detection rate', 'IFR')]
  tda2$state = factor(tda2$state, levels = c('Susceptibility', 'Rtx', 'infection detection rate', 'IFR'))
  tda2 = dcast(tda2, loc + Week.start + state ~ variable, value.var = 'value')
  
  summary(newVstat %>% dplyr:: filter(variant == 'variant-beta'))
  summary(newVstat %>% dplyr:: filter(variant == 'variant-delta'))
  summary(newVstat %>% dplyr:: filter(variant == 'variant-omicron'))
  
  sum((newVstat %>% dplyr:: filter(variant == 'variant-beta') %>% .$perc.dImm.mn > 100))
  sum((newVstat %>% dplyr:: filter(variant == 'variant-delta') %>% .$perc.dImm.mn > 100))
  sum((newVstat %>% dplyr:: filter(variant == 'variant-omicron') %>% .$perc.dImm.mn > 100))
  
}


# output the estiamted changes in transmissbility and immune erosion potential in a table
tab = NULL; sce_variant = unique(newVstat$variant)
{
  res = newVstat %>% data.table()
  res = melt(res, id.vars = c('variant', 'loc','run')) 
  res = res[variable %in% c('perc.dImm.mn','perc.dRtx.mean')]
  res$state = factor(res$variable, levels = c('perc.dRtx.mean','perc.dImm.mn'), labels =  c('dRtx', 'dImm'))
  res$loc = factor(res$loc, levels = locs)
  res = res[order(loc, variant, run, state)]
  
  for(loc.t in locs){
    for(ve.t in sce_variant){
      tmp = res[loc == loc.t & variant == ve.t]
      tab = rbind(tab,
                  data.table(loc = loc.t, variant = gsub('variant-','',ve.t), state = 'dRtx', 
                             mean = mean(tmp[state=='dRtx']$value) %>% round(2), sd = sd(tmp[state=='dRtx']$value)%>% round(2),
                             quantile(tmp[state=='dRtx']$value, probs = c(.5, .25, .75, .025, .975, .1, .9, .05, .95)) %>% round(2) %>% t),
                  data.table(loc = loc.t, variant = gsub('variant-','',ve.t), state = 'dImm', 
                             mean = mean(tmp[state=='dImm']$value) %>% round(2), sd = sd(tmp[state=='dImm']$value) %>% round(2),
                             quantile(tmp[state=='dImm']$value, probs = c(.5, .25, .75, .025, .975, .1, .9, .05, .95)) %>% round(2) %>% t)
      )
    }
  }
  # combine all locations
  for(ve.t in sce_variant){
    tmp = res[variant == ve.t]
    tab = rbind(tab,
                data.table(loc = 'All combined', variant = gsub('variant-','',ve.t), state = 'dRtx', 
                           mean = mean(tmp[state=='dRtx']$value) %>% round(2), sd = sd(tmp[state=='dRtx']$value)%>% round(2),
                           quantile(tmp[state=='dRtx']$value, probs = c(.5, .25, .75, .025, .975, .1, .9, .05, .95)) %>% round(2) %>% t),
                data.table(loc = 'All combined', variant = gsub('variant-','',ve.t), state = 'dImm', 
                           mean = mean(tmp[state=='dImm']$value) %>% round(2), sd = sd(tmp[state=='dImm']$value) %>% round(2),
                           quantile(tmp[state=='dImm']$value, probs = c(.5, .25, .75, .025, .975, .1, .9, .05, .95)) %>% round(2) %>% t)
    )
  }
}
setnames(tab, c("50%", "25%","75%","2.5%","97.5%","10%", '90%',"5%", '95%'), c('median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr','ci80.lwr','ci80.upr','ci90.lwr','ci90.upr'))
tab$loc = factor(tab$loc, levels = c('All combined', locs))
write.csv(tab, paste0(dir_res, 'tab_summary.est',date.tag,'.csv'), row.names = F)

# TABLE 1 -
tab$variant = factor(tab$variant, levels = c('beta', 'delta', 'omicron'), labels = c('Beta', 'Delta', 'Omicron (BA.1)'))
tab$cb = tab[,c('mean', 'ci95.lwr','ci95.upr'), with =F] %>% apply(1, fn_format, roundigt=1)
tda1 = dcast(tab, loc + variant ~ state, value.var = 'cb')
tda2 = dcast(tab, loc + state ~ variant, value.var = 'cb')
tda2$state = factor(tda2$state, levels = c('dRtx','dImm'),labels = c('% Increase in transmissibility vs WT', '% Immune erosion vs ALL prior variants/vax'))
tda2 = tda2[order(loc, state)] %>% setnames(c('loc','state'), c('Province','Quantity'))

write.csv(tda2, paste0(dir_res, 'Table1_summary.est_mean.95CI',date.tag,'.csv'), row.names = F)

# out put in a xlsx file for each Province
sheets = list("Gauteng" = tda2[Province == 'Gauteng'], 
              "KwaZulu-Natal" = tda2[Province == 'KwaZulu-Natal'], 
              "Western Cape" = tda2[Province == 'Western Cape'],
              "Eastern Cape" = tda2[Province == 'Eastern Cape'], 
              "Limpopo" = tda2[Province == 'Limpopo'],
              "Mpumalanga" = tda2[Province == 'Mpumalanga'], 
              "North West" = tda2[Province == 'North West'],
              "Free State" = tda2[Province == 'Free State'], 
              "Northern Cape" = tda2[Province == 'Northern Cape']) 
File.name = paste0(dir_res,'Table_summary.est_mean.95CI',date.tag,'.xlsx')
wb <- openxlsx:: createWorkbook()
for(i in 1:length(sheets)){
  sheet.name = names(sheets)[i]
  sheet.cont = sheets[[i]]
  addWorksheet(wb, sheet.name)
  writeData(wb, i, sheet.cont, colNames = T)
  setColWidths(wb, sheet = i, cols = 1:ncol(sheet.cont), widths = 'auto')
}
openxlsx:: saveWorkbook(wb, File.name, overwrite = T)



# Compute the cumulative infection rate over each wave
# read the parm bounds for different stage
# read the parm bounds for different stage
parm.bound_VEC = read.csv(paste0(dir_code, 'parm.bounds.csv'))  %>% data.table()
parm.bound_vec = parm.bound_VEC[country == 'South Africa']
parm.bound_vec$lwr = parm.bound_vec$lwr %>% as.numeric()
parm.bound_vec$upr = parm.bound_vec$upr %>% as.numeric()

cumIperc_ens$date = cumIperc_ens$date %>% as.Date
Week.starts = res.train$Week.start %>% unique %>% sort

RES.CUMI = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  
  for(iv in 1:nrow(tm_variant)){
    idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) %>% Week.starts[.]
    # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
    tmp = fn_getCumI(tda = cumIperc_ens, loc = loc.t, t.start = idx0, t.end = idx1)
    names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    RES.CUMI = rbind(RES.CUMI, 
                     data.table(Province = loc.t, variant = tm_variant[iv]$type, date.start = idx0, date.end = idx1, tmp)
                     )
    
  }
}
# RES.CUMI$variant = factor(RES.CUMI$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron'))
write.csv(RES.CUMI, paste0(dir_res, 'res_CumulativeInfectRate_byloc_bywave.csv'), row.names = F)

if(F){# match with the hosp and excess death data as early data are missing
cum.hosp = read.csv(paste0(dir_data,'data_cum.hosp_byloc_bywave.csv')) %>% data.table()  # note early data are incomplete, starting from 6/6/20
cum.excess.d = read.csv(paste0(dir_data,'data_cum.excess.death_byloc_bywave.csv')) %>% data.table()  # note early data incomplete, starting from 5/5/20, but small
RES.CUMI = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  tm_variant2 = cum.hosp[loc == loc.t]
  tm_variant2$date.start = tm_variant2$date.start %>% as.Date()
  tm_variant2$date.end = tm_variant2$date.end %>% as.Date()
  
  
  for(iv in 1:nrow(tm_variant)){
    idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx0b = which(as.Date(Week.starts) >=  as.Date(tm_variant2[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx0 = max(idx0, idx0b)
    idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) %>% Week.starts[.]
    # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
    tmp = fn_getCumI(tda = cumIperc_ens, loc.t=loc.t, t.start = idx0, t.end = idx1)
    names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    RES.CUMI = rbind(RES.CUMI, 
                     data.table(loc = loc.t, variant = tm_variant[iv]$type, date.start = idx0, date.end = idx1, tmp)
    )
    
  }
}
# RES.CUMI$variant = factor(RES.CUMI$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron'))
write.csv(RES.CUMI, paste0(dir_res, 'res_cumI_byloc_bywave_match.hosp.csv'), row.names = F)

RES.CUMI = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  tm_variant2 = cum.excess.d[loc == loc.t]
  tm_variant2$date.start = tm_variant2$date.start %>% as.Date()
  tm_variant2$date.end = tm_variant2$date.end %>% as.Date()
  
  
  for(iv in 1:nrow(tm_variant)){
    idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx0b = which(as.Date(Week.starts) >=  as.Date(tm_variant2[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx0 = max(idx0, idx0b)
    idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) %>% Week.starts[.]
    # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
    tmp = fn_getCumI(tda = cumIperc_ens, loc.t= loc.t, t.start = idx0, t.end = idx1)
    names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    RES.CUMI = rbind(RES.CUMI, 
                     data.table(loc = loc.t, variant = tm_variant[iv]$type, date.start = idx0, date.end = idx1, tmp)
    )
    
  }
}
# RES.CUMI$variant = factor(RES.CUMI$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron'))
write.csv(RES.CUMI, paste0(dir_res, 'res_cumI_byloc_bywave_match.excessdeath.csv'), row.names = F)
}

# Output overall infection detection rate
res = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  da.t = dcast(DAT.EPI[country == loc.t], date ~ data.type, value.var = 'value')
  da.t[is.na(da.t)] = 0
  da.t$date = da.t$date %>% as.Date
  da.t = da.t %>% dplyr::filter(date %in% Week.starts) # exclude very early weeks with low cases
  
  for(iv in 1:nrow(tm_variant)){
    
    # to match the week start, as the dates in the parm.bound_VEC file are just rough timings and may not exactly be the start of each week
    idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) %>% Week.starts[.]
    
    t.case = (da.t %>% dplyr::filter(date >= idx0 & date <= idx1) %>% .$case %>% sum) / N * 100
    t.death = (da.t %>% dplyr::filter(date >= idx0 & date <= idx1) %>% .$death %>% sum) / N * 100
    
    res = rbind(res, 
                     data.table(Province = loc.t, variant = tm_variant[iv]$type, 
                                date.start = idx0, date.end = idx1, 
                                case.rate = t.case, death.rate = t.death,
                                raw.cfr = t.death / t.case * 100)
    )
    
  }
}
RES.CUMI = read.csv(paste0(dir_res,'res_CumulativeInfectRate_byloc_bywave.csv')) %>% data.table()
RES.CUMI$date.start = RES.CUMI$date.start %>% as.Date
RES.CUMI$date.end = RES.CUMI$date.end %>% as.Date
res = merge(res, RES.CUMI, by = c("Province","variant", "date.start", "date.end"))
res = res %>% dplyr::mutate(idr.mean.perc = case.rate / v.mean * 100, 
                     idr.median.perc = case.rate / v.median * 100, 
                     idr.iqr.upr.perc = case.rate / iqr.lwr * 100, 
                     idr.iqr.lwr.perc = case.rate / iqr.upr * 100, 
                     idr.ci95.upr.perc = case.rate / ci95.lwr * 100, 
                     idr.ci95.lwr.perc = case.rate / ci95.upr * 100, 
                     ifr.mean.perc = death.rate / v.mean * 100, 
                     ifr.median.perc = death.rate / v.median * 100, 
                     ifr.iqr.upr.perc = death.rate / iqr.lwr * 100, 
                     ifr.iqr.lwr.perc = death.rate / iqr.upr * 100, 
                     ifr.ci95.upr.perc = death.rate / ci95.lwr * 100, 
                     ifr.ci95.lwr.perc = death.rate / ci95.upr * 100
                     )
res$idr = res[,c('idr.mean.perc','idr.ci95.lwr.perc','idr.ci95.upr.perc'),with=F] %>% apply(1, fn_format, roundigt=2)
res$ifr = res[,c('ifr.mean.perc','ifr.ci95.lwr.perc','ifr.ci95.upr.perc'),with=F] %>% apply(1, fn_format, roundigt=2)

res$variant = factor(res$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral wave','Beta wave', 'Delta wave', 'Omicron (BA.1) wave'))
res$Province = factor(res$Province, levels = locs); 
res = res %>% dplyr::arrange(Province, variant)

write.csv(res, paste0(dir_res, 'res_IDR_IFR_byloc_bywave.csv'), row.names = F)

# Format outputs
res$date.start = res$date.start %>% format('%b %d, %Y')
res$date.end = res$date.end %>% format('%b %d, %Y')
res$time_period = res[,c('date.start','date.end'),with=F] %>% apply(1, paste, collapse = ' - ')
res$cumI = res[,c('v.mean','ci95.lwr','ci95.upr'),with=F] %>% apply(1, fn_format, roundigt=2)
cumI = dcast(res, Province ~ variant, value.var = 'cumI')
idr = dcast(res, Province ~ variant, value.var = 'idr')
ifr = dcast(res, Province ~ variant, value.var = 'ifr')
cumI$Province = factor(cumI$Province, levels = locs); cumI = cumI %>% dplyr::arrange(Province)
idr$Province = factor(idr$Province, levels = locs); idr = idr %>% dplyr::arrange(Province)
ifr$Province = factor(ifr$Province, levels = locs); ifr = ifr %>% dplyr::arrange(Province)

# mortality rates are very different across diff provinces - 
# potentially, there are some data problem (e.g. data dumps including deaths occurred in previous times)
raw.cfr = res %>% mutate(raw.cfr = raw.cfr %>% round(., digits = 2)) %>% 
  dcast(., Province ~ variant, value.var = 'raw.cfr')
# b/c some of the mortality data may be problematic (e.g. potential data dumps including deaths occurred in previous times but not properly distributed)
# also compute the IFR as a weighted average of estimated weekly infection rate during each wave
wtIFR = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  
  for(iv in 1:nrow(tm_variant)){
    
    # to match the week start, as the dates in the parm.bound_VEC file are just rough timings and may not exactly be the start of each week
    idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) %>% Week.starts[.]
    idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) %>% Week.starts[.]
    
    wt.ifr.t = res.train %>% 
      filter(Week.start >= idx0 & Week.start <= idx1 & state %in% c('IFR','infection') & 
                           loc == loc.t & variable != 'sd') %>% 
      dcast(., loc + variable + Week.start ~ state, value.var = 'value') %>% 
      group_by(variable) %>%
      summarise(wtIFR = weighted.mean(x = IFR, w = infection) * 100) %>% 
      mutate(Province = loc.t, variant = tm_variant[iv]$type, 
             date.start = idx0, date.end = idx1)
    wtIFR = rbind(wtIFR, wt.ifr.t)
  }
}
wtIFR = wtIFR %>% dcast(., Province + variant + date.start + date.end ~ variable, value.var = 'wtIFR')
wtIFR$wtIFR = wtIFR[,c('mean','ci95.lwr','ci95.upr')] %>% apply(1, fn_format, roundigt=2)
wtIFR$variant = factor(wtIFR$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral wave','Beta wave', 'Delta wave', 'Omicron (BA.1) wave'))
wtIFR$Province = factor(wtIFR$Province, levels = locs); 
wtIFR = wtIFR %>% dcast(., Province ~ variant, value.var = 'wtIFR')
wtIFR = wtIFR %>% dplyr::arrange(Province)

sheets = list('attack rate' = cumI,
              "infection-detection rate" = idr , 
              "IFR per data" = ifr, 
              "IFR per model estimates" = wtIFR, 
              'raw case-fatality ratio' = raw.cfr) 
File.name = paste0(dir_res, 'TableS1-S3_',date.tag,'.xlsx')
wb <- openxlsx:: createWorkbook()
for(i in 1:length(sheets)){
  sheet.name = names(sheets)[i]
  sheet.cont = sheets[[i]]
  addWorksheet(wb, sheet.name)
  writeData(wb, i, sheet.cont, colNames = T)
  setColWidths(wb, sheet = i, cols = 1:ncol(sheet.cont), widths = 'auto')
}
openxlsx:: saveWorkbook(wb, File.name, overwrite = T)

# Get cumulative infection rate through the first three waves (i.e. before Omicron BA.1)
res = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  da.t = dcast(DAT.EPI[country == loc.t], date ~ data.type, value.var = 'value')
  da.t[is.na(da.t)] = 0
  da.t$date = da.t$date %>% as.Date
  da.t = da.t[date %in% Week.starts]
  
  idx0 = Week.starts[1]
  idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[3]$date.end)) %>% tail(1) %>% Week.starts[.]
  # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
  tmp = fn_getCumI(tda = cumIperc_ens, loc.t=loc.t, t.start = idx0, t.end = idx1)
  names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  res = rbind(res, 
              data.table(loc = loc.t, variant = 'first 3 waves', date.start = idx0, date.end = idx1, tmp)
  )
}
write.csv(res, paste0(dir_res,  'res_cumI_byloc_first3waves.csv'), row.names = F)

# Get cumulative infection rate through the first 2 waves (i.e. before Delta)
res = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  da.t = dcast(DAT.EPI[country == loc.t], date ~ data.type, value.var = 'value')
  da.t[is.na(da.t)] = 0
  da.t$date = da.t$date %>% as.Date
  da.t = da.t[date %in% Week.starts]
  
  idx0 = Week.starts[1]
  idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[2]$date.end)) %>% tail(1) %>% Week.starts[.]
  # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
  tmp = fn_getCumI(tda = cumIperc_ens, loc.t=loc.t, t.start = idx0, t.end = idx1)
  names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  res = rbind(res, 
              data.table(loc = loc.t, variant = 'first 2 waves', date.start = idx0, date.end = idx1, tmp)
  )
}
write.csv(res, paste0(dir_res,  'res_cumI_byloc_first2waves.csv'), row.names = F)


# save timing of each wave - table S6
tm_waves = NULL
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  
  tm_waves = rbind(tm_waves, 
                   data.table(loc = loc.t, variant = tm_variant$type, date.start = tm_variant$date.start, date.end = tm_variant$date.end)
  )
}
tm_waves[variant == 'omicron']$date.end = as.Date('2022/3/5')
tm_waves$variant = factor(tm_waves$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron (BA.1)'))
write.csv(tm_waves, paste0(dir_res, 'TableS6_tm_waves.csv'), row.names = F)

if(F){
  
  # done in local machine, as file for cumulative infection rate is too large to up load
  # match cumulative infection rate with sero-survey data - use mid month
  load(paste0(dir_res, 'res.cumIperc_ens',date.tag,'.RData'))  # LARGE FILE (>300 MB)
  
  # cross check with serology data
  d.sero = read_xlsx(paste0(dir_data, 'serology_sa_byloc.xlsx'), sheet = 1) %>% data.table()
  d.sero$loc = factor(d.sero$loc, levels = provinces$code, labels = provinces$label)
  
  est.sero = NULL
  for(iv in 1:nrow(d.sero)){
    idx0 = Week.starts[1] %>% as.Date
    # idx1 = which(as.Date(Week.starts) <=  as.Date(paste0(d.sero[iv]$month,'/15'))) %>% tail(1) %>% Week.starts[.]
    idx1 = which.min(abs(as.Date(Week.starts) -  as.Date(paste0(d.sero[iv]$month,'/15'))))  %>% Week.starts[.]
    loc.t = d.sero[iv]$loc %>% as.character()
    # tmp = fn_getCumI(tda = cumIperc_ens, loc.t=gsub(' ', '',loc.t), t.start = idx0, t.end = idx1)
    tmp = fn_getCumI(tda = cumIperc_ens, loc.t= loc.t, t.start = idx0, t.end = idx1)
    names(tmp) = c('v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    est.sero = rbind(est.sero, 
                     data.table(loc = loc.t, month = d.sero[iv]$month, measure = d.sero[iv]$measure * 100, date.start = idx0, date.end = idx1, tmp)
    )
  }
  
  est.sero.ddup = unique(est.sero[,c('loc','month', 'date.start',  'date.end', 'v.mean','v.median','iqr.lwr','iqr.upr','ci95.lwr','ci95.upr'),with=F])
  est.sero.ddup$loc = factor(est.sero.ddup$loc, levels = locs)
  est.sero$loc= factor(est.sero$loc, levels = locs)
  est.sero.ddup$month2 = format(as.Date(paste0(est.sero.ddup$month,'/1')),'%Y %b')
  est.sero$month2 = format(as.Date(paste0(est.sero$month,'/1')),'%Y %b')
  est.sero.ddup$month2 = factor(est.sero.ddup$month2, levels = c(paste('2020', month.abb), paste('2021', month.abb)))
  est.sero$month2 = factor(est.sero$month2, levels = c(paste('2020', month.abb), paste('2021', month.abb)))
  
  # save this for future use
  write.csv(est.sero.ddup, paste0(dir_res, 'est.sero.ddup.csv'), row.names = F)
  write.csv(est.sero, paste0(dir_res, 'est.sero.csv'), row.names = F)
}

