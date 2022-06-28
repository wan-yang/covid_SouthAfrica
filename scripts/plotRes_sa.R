# to plot inference results for SA provincial estimates including the omicron wave
# for the MS, revision


date.tag = '_2022-03-07'

library(data.table)
library(magrittr)
library(ggplot2)
library(xlsx)
library(MMWRweek)
library(lemon)
library('readr')
library('readxl')
library('writexl')
library('stringi')
library(tidyverse)
library(tgp)
library(gridExtra)
library(classInt)
library(RColorBrewer)

## Note: first set working directory to source file location
dir_data = '../data/'
dir_code = '../scripts/'
dir_res = '../results/'
dir_plot = '../plots/'

# dir_plot = dir_res

if(!file.exists(dir_plot)) dir.create(dir_plot)

source(paste0(dir_code,'Fn_util_sa_plots.R'))

# load related data
mob.type = 'business'
N = 1e6; # population size is set to 1M

DAT.EPI = read.csv(paste0(dir_data, 'da_case_death_sa_subnational',date.tag,'.csv')) %>% data.table()
DAT.EPI$date = DAT.EPI$date %>% as.Date
# da = da[date >= date.start]

# read mobility data
DAT.MOB = read.csv(paste0(dir_data, 'da_mobility_sa_subnational',date.tag,'.csv')) %>% data.table()
DAT.MOB = DAT.MOB[data.type == mob.type]

# read vaccination data
DAT.VAC = read.csv(paste0(dir_data,'da_vx_perM_sa_subnational_lagged',date.tag,'.csv')) %>% data.table()
DAT.VAC.nolag = read.csv(paste0(dir_data,'da_vx_perM_sa_subnational_nolag',date.tag,'.csv')) %>% data.table()

# read seasonal trend
DAT.SN = read.csv(paste0(dir_data, 'est.sn.2000t2020_sa_subnational.csv')) %>% data.table()
DAT.SN[location=="Nothern Cape"]$location = "Northern Cape" # mis-spell

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
nloc = length(locs)


# load model results
load(paste0(dir_res, "res.summary",date.tag,".RData"))
res.train00 = res.train
loc.names = locs


res.train = res.train00 %>% data.table()

res.train = dcast(res.train, loc + state + Week.start ~ variable, value.var = 'value')
res.train$Week.start = res.train$Week.start %>% as.Date

# load cumulative infection rate by wave
cumIbywave = read.csv(paste0(dir_res,'res_CumulativeInfectRate_byloc_bywave.csv')) %>% data.table()
cumIbywave$variant = factor(cumIbywave$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron (BA.1)'))
cumIbywave$ci95.upr = pmin(100, cumIbywave$ci95.upr)
cumIbywave$loc = cumIbywave$Province

da1 = data.table::copy(DAT.EPI)  
da1 = da1 %>% setnames(c('country','data.type','value'), c('loc','state', 'obs'))
est1 = res.train[state %in% c('case', 'death')]
est1$date = est1$Week.start %>% as.Date
# est1$loc = factor(est1$loc, levels = gsub(' ','', locs), labels = locs)
est1$loc = factor(est1$loc, levels = locs)
tda1 = merge(da1, est1, by = c('loc','state','date'))
tda1$loc = factor(tda1$loc, levels = locs)

provinces = data.table(code = c('WC', 'NC', 'EC', 'GP', 'KZN', 'MP', 'FS', 'LP', 'NW'), 
                       label = c('Western Cape', 'Northern Cape', 'Eastern Cape', 'Gauteng', 'KwaZulu-Natal', 
                                 'Mpumalanga', 'Free State', 'Limpopo', 'North West'))


# read the parm bounds for different stage
parm.bound_VEC = read.csv(paste0(dir_code, 'parm.bounds.csv'))  %>% data.table()
parm.bound_vec = parm.bound_VEC[country == 'South Africa']
parm.bound_vec$lwr = parm.bound_vec$lwr %>% as.numeric()
parm.bound_vec$upr = parm.bound_vec$upr %>% as.numeric()
date.start = as.Date('2020/03/15')


# LOAD INDEPENDENT DATA FOR MODEL VALIDATION
# compare with hospitalization data and excess mortality data
cum.hosp = read.csv(paste0(dir_data,'data_cum.hosp_byloc_bywave.csv')) %>% data.table()  # note early data are incomplete, starting from 6/6/20
cum.excess.d = read.csv(paste0(dir_data,'data_cum.excess.death_byloc_bywave.csv')) %>% data.table()  # note early data incomplete, starting from 5/5/20, but small
cum.hosp$variant = factor(cum.hosp$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron (BA.1)'))
cum.excess.d$variant = factor(cum.excess.d$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron (BA.1)'))
p.hosp = 8
cum.hosp$value = cum.hosp$hosp.per100k / p.hosp  # scale it to per 10K
p.death = 5
cum.excess.d$value = cum.excess.d$excess.death.per100k / p.death




est.sero.ddup = read.csv(paste0(dir_res, 'est.sero.ddup.csv')) %>% data.table()
est.sero = read.csv(paste0(dir_res, 'est.sero.csv')) %>% data.table()
est.sero.ddup$loc = factor(est.sero.ddup$loc, levels = locs)
est.sero$loc = factor(est.sero$loc, levels = locs)

# merge for comparison
cp = merge(cumIbywave[,c('loc', 'variant','v.mean')], cum.hosp[,c('loc', 'variant','hosp.per100k')], by = c('loc', 'variant'))
cp = merge(cp, cum.excess.d[,c('loc', 'variant','excess.death.per100k')], by = c('loc', 'variant'))
# compute correlations
corr = cp[, list(corr.ih = cor(v.mean, hosp.per100k), corr.id = cor(v.mean, excess.death.per100k)), by = 'variant']
corr$label.hosp = paste0('r = ', round(corr$corr.ih, 2))
corr$label.death = paste0('r = ', round(corr$corr.id, 2))
corr = corr %>% data.table()

# outlier during the Beta wave
id.beta.ex = cp[variant == 'Beta' & loc != 'Western Cape']
corr.id.beta.ex = cor(id.beta.ex$v.mean, id.beta.ex$excess.death.per100k) %>% round(2)
corr.id.beta.ex 
corr[variant == 'Beta']$label.death = paste(corr[variant == 'Beta']$label.death, ' (', corr.id.beta.ex, ' excl WC)', sep='')


#####  PLOT MAIN FIGURES ##### 

##### FUGURE 1 start ##### 
p1a = getPlotMultiLoc(tda1, ytitle = 'Number per million population', ptitle = '(A) Observations vs. Model fit', ncol.t = 1)
p1b = ggplot(est.sero.ddup, aes(x = month2)) +
  geom_boxplot(aes(
    lower = iqr.lwr, 
    upper = iqr.upr, 
    middle = v.mean, 
    ymin = ci95.lwr, 
    ymax = ci95.upr,
    group= month2),
    stat = "identity", lwd = .2
  ) + 
  geom_point(aes(x = month2, y= measure), data = est.sero, col = 'red', size = .5) +
  facet_wrap(~loc, ncol = 3) + # scales = 'free_y',
  labs(x = '', y = 'Cumulative infection rate or Seroprevalence (%)') + ggtitle('(B) Estimated cumulative infection rates vs. independent serology data') +
  theme_minimal() + theme.t8

pdf(paste0(dir_plot, 'Fig1_model.fit.validation_sero.pdf'), width = 9, height = 5)
grid.arrange(
  grobs = list(p1a, p1b),
  layout_matrix = matrix(c(1,2), nrow = 1))
dev.off()

##### FUGURE 1 end ##### 

##### FUGURE 2:  SEE BELOW IN THE END  ##### 


##### FUGURE 3 start ##### 
# Example estimates for Gauteng
p.titles = c('(A) Seasonality, NPIs, Vaccination v. Pandemic Dynamics',
             '(B) Rt v. Infection Rate', '(C) Transmissibility','(D) Susceptibility',
             '(E) Infection-detection rate', '(F) Infection-fatality risk')
for(loc.t in locs[1]){
  
  # rough timing of diff waves
  tm_waves = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_waves$start = tm_waves$date.start %>% as.Date()
  tm_waves$end = tm_waves$date.end %>% as.Date()
  tm_waves$event = factor(tm_waves$type, levels = c('wt', 'beta', 'delta', 'omicron'), labels = c('Ancestral', 'Beta','Delta', 'Omicron (BA.1)'))
  cnt = 0
  pdf(paste0(dir_plot, 'Fig3_',loc.t,'_model_fit_key.est', date.tag,'.pdf'), width = 8, height = 8)
  par(mfrow=c(3,2),mar=c(2,2.5,1.5,2),oma=c(0.3,.1, 0.1,0),mgp=c(1.0,.1,0), lheight=1, cex=.8,cex.axis=.85,cex.lab=.9,tck=-.015)
  
  cnt = cnt + 1
  da.t = dcast(DAT.EPI[country == loc.t], date ~ data.type, value.var = 'value')
  da.t$date = da.t$date %>% as.Date
  da.t = da.t[date >= date.start]
  da.t[is.na(da.t)] = 0
  dates.t = unique(da.t$date)  %>% as.Date %>% sort
  
  mob.t = DAT.MOB[data.type == 'business' & country == loc.t]
  mob.t$date = mob.t$date %>% as.Date
  relR0.t = DAT.SN[location == loc.t]
  vac.t = DAT.VAC.nolag[location == loc.t]
  vac.t$date =vac.t$date %>% as.Date
  vac.t$cum.v1 = cumsum(vac.t$n.v1) / N
  vac.t$cum.v2 = cumsum(vac.t$n.v2) / N
  
  da.t = merge(da.t, mob.t[,c('date', 'year', 'week', 'value')], by = 'date') %>% setnames('value','rel.mob')
  da.t = merge(da.t, relR0.t[,c('week','value')], by = 'week', all.x = T) %>% setnames('value','rel.R0')
  da.t = merge(da.t, vac.t[,c('date','cum.v1','cum.v2')], by = 'date', all.x = T) 
  
  da.t = da.t[order(date)]
  da2.t = da.t; da2.t$death = da2.t$death * 10
  da2.t = melt(da2.t[,c('date','case','death')], id.vars = 'date')
  
  ymin = 0 # min(da.t$rel.mob, da.t$rel.R0, da.t$cum.v1, na.rm=T) * .9; 
  ymax = max(da.t$rel.mob, da.t$rel.R0) * 1.1
  xx = barplot(value ~ variable + date, ylim = c(0, max(da.t$case) * 1.1), data = da2.t, xaxt = 'n', yaxs="i",
               ylab =  'Case per million or Death per 100,000', xlab = '', beside = T, border = 'transparent', col = c('grey50', 'red'))
  
  par(new = T)
  xx = colMeans(xx)
  plot(xx, da.t$rel.mob, ylim = c(ymin, ymax), type = 'l', ylab = '', xlab = 'Week start (mm/dd/yy)', xaxt = 'n', yaxt = 'n', yaxs="i",col = 'blue', lwd = 1.2)
  lines(xx, da.t$rel.R0, ylim = c(ymin, ymax), col = 'orange', lwd = 1.2)
  lines(xx, da.t$cum.v1, ylim = c(ymin, ymax), col = 'darkgreen', lwd = 1.2, lty = 5)
  lines(xx, da.t$cum.v2, ylim = c(ymin, ymax), col = 'darkgreen', lwd = 1.2, lty = 1)
  abline(h = 1, col = 'grey50', lty=2)
  axis(4)
  axis(1, at = xx, labels = da.t$date %>% format('%m/%d/%y'))
  mtext('Mobility / Vaccination / Seasonality', side = 4, cex = .72, outer = F, line = 1.1)
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  
  text(x = xx[21], y = .3, 'case', adj = 0, cex=.75, font = 2, col = 'grey30')
  text(x = xx[22], y = .16, 'death', adj = 0, cex=.75, font = 2, col = 'red')
  text(x = xx[12], y = 1.1, 'seasonality', adj = 0, cex=.75,srt=0, font = 2, col = 'orange'); # pos=4,
  text(x = xx[8], y = .7, 'mobility', adj = .5, cex=.75,srt=0, font = 2, col = 'blue'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v1, na.rm = T) + .06, 'vaccination:\n1st dose',  adj = .95, cex=.75,srt=0, font =2, col = 'darkgreen'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v2, na.rm = T) - .06, '2nd dose', adj = .95, cex=.75,srt=0, font =2, col = 'darkgreen'); # pos=4,
  
  
  # key model estimates
  # do estimated infections and Rt
  cnt=cnt+1
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  dates.t = unique(as.Date(mm$date)) %>% sort
  x=1:length(dates.t)
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.1;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax),  border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4, # 
      
    }
  }
  
  # overlay with Rt
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.1; 
  
  plot(x,tda$mean,ylab='', yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  abline(h = 1, lty = 1, col = 'grey')
  lines(x,tda$mean, col='blue',lwd=2)
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Rt', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4, -z.width*.01
      
    }
  }
  # overlay with Rtx
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(-.5, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated transmissibility', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with susceptibility
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, 100),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated susceptibility (%)', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # infection-detection rate
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with idr
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'infection detection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'infection detection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated infection detection rate', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # ifr
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with IFR
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'IFR'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'IFR'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated infection fatality risk', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  dev.off()
  
}
##### FUGURE 3 end ##### 



##### FUGURE 4 start ##### 
# Get weekly infection rate estimates
newIest = res.train00[state == 'infection']
newIest$value = newIest$value / N * 100 # in percentage
newIest$loc = factor(newIest$loc, levels = locs)
newIest$Week.start = newIest$Week.start %>% as.Date

# Get cumulative infection rate estimates
cumIest = res.train00[state == 'Cumulative infection rate']
cumIest$loc = factor(cumIest$loc, levels = locs)
cumIest$Week.start = cumIest$Week.start %>% as.Date

# Get estimated susceptibility
Sest = res.train00[state == 'Susceptibility']
Sest$loc = factor(Sest$loc, levels = locs)
Sest$Week.start = Sest$Week.start %>% as.Date

# Get estimated changes in transmissibility and immune erosion
res = newVstat %>% data.table()
res = melt(res, id.vars = c('loc', 'run', 'variant')) 
res = res[variable %in% c('perc.dImm.mn','perc.dRtx.mean')]
res$state = factor(res$variable, levels = c('perc.dRtx.mean','perc.dImm.mn'), 
                   labels =  c('Transmissibility','Immune erosion'))
variant.lab_vec = c('Beta','Delta','Omicron (BA.1)')
variant_vec = c(paste0('variant-',c('beta','delta','omicron')))

locs_vec = locs
res$variant = factor(res$variant, levels = variant_vec, labels = variant.lab_vec)
res$loc = factor(res$loc, levels = locs)
res = res[order(variant, loc, run, state)]
res.v.mn = res %>% dplyr::group_by(variant, state) %>% dplyr::summarise(value = mean(value, na.rm=T)) %>% data.table()


ymax = max(newIest[variable == 'mean']$value) * 1.01
p4a = ggplot(newIest[variable == 'mean']) +
  geom_tile(aes(x = Week.start, y = loc, fill = value)) + 
  scale_fill_distiller(palette = "RdPu", direction = 1, limits = c(0, ymax),expand = c(0, 0)) +
  labs(x = '', y = '', fill = '% pop') + ggtitle('(A) Estimated mean weekly infection rate (% population)') +
  scale_x_date(breaks = seq(min(newIest$Week.start), max(newIest$Week.start), by = '6 months'),
               labels = format(seq(min(newIest$Week.start), max(newIest$Week.start), by = '6 months'),'%Y %b'), expand = c(0, 2)) +
  theme_minimal() + theme.t 

ymax = max(cumIest[variable == 'mean']$value) * 1.01
p4b = ggplot(cumIest[variable == 'mean']) +
  geom_tile(aes(x = Week.start, y = loc, fill = value)) + 
  scale_fill_distiller(palette = "RdPu", direction = 1, limits = c(0, ymax),expand = c(0, 0)) +
  labs(x = '', y = '', fill = '% pop') + ggtitle('(B) Estimated mean cumulative infection numbers / population') +
  scale_x_date(breaks = seq(min(cumIest$Week.start), max(cumIest$Week.start), by = '6 months'),
               labels = format(seq(min(cumIest$Week.start), max(cumIest$Week.start), by = '6 months'),'%Y %b'), expand = c(0, 2)) +
  theme_minimal() + theme.t # theme_grey() +  

ymax = max(Sest[variable == 'mean']$value) * 1.01
ymin = min(Sest[variable == 'mean']$value) * .99
p4c = ggplot(Sest[variable == 'mean']) +
  geom_tile(aes(x = Week.start, y = loc, fill = value)) + 
  scale_fill_distiller(palette = "RdPu", direction = -1, limits = c(ymin, ymax),expand = c(0, 0)) +
  labs(x = '', y = '', fill = '% pop') + ggtitle('(C) Estimated mean susceptiblity (% population)') +
  scale_x_date(breaks = seq(min(Sest$Week.start), max(Sest$Week.start), by = '6 months'),
               labels = format(seq(min(Sest$Week.start), max(Sest$Week.start), by = '6 months'),'%Y %b'), expand = c(0, 2)) +
  theme_minimal() + theme.t # theme_grey() +  


p4d = ggplot(res, 
             aes(y=value, x = loc)) + 
  geom_boxplot(outlier.size= .5, alpha = .3, lwd = .3, color = 'grey30') + 
  facet_rep_wrap(~state+variant, ncol = 3, repeat.tick.labels = T, labeller = label_wrap_gen(width = 30, multi_line=T)) +
  labs(x='', y='Relative increase (%)') +  ggtitle('(D) Estimated epidemiological properties for each variant') +
  geom_hline(aes(yintercept = value), data = res.v.mn, colour = "red") +
  theme_minimal() + theme.t8


pdf(paste0(dir_plot, 'Fig4_overall.est.pdf'), width = 9, height = 5)
grid.arrange(
  grobs = list(p4a, p4b, p4c, p4d),
  layout_matrix = rbind(c(1, 4),
                        c(2, 4),
                        c(3, 4))
)
dev.off()
##### FUGURE 4 end ##### 


#####  PLOT SUMMPLEMENTAL FIGURES ##### 

##### FUGURE S1 start ##### 
res.train = res.train00 %>% data.table()

res.train = dcast(res.train, loc + state + Week.start ~ variable, value.var = 'value')
res.train$Week.start = res.train$Week.start %>% as.Date


fits = res.train[state %in% c('case','death') ]
# fits$loc = factor(fits$loc, levels = gsub(' ','',locs), labels = loc.names)
fits$loc = factor(fits$loc, levels = locs)

tda = DAT.EPI[country %in% loc.names] %>% setnames(., c('date','country','data.type', 'value'), c("Week.start",'loc', 'state','obs')) # dcast(DAT.EPI[country %in% loc.names], country + date ~ data.type, value.var = 'value')
tda$Week.start = tda$Week.start %>% as.Date
tda$year = NULL; tda$week = NULL
fits = merge(fits, tda, by = c('loc', 'state', "Week.start"))
# fits$threshold = NA;

tda = fits
tda$state = factor(tda$state, levels = c('case', 'death'), labels = c('case', 'death'))
tda$loc = factor(tda$loc, levels = locs)
# tda$loc = factor(tda$loc, levels = paste0('sce',1:n.sce), labels = c('truth 1','truth 2','truth 3', 'truth n.sce'))
pdf(paste0(dir_plot, 'FigS1_model_fits', tail(tda$Week.start,1),'.pdf'), width = 8, height = 10)
p = getPlot_cpObs2(tda)
print(p)
dev.off()
##### FUGURE S1 end ##### 

##### FUGURE S2 start ##### 
cnt = 0; plot_list = list(); 
for(v.t in c('Ancestral','Beta', 'Delta', 'Omicron (BA.1)')){
  cnt = cnt + 1
  print(cnt)
  tda = cumIbywave[variant == v.t]
  tda.hosp = cum.hosp[variant == v.t]
  tda.exd = cum.excess.d[variant == v.t]
  corr.t = corr[variant == v.t]
  tda$loc = factor(tda$loc, levels = locs)
  tda.hosp$loc = factor(tda.hosp$loc, levels = locs)
  tda.exd$loc = factor(tda.exd$loc, levels = locs)
  tda = tda[order(loc)]
  tda.hosp = tda.hosp[order(loc)]
  tda.exd = tda.exd[order(loc)]
  
  p.hosp = (tda.hosp$hosp.per100k %>% mean) / (tda$v.mean %>% mean) 
  tda.hosp$value = tda.hosp$hosp.per100k / p.hosp
  p.exd = (tda.exd$excess.death.per100k %>% mean) / (tda$v.mean %>% mean) 
  tda.exd$value = tda.exd$excess.death.per100k / p.exd
  ptitle = paste0('(',LETTERS[cnt],') ', v.t,' wave: Estimated infection rates vs. hospitalizations and excess deaths')
  
  p.hosp.b = (tda.hosp$hosp.per100k %>% mean) / (tda$v.mean %>% mean) 
  tda.hosp$value = tda.hosp$hosp.per100k / p.hosp
  p.exd = (tda.exd$excess.death.per100k %>% mean) / (tda$v.mean %>% mean) 
  tda.exd$value = tda.exd$excess.death.per100k / p.exd
  ptitle = paste0('(',LETTERS[cnt],') ', v.t,' wave: Estimated infection rates vs. hospitalizations and excess deaths')
  ptitle.b = paste0('(',LETTERS[cnt+2],') ', v.t,' wave: Estimated infection rates vs.')
  
  pp.hosp = ggplot(tda, aes(x = loc)) +
    geom_boxplot(aes(
      lower = iqr.lwr, 
      upper = iqr.upr, 
      middle = v.mean, 
      ymin = ci95.lwr, 
      ymax = ci95.upr,
      group=loc),
      stat = "identity", lwd = .2
    ) + 
    geom_point(aes(x = loc, y=value), data = tda.hosp, col = 'red', size = .8) + 
    # geom_point(aes(x = loc, y=value), data = cum.excess.d[variant!='Omicron'], col = 'red') +
    scale_y_continuous(name = "Cumulative infection rate (%)", # Features of the first axis
                       sec.axis = sec_axis(trans=~. * p.hosp, name="Hospitalizations per 100K") # Add a second axis and specify its features
    ) + 
    geom_text(data = corr.t,
              mapping = aes(x = -Inf, y = -Inf, label = label.hosp),
              hjust   = -.5,vjust   = -10, col = 'red') +
    facet_rep_wrap(~variant, ncol = 3, repeat.tick.labels = 'all', labeller = as_labeller(c(Ancestral = "vs Hospitalizations", Beta = "vs Hospitalizations",Delta = "vs Hospitalizations", `Omicron (BA.1)` = "Hospitalizations"))) + # scales = 'free_y',
    labs(x = '') + # , y = 'Cumulative infection rate (%)'
    ggtitle(ptitle) +
    theme_minimal() + theme.t7
  
  
  pp.exd = ggplot(tda, aes(x = loc)) +
    geom_boxplot(aes(
      lower = iqr.lwr, 
      upper = iqr.upr, 
      middle = v.mean, 
      ymin = ci95.lwr, 
      ymax = ci95.upr,
      group=loc),
      stat = "identity", lwd = .2
    ) + 
    geom_point(aes(x = loc, y=value), data = tda.exd, col = 'red', size = .8) + 
    # geom_point(aes(x = loc, y=value), data = cum.excess.d[variant!='Omicron'], col = 'red') +
    scale_y_continuous(name = "Cumulative infection rate (%)", # Features of the first axis
                       sec.axis = sec_axis(trans=~. * p.exd, name="Excess deaths per 100K") # Add a second axis and specify its features
    ) + 
    geom_text(data = corr.t,
              mapping = aes(x = -Inf, y = -Inf, label = label.death), 
              hjust = ifelse(v.t!='Beta', -.5, -.18),vjust = -10, col = 'red') +
    facet_rep_wrap(~variant, ncol = 3, repeat.tick.labels = 'all', labeller = as_labeller(c(Ancestral = "vs Excess deaths", Beta = "vs Excess deaths",Delta = "vs Excess deaths", `Omicron (BA.1)` = "vs Excess deaths"))) + # scales = 'free_y',
    labs(x = '') + # , y = 'Cumulative infection rate (%)'
    ggtitle(' ') +
    theme_minimal() + theme.t7
  
  
  
  plot_list[[(cnt*2-1)]] = pp.hosp
  plot_list[[(cnt*2)]] = pp.exd
}

pp1 = plot_list[[1]]; pp2 = plot_list[[2]]; pp3 = plot_list[[3]]; pp4 = plot_list[[4]]; pp5 = plot_list[[5]]; pp6 = plot_list[[6]]; pp7 =plot_list[[7]]; pp8 =plot_list[[8]];

pdf(paste0(dir_plot, 'FigS2_validation_hosp_exd.pdf'), width = 6, height = 9)
grid.arrange(
  grobs = list(pp1,pp2, pp3,pp4,pp5,pp6,pp7, pp8),
  layout_matrix = matrix(1:8, nrow=4, byrow=T)
)
dev.off()
##### FUGURE S2 end ##### 

##### FUGURE S3 SEE BELOW IN THE END ##### 


##### FUGURE S4 - S11 ##### 
# Example estimates for other provinces
p.titles = c('(A) Seasonality, NPIs, Vaccination v. Pandemic Dynamics',
             '(B) Rt v. Infection Rate', '(C) Transmissibility','(D) Susceptibility',
             '(E) Infection-detection rate', '(F) Infection-fatality risk')

ifig.start = 4 # fig number
ifig = 0
for(loc.t in locs[2:9]){
  
  ifig = ifig + 1
  
  # rough timing of diff waves
  tm_waves = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_waves$start = tm_waves$date.start %>% as.Date()
  tm_waves$end = tm_waves$date.end %>% as.Date()
  tm_waves$event = factor(tm_waves$type, levels = c('wt', 'beta', 'delta', 'omicron'), labels = c('Ancestral', 'Beta','Delta', 'Omicron'))
  cnt = 0
  pdf(paste0(dir_plot, 'FigS',ifig.start+ifig-1,'_',loc.t,'_model_fit_key.est', date.tag,'.pdf'), width = 8, height = 8)
  par(mfrow=c(3,2),mar=c(2,2.5,1.5,2),oma=c(0.3,.1, 0.1,0),mgp=c(1.0,.1,0), lheight=1, cex=.8,cex.axis=.85,cex.lab=.9,tck=-.015)
  
  cnt = cnt + 1
  da.t = dcast(DAT.EPI[country == loc.t], date ~ data.type, value.var = 'value')
  da.t$date = da.t$date %>% as.Date
  da.t = da.t[date >= date.start]
  da.t[is.na(da.t)] = 0
  dates.t = unique(da.t$date)  %>% as.Date %>% sort
  
  mob.t = DAT.MOB[data.type == 'business' & country == loc.t]
  mob.t$date = mob.t$date %>% as.Date
  relR0.t = DAT.SN[location == loc.t]
  vac.t = DAT.VAC.nolag[location == loc.t]
  vac.t$date =vac.t$date %>% as.Date
  vac.t$cum.v1 = cumsum(vac.t$n.v1) / N
  vac.t$cum.v2 = cumsum(vac.t$n.v2) / N
  
  da.t = merge(da.t, mob.t[,c('date', 'year', 'week', 'value')], by = 'date') %>% setnames('value','rel.mob')
  da.t = merge(da.t, relR0.t[,c('week','value')], by = 'week', all.x = T) %>% setnames('value','rel.R0')
  da.t = merge(da.t, vac.t[,c('date','cum.v1','cum.v2')], by = 'date', all.x = T) 
  
  da.t = da.t[order(date)]
  da2.t = da.t; da2.t$death = da2.t$death * 10
  da2.t = melt(da2.t[,c('date','case','death')], id.vars = 'date')
  
  ymin = 0 # min(da.t$rel.mob, da.t$rel.R0, da.t$cum.v1, na.rm=T) * .9; 
  ymax = max(da.t$rel.mob, da.t$rel.R0) * 1.1
  xx = barplot(value ~ variable + date, ylim = c(0, max(c(da.t$case, da.t$death*10)) * 1.1), data = da2.t, xaxt = 'n', yaxs="i",
               ylab =  'Case per million or Death per 100,000', xlab = '', beside = T, border = 'transparent', col = c('grey50', 'red'))
  
  par(new = T)
  xx = colMeans(xx)
  plot(xx, da.t$rel.mob, ylim = c(ymin, ymax), type = 'l', ylab = '', xlab = 'Week start (mm/dd/yy)', xaxt = 'n', yaxt = 'n', yaxs="i",col = 'blue', lwd = 1.2)
  lines(xx, da.t$rel.R0, ylim = c(ymin, ymax), col = 'orange', lwd = 1.2)
  lines(xx, da.t$cum.v1, ylim = c(ymin, ymax), col = 'darkgreen', lwd = 1.2, lty = 5)
  lines(xx, da.t$cum.v2, ylim = c(ymin, ymax), col = 'darkgreen', lwd = 1.2, lty = 1)
  abline(h = 1, col = 'grey50', lty=2)
  axis(4)
  axis(1, at = xx, labels = da.t$date %>% format('%m/%d/%y'))
  mtext('Mobility / Vaccination / Seasonality', side = 4, cex = .72, outer = F, line = 1.1)
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  
  text(x = xx[21], y = .3, 'case', adj = 0, cex=.75, font = 2, col = 'grey30')
  text(x = xx[22], y = .16, 'death', adj = 0, cex=.75, font = 2, col = 'red')
  text(x = xx[12], y = 1.1, 'seasonality', adj = 0, cex=.75,srt=0, font = 2, col = 'orange'); # pos=4,
  text(x = xx[8], y = .7, 'mobility', adj = .5, cex=.75,srt=0, font = 2, col = 'blue'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v1, na.rm = T) + .06, 'vaccination:\n1st dose',  adj = .95, cex=.75,srt=0, font =2, col = 'darkgreen'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v2, na.rm = T) - .06, '2nd dose', adj = .95, cex=.75,srt=0, font =2, col = 'darkgreen'); # pos=4,
  
  
  # key model estimates
  # do estimated infections and Rt
  cnt=cnt+1
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  dates.t = unique(as.Date(mm$date)) %>% sort
  x=1:length(dates.t)
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.1;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax),  border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4, # 
      
    }
  }
  
  # overlay with Rt
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.1; 
  
  plot(x,tda$mean,ylab='', yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  abline(h = 1, lty = 1, col = 'grey')
  lines(x,tda$mean, col='blue',lwd=2)
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Rt', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4, -z.width*.01
      
    }
  }
  # overlay with Rtx
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(-.5, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated transmissibility', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with susceptibility
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, 100),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated susceptibility (%)', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # infection-detection rate
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with idr
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'infection detection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'infection detection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated infection detection rate', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # ifr
  cnt=cnt+1
  x=1:length(dates.t)
  # mm = res.train00[loc == gsub(' ','', loc.t) & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm = res.train00[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent', yaxs="i") # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  # add time lines
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .025; 
  e.t = tm_waves # events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*5), 
           angle = 45,col = alpha('grey50', .3 - .2 * (i %% 2)), border = 'transparent')
      # arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
      text(d.t3,ymax*(z-z.width*.8), tx.t,  adj = .5, cex=.75,srt=0, font = 1); # pos=4,
      
    }
  }
  # overlay with IFR
  par(new=T)
  # tda=res.train00[loc == gsub(' ','', loc.t) & state == 'IFR'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=res.train00[loc == loc.t & state == 'IFR'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='Week start (mm/dd/yy)',xaxt='n', yaxs="i")
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated infection fatality risk', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  dev.off()
  
}
##### FUGURE S4 - S11 end ##### 


##### FUGURE S12 - S14 ##### 
## sensitivity analysis, from multiple local files
## generated locally
## similar format as other figures
## thus, these are not included here
##### FUGURE S12 - S14 ##### 


##### FUGURE S15 - S23, start ##### 
## plot other parameters, Zt, Dt, Lt, etc.
# rough time line
tm_waves = as.Date(c('2020/10/15', '2021/5/15', '2021/11/15'))
vars = c('transmission rate', 'latent period', 'infectious period', 'immunity period', 'p.mob','Td.mean', 'Td.sd', 'infection detection rate', 'IFR')
vars.names =  c('Variant-specific transmission rate (per day)', 'Latent period (days)', 'Infectious period (days)', 'Variant-specific immunity period (days)', 
                'Scaling of NPI effectiveness (e_t)',
                'Time from infectiousness to detection, mean (days)', 'Time from infectiousness to detection, SD (days)', 
                'Infection detection rate (r_t)', 'IFR')
ifig.start = 15; ifig = 0
for(iv in 1:length(vars)){
  var.t = vars[iv]; var.name.t = vars.names[iv]
  tda = res.train00 %>% filter(state == var.t) %>% dcast(., loc + Week.start ~ variable, value.var = 'value')
  tda$loc = factor(tda$loc, levels = loc.names)
  pp = ggplot(data = tda) + 
    geom_line(aes(x = Week.start, y = median), size = 1) +  # no ctrl
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    geom_ribbon(aes(x = Week.start, ymin = ci95.lwr, ymax = ci95.upr),fill = 'blue', alpha = .1) +
    geom_vline(xintercept = tm_waves, size = .3, color = 'black', linetype="dashed") +
    facet_wrap(~ loc,  ncol = 3) + 
    ggtitle(var.name.t) + labs(x = 'Week start (mm/dd/yy)', y = 'Posterior estimates (median, 50% and 95% CrIs)') +
    scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 8)],
                 labels = format(dates.t[seq(1, length(dates.t), by = 8)],'%m/%d/%y')) + 
    ylim(0, max(c(tda$ci95.upr)) * 1.1) + 
    theme_minimal() + theme.t4
  
  pdf(paste0(dir_plot, 'FigS', ifig.start + ifig, '_', var.t, '.pdf'), width = 9, height = 7)
  print(pp)
  dev.off()
  
  ifig = ifig + 1
  
}
##### FUGURE S15 - S23, end ##### 




#### PLOT RETROSPECTIVE PREDICTIONS #####
# THIS IS DONE IN THE END, AS NEED TO LOAD THE REUSLTS, SOME SAVED IN VARAIBLES WITH THE SAME NAMES 
# WHICH WILL OVERWRITE THE MODEL-INFERENCE RESULTS

# load results
load(paste0(dir_res, 'res.train',date.tag,'.RData'))
load(paste0(dir_res, 'res.proj',date.tag,'.RData'))


res.train00 = res.train
res.proj00 = res.proj

rm(res.train, res.proj)

wave_vec = c('delta','omicron')
fcast.tm_vec = c('1wktopeak','2wktopeak')
measures_vec = c('Cases','Deaths') # 'Infections',
date.start = '2020/03/15' %>% as.Date()

##### FUGURE 2 ##### 

pdf(paste0(dir_plot,'Fig2ci80_proj_case.death_ex.pdf'),width = 10, height = 8)
pp = list(); ip = 1
for(loc.t in locs[1:4]){
  
  da.full.t = DAT.EPI[country == loc.t] %>% dcast(., date + year + week ~ data.type, value.var = 'value')
  da.full.t$date = da.full.t$date %>% as.Date()
  da.full.t = da.full.t[date >= date.start]
  
  for(iw in 1:length(wave_vec)){
    
    wave.t = wave_vec[iw]
    wave.start.t = parm.bound_vec[parm == 'wave.start' & type == wave.t & location == loc.t]$date.start 
    res.proj.t = res.proj00[loc == loc.t & wave == wave.t]
    wks.fcast = res.proj.t$Week.start %>% unique()
    wks.fcast.a = res.proj.t %>% filter(fcast.tm == '2wktopeak') %>% .$Week.start %>% unique()
    wks.fcast.b = res.proj.t %>% filter(fcast.tm == '1wktopeak') %>% .$Week.start %>% unique()
    
    # res.train.t = res.train00[loc == loc.t & # loc == gsub(' ','', loc.t) & 
    #                             # seasonality == 'No seasonality' &
    #                             wave == wave.t & 
    #                             state %in% c('infection','case','death') &
    #                             !Week.start %in% wks.fcast]
    res.train.t = res.train00 %>% filter((loc == loc.t & wave == wave.t & fcast.tm == '2wktopeak' & Week.start >= as.Date(wave.start.t) &
                                            state %in% c('infection','case','death') &
                                            !Week.start %in% wks.fcast.a) |
                                           (loc == loc.t & wave == wave.t & fcast.tm == '1wktopeak' & Week.start >= as.Date(wave.start.t) &
                                              state %in% c('infection','case','death') &
                                              !Week.start %in% wks.fcast.b)
                                         
    )
    
    res.train.t = dcast(res.train.t, loc + fcast.tm + state + Week.start ~ variable, value.var = 'value')
    res.train.t$measure = factor(res.train.t$state, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    wks.train = res.train.t$Week.start %>% unique() %>% sort
    da.t = da.full.t[date <= max(wks.train)] %>% melt(id.var = c('date','year','week')) %>% 
      setnames(c('date', 'variable', 'value'), c('Week.start','measure','obs'))
    da.t$measure = factor(da.t$measure, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    res.train.t = merge(res.train.t, da.t, by = c('Week.start', 'measure'), all = T)
    
    da.t = da.full.t[date %in% wks.fcast] %>% melt(id.var = c('date','year','week')) %>% 
      setnames(c('date', 'variable', 'value'), c('Week.start','measure','obs'))
    da.t$measure = factor(da.t$measure, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    res.proj.t = dcast(res.proj.t, loc + fcast.tm + measure + Week.start ~ variable, value.var = 'value')
    res.proj.t = merge(res.proj.t, da.t, by = c('Week.start', 'measure'), all = T)
    
    res.train.t$fcast.tm = factor(res.train.t$fcast.tm, levels = c('2wktopeak','1wktopeak'), labels = c('2 weeks before peak incidence', '1 week before peak incidence'))
    res.proj.t$fcast.tm = factor(res.proj.t$fcast.tm, levels = c('2wktopeak','1wktopeak'), labels = c('2 weeks before peak incidence', '1 week before peak incidence'))
    
    # train1, train2, proj1, proj2
    eval(parse(text = paste('train', iw, '=res.train.t', sep = '')))
    eval(parse(text = paste('proj', iw, '=res.proj.t', sep = '')))
  } # ifcast
  
  pp.t = getPlotProj5ci80(train1, train2, proj1, proj2, LettStart.t = ip)
  pp[[ip]] = pp.t[[1]]
  ip = ip + 1
  pp[[ip]] = pp.t[[2]]
  ip = ip + 1
} # loc.t

p = grid.arrange(# top = textGrob(ptitle.t, gp=gpar(fontsize=11,font=1)),
  grobs = list(pp[[1]], pp[[2]], pp[[3]], pp[[4]], pp[[5]], pp[[6]], pp[[7]], pp[[8]]),
  layout_matrix = matrix(1:8, nrow = 4, byrow=T)
)
print(p)
dev.off()
##### FUGURE 2 END ##### 


##### FUGURE S3 ##### 
pdf(paste0(dir_plot,'FigS3ci80_proj_case.death_others.pdf'),width = 10, height = 10)
pp = list(); ip = 1
for(loc.t in locs[5:9]){
  
  da.full.t = DAT.EPI[country == loc.t] %>% dcast(., date + year + week ~ data.type, value.var = 'value')
  da.full.t$date = da.full.t$date %>% as.Date()
  da.full.t = da.full.t[date >= date.start]
  
  for(iw in 1:length(wave_vec)){
    
    wave.t = wave_vec[iw]
    wave.start.t = parm.bound_vec[parm == 'wave.start' & type == wave.t & location == loc.t]$date.start 
    res.proj.t = res.proj00[loc == loc.t & wave == wave.t]
    wks.fcast = res.proj.t$Week.start %>% unique()
    wks.fcast.a = res.proj.t %>% filter(fcast.tm == '2wktopeak') %>% .$Week.start %>% unique()
    wks.fcast.b = res.proj.t %>% filter(fcast.tm == '1wktopeak') %>% .$Week.start %>% unique()
    
    # res.train.t = res.train00[loc == loc.t & # loc == gsub(' ','', loc.t) & 
    #                             # seasonality == 'No seasonality' &
    #                             wave == wave.t & 
    #                             state %in% c('infection','case','death') &
    #                             !Week.start %in% wks.fcast]
    res.train.t = res.train00 %>% filter((loc == loc.t & wave == wave.t & fcast.tm == '2wktopeak' & Week.start >= as.Date(wave.start.t) &
                                            state %in% c('infection','case','death') &
                                            !Week.start %in% wks.fcast.a) |
                                           (loc == loc.t & wave == wave.t & fcast.tm == '1wktopeak' & Week.start >= as.Date(wave.start.t) &
                                              state %in% c('infection','case','death') &
                                              !Week.start %in% wks.fcast.b)
                                         
    )
    
    res.train.t = dcast(res.train.t, loc + fcast.tm + state + Week.start ~ variable, value.var = 'value')
    res.train.t$measure = factor(res.train.t$state, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    wks.train = res.train.t$Week.start %>% unique() %>% sort
    da.t = da.full.t[date <= max(wks.train)] %>% melt(id.var = c('date','year','week')) %>% 
      setnames(c('date', 'variable', 'value'), c('Week.start','measure','obs'))
    da.t$measure = factor(da.t$measure, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    res.train.t = merge(res.train.t, da.t, by = c('Week.start', 'measure'), all = T)
    
    da.t = da.full.t[date %in% wks.fcast] %>% melt(id.var = c('date','year','week')) %>% 
      setnames(c('date', 'variable', 'value'), c('Week.start','measure','obs'))
    da.t$measure = factor(da.t$measure, levels = c('infection','case','death'), labels = c('Infections','Cases','Deaths'))
    res.proj.t = dcast(res.proj.t, loc + fcast.tm + measure + Week.start ~ variable, value.var = 'value')
    res.proj.t = merge(res.proj.t, da.t, by = c('Week.start', 'measure'), all = T)
    
    res.train.t$fcast.tm = factor(res.train.t$fcast.tm, levels = c('2wktopeak','1wktopeak'), labels = c('2 weeks before peak incidence', '1 week before peak incidence'))
    res.proj.t$fcast.tm = factor(res.proj.t$fcast.tm, levels = c('2wktopeak','1wktopeak'), labels = c('2 weeks before peak incidence', '1 week before peak incidence'))
    
    # train1, train2, proj1, proj2
    eval(parse(text = paste('train', iw, '=res.train.t', sep = '')))
    eval(parse(text = paste('proj', iw, '=res.proj.t', sep = '')))
  } # ifcast
  
  pp.t = getPlotProj5ci80(train1, train2, proj1, proj2, LettStart.t = ip)
  pp[[ip]] = pp.t[[1]]
  ip = ip + 1
  pp[[ip]] = pp.t[[2]]
  ip = ip + 1
} # loc.t

p = grid.arrange(# top = textGrob(ptitle.t, gp=gpar(fontsize=11,font=1)),
  grobs = list(pp[[1]], pp[[2]], pp[[3]], pp[[4]], pp[[5]], pp[[6]], pp[[7]], pp[[8]]),
  layout_matrix = matrix(1:8, nrow = 4, byrow=T)
)
print(p)
dev.off()
##### FUGURE S3 END ##### 
