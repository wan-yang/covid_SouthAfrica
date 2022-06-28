# driver script to run retrospective predictions for each location
# done on an HPC, or local machine

date.tag = '_2022-03-07'

doFcast = T # whether to do projection 
realtimeFcast = F

num_runs = 100  # number of runs 

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


dir_data = '../data/'
dir_code = '../scripts/'
dir_res = '../results/test/'

if(!file.exists(dir_res))
  dir.create(dir_res, recursive = T)

source(paste0(dir_code,'SEIRS.R'))
source(paste0(dir_code,'EAKF_util.R'))
source(paste0(dir_code,'EAKF_rproj.R'))


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
DAT.SN[location=="Nothern Cape"]$location = "Northern Cape" # mis-spell
# the list of states 
locs = DAT.EPI$country %>% unique()

# read timing of peak cases for each wave - for setting the time of retrospetive projection
tm_peak = read.csv(paste0(dir_code,'tm_peaks_sa.csv'))  %>% data.table()
tm_peak$date.start = tm_peak$date.start %>% as.Date(format = '%m/%d/%y')
tm_peak$date.end = tm_peak$date.end %>% as.Date(format = '%m/%d/%y')
tm_peak$wk.case.max = tm_peak$wk.case.max %>% as.Date(format = '%m/%d/%y')

# read the parm bounds for different stage
parm.bound_VEC = read.csv(paste0(dir_code, 'parm.bounds.csv'))  %>% data.table()
parm.bound_vec = parm.bound_VEC[country == 'South Africa']
parm.bound_vec$lwr = parm.bound_vec$lwr %>% as.numeric()
parm.bound_vec$upr = parm.bound_vec$upr %>% as.numeric()

# set parameters
# population size, etc
N = 1e6; # per 1 M
num_gr = num_obs = length(N); # no age structure
num_ens = 500
epi.model = 'SEIRSV' # susceptible-exposed-infectious-recovered-susc
stoch = T # run the model stochastically
seasonality = T; # include seasonality

# SR
doSR = T
percSRmajor = .1
SR.perc = .05
SR.perc.local = .03;
SR.perc.full = 0.05; # increase from .03
SR.perc.imm = .08; # for immune evasion, probing on S
SR.perc.extra = .12; # decrease from .15
SR.perc.Extra = .2
SR.perc.extra.lt = .1; # for more weeks
SR.perc.EXTRA = .5; # RE-NEW THE SYSTEM

SR.var.local= c('beta','Tei','Tir','Trs','Td.mean','Td.sd','p.mob','alpha','ifr')
SR.var.full= c('beta','Tir','ifr','alpha') # ,'alpha','ifr'
# 3/7/22 add p.mob
SR.var.full= c('beta','Tir','ifr','alpha', 'p.mob')
SR.var.tx = c('beta','Tir')

donotUpdateS = T # do not allow the filter to update S during first wave

rednUpdateEI = T # do not allow or reduce the level allowed, the filter to update E or I - it takes OEVr all efforts

redn.priority = 1  # no prioritization of vac

{
  fn_getWkLowIFR = function(idx.all){
    consecutive =  idx.all[-1] - idx.all[-length(idx.all)]
    i.div = which(consecutive >1)
    if(length(i.div) > 0){
      w.div = idx.all[which(consecutive >1)]
      grps = list()
      if(length(i.div) == 1){
        grps[[1]] = idx.all[1]: w.div[1]
        grps[[2]] =idx.all[i.div[1]+1]: tail(idx.all,1)
      } else {
        for(id in 1: length(i.div)){
          if(id == 1){
            grps[[id]] = idx.all[1]: w.div[id]
          } else if (id == length(i.div)){
            # both before and after
            
            grps[[id]] = idx.all[(i.div[id-1]+1): i.div[id]]
            grps[[id+1]] = idx.all[i.div[id]+1]: tail(idx.all,1)
          } else {
            grps[[id]] = idx.all[(i.div[id-1]+1): i.div[id]] # (idx.all[i.div[id-1]+1]) : w.div[id]
          }
          
        }
      }
      
      dur = lapply(grps, length) %>% unlist
      igrs = which(dur > 5)
      
      WkLowIFR = NULL
      if(length(igrs) > 0){
        for(igr in igrs){
          WkLowIFR = c(WkLowIFR, grps[[igr]])
        }
      }
    } else {
      WkLowIFR = idx.all
    }
    
    WkLowIFR
  }
}

# for adjusting time from infection to deaths
tm2death_adj_beta.mn = -5 # shorter time lag, ~10 days (lag b/w case and death) vs 15 days
tm2death_adj_beta.sd = 0 # keep the wider distribution
tm2death_adj_omicron.mn = 15 # much longer time lag, ~33 days (lag b/w case and death) vs 15 days
tm2death_adj_omicron.sd = 7 # also widen the distribution
tm.to.death.max_beta = 40
tm.to.death.max_omicron = 80


# for test run
loc.t = 'Gauteng'
ir = tno = 1 
dummy_wk2peak = 2 # set time of forecast, number of week before peak cases
dummy_wave2fcast = 1 # set wave to forecast, 1 = delta
# for test run

for(loc.t in locs){  # locs[dummy_iloc]
  print(loc.t)
  
  da.t = DAT.EPI[country == loc.t] %>% dcast(., date + year + week ~ data.type, value.var = 'value')
  da.mob.t = DAT.MOB[country == loc.t & data.type == mob.type] %>% setnames('value', 'mob')
  da.t$date = da.t$date %>% as.Date()
  da.mob.t$date = da.mob.t$date  %>% as.Date()
  da.t = merge(da.t, da.mob.t, x.all = T, by = c('date', 'year', 'week'))
  
  
  parm.bound_vec = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location)]
  parm.bound_vec$lwr = parm.bound_vec$lwr %>% as.numeric()
  parm.bound_vec$upr = parm.bound_vec$upr %>% as.numeric()
  parm.bound_vec$date.start = parm.bound_vec$date.start %>% as.Date()
  parm.bound_vec$date.end = parm.bound_vec$date.end %>% as.Date()
  
  tm_peak.t = tm_peak[loc == loc.t]
  # get the time series based from start to end of the wave
  fcast.start = (tm_peak.t[dummy_wave2fcast]$wk.case.max %>% as.Date) - 7 * (dummy_wk2peak - 1) # 2 weeks before the peak, often haven't got the S and beta estimates stable yet as the wave moves so fast
  
  date.end = tm_peak.t[dummy_wave2fcast]$date.end %>% as.Date # fcast.start + (nfcast - 1) * 7
  nfcast = ((date.end - fcast.start) / 7 + 1) %>% as.numeric()
  wave.t = tm_peak.t[dummy_wave2fcast]$variant
  
  date.start = parm.bound_vec[parm == 'wk.start']$date.start %>% as.Date
  
  da.t = da.t[date >= date.start]
  da.t = da.t[date <= date.end]
  # da.t = da.t[date >= date.start & case > 0]
  da.t = da.t[order(date)]
  # some early week for death is missing set to na
  da.t[is.na(da.t)] = 0
  
  rel.mob = da.t$mob %>% as.matrix()
  
  da.vacc = DAT.VAC[location == loc.t]
  vax.start = min(as.Date(da.vacc$date)) %>% as.Date()
  # seasonality
  relR0 = DAT.SN[location == loc.t] %>% .[order(week)]
  relR0 = matrix(as.numeric(relR0$value), nrow = nrow(relR0), ncol = num_ens)
  
  VE1wt = .85
  VE2wt = .95  # higher b/c we are using mortality data too
  
  VE1gamma = .85; VE2gamma = .95 # just as place holder, update accordingly
  
  VE1beta = parm.bound_vec[parm == 'VE1beta']$lwr %>% as.numeric() # .85
  VE2beta = parm.bound_vec[parm == 'VE2beta']$lwr %>% as.numeric() # .95  # higher b/c we are using mortality data too
  
  VE1delta = parm.bound_vec[parm == 'VE1delta']$lwr  %>% as.numeric() #  .5
  VE2delta = parm.bound_vec[parm == 'VE2delta']$lwr %>% as.numeric() #  .8  # higher b/c we are using mortality data too
  
  # VE1omicron = parm.bound_vec[parm == 'VE1omicron']$lwr  %>% as.numeric() #  .5
  # VE2omicron = parm.bound_vec[parm == 'VE2omicron']$lwr %>% as.numeric() #  .8  # higher b/c we are using mortality data too
  VE1omicron = .1
  VE2omicron = .35
  
  # for getting the time-specific/variant-specific VE
  date.beta = (parm.bound_vec[parm=='wave.start' & type == 'beta']$date.start %>% as.Date) + 14 
  date.delta = (parm.bound_vec[parm=='wave.start' & type == 'delta']$date.start %>% as.Date) + 14 
  date.omicron = (parm.bound_vec[parm=='wave.start' & type == 'omicron']$date.start %>% as.Date) + 14
  
  dates_wt = seq((parm.bound_vec[parm=='wave.start' & type == 'wt']$date.start %>% as.Date), #  + 14
                 (parm.bound_vec[parm=='wave.start' & type == 'wt']$date.end %>% as.Date), 
                 by = 'day') + 14 # shift by 14 days for ve development?
  dates_beta = seq((parm.bound_vec[parm=='wave.start' & type == 'beta']$date.start %>% as.Date), #  + 14
                 (parm.bound_vec[parm=='wave.start' & type == 'beta']$date.end %>% as.Date), 
                 by = 'day') + 14 # shift by 14 days for ve development?
  dates_delta = seq((parm.bound_vec[parm=='wave.start' & type == 'delta']$date.start %>% as.Date), #  + 14
                 (parm.bound_vec[parm=='wave.start' & type == 'delta']$date.end %>% as.Date), 
                 by = 'day') + 14 # shift by 14 days for ve development?
  dates_omicron = seq((parm.bound_vec[parm=='wave.start' & type == 'omicron']$date.start %>% as.Date), #  + 14
                 (parm.bound_vec[parm=='wave.start' & type == 'omicron']$date.end %>% as.Date), 
                 by = 'day') + 14 # shift by 14 days for ve development?
  
  dates_alpha = NULL
  dates_gamma = NULL
  
  if(F){
    dates_alpha = seq((parm.bound_vec[parm=='wave.start' & type == 'alpha']$date.start %>% as.Date), #  + 14
                      (parm.bound_vec[parm=='wave.start' & type == 'alpha']$date.end %>% as.Date), 
                      by = 'day') + 14 # shift by 14 days for ve development?
    dates_gamma = seq((parm.bound_vec[parm=='wave.start' & type == 'gamma']$date.start %>% as.Date), #  + 14
                      (parm.bound_vec[parm=='wave.start' & type == 'gamma']$date.end %>% as.Date), 
                      by = 'day') + 14 # shift by 14 days for ve development?
  }

  
  p.seed.max = parm.bound_vec[parm == 'p.seed.max']$lwr  %>% as.numeric() # 
  
  seed =parm.bound_vec[parm == 'seed']$lwr  %>% as.numeric() # .1
  
  seed_max = (da.t$case[1] + .01) * p.seed.max # 10000
  tm_largerVar = parm.bound_vec[parm == 'tm_largerVar']$lwr %>% as.numeric() # number of initial weeks to have larger OEV
  pOEV = parm.bound_vec[parm == 'pOEV']$lwr %>% as.numeric()
  tighterOEV4death = parm.bound_vec[parm == 'tighterOEV4death']$lwr %>% as.logical()

  p.mob_bounds = c(.5, 1.5); # scaling for mobility
  Td.mean_bounds = parm.bound_vec[parm == 'Td.mean' & type == 'initialization'] %>% .[,c('lwr','upr')] %>% unlist # c(5,8) # mean Td: reporting delay
  Td.mean_SRbounds = parm.bound_vec[parm == 'Td.mean' & type == 'SR'] %>% .[,c('lwr','upr')] %>% unlist #  c(5, 7)
  Td.sd_bounds = c(1,3) # Td, sd: reporting delay sd 
  imm_bounds = c(2, 3) * 365
  

  beta_bounds = parm.bound_vec[parm == 'beta' & type == 'initialization'] %>% .[,c('lwr','upr')] %>% unlist
  alpha_bounds = parm.bound_vec[parm == 'alpha' & type == 'initialization'] %>% .[,c('lwr','upr')] %>% unlist
  ifr_bounds = parm.bound_vec[parm == 'ifr' & type == 'initialization'] %>% .[,c('lwr','upr')] %>% unlist
  
  
  # time period need filtering restriction on S, E, or I
  tm_reset_cntSR.S = NULL # week to reset cntSR.S so the filter can probe on S to detect imm evasion again
  tm_redn = parm.bound_vec[parm == 'S' & type == 'filtering' & period == 'reset cntSR.S'] %>% .[,c('date.start','date.end')] 
  if(any(!is.na(tm_redn$date.start))){
    for(i in 1:nrow(tm_redn)){
      tm_reset_cntSR.S = append(tm_reset_cntSR.S, seq(as.Date(tm_redn[i]$date.start), as.Date(tm_redn[i]$date.end), by = 'day'))
    }
  }
  
  tm_rednUpdateS = NULL
  tm_redn = parm.bound_vec[parm == 'S' & type == 'filtering' & period == 'no.imm.escape'] %>% .[,c('date.start','date.end')] 
  tm_redn = tm_redn[complete.cases(tm_redn)]
  if(any(!is.na(tm_redn$date.start))){
    for(i in 1:nrow(tm_redn)){
      tm_rednUpdateS = append(tm_rednUpdateS, seq(as.Date(tm_redn[i]$date.start), as.Date(tm_redn[i]$date.end), by = 'day'))
    }
  }
  
  
  tm_rednUpdateEI = NULL
  tm_redn = parm.bound_vec[parm == 'EI' & type == 'filtering' & period == 'voc.ini'] %>% .[,c('date.start','date.end')] 
  tm_redn = tm_redn[complete.cases(tm_redn)]
  if(any(!is.na(tm_redn$date.start))){
    for(i in 1:nrow(tm_redn)){
      tm_rednUpdateEI = append(tm_rednUpdateEI, seq(as.Date(tm_redn$date.start), as.Date(tm_redn$date.end), by = 'day'))
    }
  }
  
  
  ## SET OBSERVATIONAL ERRORS FOR CASES AND DEATHS
  obs_i = (da.t$case) %>% as.matrix() 
  obs_vars_i = obs_i
  for(j in 1:num_obs){
    tmp=rep(0,nrow(da.t))
    for (i in 3:nrow(da.t)){
      tmp[i]=mean(obs_i[(i-2):(i-0),j]);
    }
    obs_vars_i[,j]= (c(rep(N/1000,tm_largerVar),rep(N/1e3,nrow(da.t)-tm_largerVar)) + (tmp^2)/50) * pOEV;
    
  }
  
  obs_d = (da.t$death) %>% as.matrix() 
  obs_vars_d = obs_d
  for(j in 1:num_obs){
    tmp=rep(0,nrow(da.t))
    for (i in 3:nrow(da.t)){
      tmp[i]=mean(obs_d[(i-2):(i-0),j]);
    }
    
    if(tighterOEV4death){
      obs_vars_d[,j]= (c(rep(N/1e4,tm_largerVar),rep(N/5e4,nrow(da.t)-tm_largerVar)) + 
                         pmin((tmp^2)/10, tmp*5)
      ) * pOEV;
    } else {
      obs_vars_d[,j]= (c(rep(N/1e4,tm_largerVar),rep(N/1e4,nrow(da.t)-tm_largerVar)) + 
                         pmin((tmp^2)/10, tmp*20)
      ) * pOEV;
    }
  }
  

  weeks = da.t$week # for seasonality if applicable
  Week.starts = da.t$date
  
  fcast.wk.starts = as.Date(fcast.start) + seq(0, length.out = nfcast, by = 7)
  weeks.fcast = MMWRweek(fcast.wk.starts)['MMWRweek'] %>% unlist
  
  # do 5 runs
  for(ir in tno){ # tno
    
    print(paste('run', ir))
    
    source(paste0(dir_code,'set_tm2event.R'))
    
    So=t(lhs(num_ens,rect = rbind(cbind(.99, 1) * N, # S0
                                  cbind(seed_max/20,seed_max/2), # E0
                                  cbind(seed_max/20,seed_max/2), # I0
                                  cbind(0,seed_max/100) # deaths0
    )))
    
    
    S0 = So[1:num_gr,,drop=F]
    E0 = So[1:num_gr+num_gr,,drop=F]
    I0 = So[1:num_gr+num_gr*2,,drop=F]
    D0 = So[1:num_gr+num_gr*3,,drop=F]
    
    newItot = I0; newIobs = I0; 
    rownames(S0)=paste0('S',1:num_gr); 
    rownames(E0)=paste0('E',1:num_gr); 
    rownames(I0)=paste0('I',1:num_gr); 
    rownames(D0)=paste0('death',1:num_gr);
    rownames(newItot)=paste0('newItot',1:num_gr); 
    rownames(newIobs)=paste0('newIobs',1:num_gr); 
    
    
    parm.bounds = rbind(beta_bounds, # beta for all loc's
                        c(2,5), # Tei: time from exposed to infectious: incubation time mean = 4
                        c(2,5), # Tir: time from infectous to not (remOEVd)
                        imm_bounds, # immunity period, Trs
                        # c(3,8), # mean Td: reporting delay 
                        # make the range smaller to constrain the model better
                        Td.mean_bounds, # c(5,7), # mean Td: reporting delay
                        Td.sd_bounds, # c(1,3), # Td, sd: reporting delay sd
                        p.mob_bounds, # scaling for mobility
                        alpha_bounds, # reporting rate
                        ifr_bounds # infection fatality risk
    )
    parm.names = c('beta','Tei','Tir','Trs', 'Td.mean', 'Td.sd', 'p.mob','alpha', 'ifr')
    
    rownames(parm.bounds) = parm.names
    parm.bounds
    
    parm0=t(lhs(num_ens,parm.bounds)); rownames(parm0)=rownames(parm.bounds)
    
    
    STATE0=rbind(S0, E0, I0, D0, newIobs, newItot, parm0)
    state.names=rownames(STATE0)
    idx.obs_i= which(state.names == 'newIobs1')  # the random tests are testing the prevalence of infectious - I
    idx.obs_d= which(state.names == 'death1')  # the random tests are testing the prevalence of infectious - I
    idx.newItot = which(state.names == 'newItot1') 
    idx.e = which(state.names == 'E1') 
    idx.i = which(state.names == 'I1') 
    
    num_state = 4 + 2
    
    DAbounds = rbind(matrix(c(rep(0,num_state * num_gr), rep(N,num_state)),num_state * num_gr,2),
                      cbind(parm.bounds[,1]*.5, parm.bounds[,2]*1.5)) # cbind(parm.bounds[,1]*.5, parm.bounds[,2]*1.5)
    rownames(DAbounds)=state.names
    DAbounds[c('E1','I1'),2] = N * .15 # / 20
    DAbounds['death1',2] = N / 100 # 200
    DAbounds['S1',1] = N / 10
    DAbounds[c('newItot1','newIobs1'),2] = N * .2
    
    DAbounds['Td.mean',1] = parm.bounds['Td.mean',1] * .6
    DAbounds['Td.mean',2] = parm.bounds['Td.mean',2] * 1.1
    DAbounds['Trs',1] = 200
    
    DAbounds['p.mob',] =  parm.bounds['p.mob',]
    
    # place holder for SR bounds
    SRbounds = parm.bounds # cbind(parm.bounds[,1]*.9, parm.bounds[,2]*1.1) # 
    rownames(SRbounds)=parm.names
    SRbounds['Td.mean',] = Td.mean_SRbounds
    
    tm.ini=1; tmstep=7; newI.previous = NULL; inflat=1.03; state0=STATE0
    
    severity['death',] = STATE0['ifr',]
    
    # model training
    # tmp = try(load(paste0(dir_res, loc.t,'_train_r',ir,'.RData')))
    tmp = try(load(paste0(dir_res, gsub(' ','',loc.t),'_train.proj_',wave.t,'_',dummy_wk2peak,'wktopeak','_r',ir,'.RData')))
    if(class(tmp) == 'try-error'){  # model haven't been trained, do it first
      print('running...')
      train.proj = EAKF_rproj(epi.model=epi.model, num_ens=num_ens,inflat=1.03, 
                              obs_i=obs_i, obs_vars_i=obs_vars_i, # case
                              obs_d=obs_d, obs_vars_d=obs_vars_d,
                              weeks=weeks,Week.starts=Week.starts,
                              parm.bounds=parm.bounds, DAbounds=DAbounds, SRbounds=SRbounds, 
                              parm.names = rownames(parm.bounds), rel.mob = rel.mob,
                              state0=STATE0, state.names=rownames(STATE0),
                              severity = severity,
                              tm.ini=1, tmstep=7,
                              newI.previous = NULL,
                              parm.bound_vec = parm.bound_vec,
                              # for the projection
                              weeks.fcast, # week of the year to get seasonality 
                              fcast.wk.starts
      )
      train.proj$fcast.start.week = fcast.start
      train.proj$wave = wave.t
      train.proj$loc = loc.t
      save(train.proj, file = paste0(dir_res, gsub(' ','',loc.t),'_train.proj_',wave.t,'_',dummy_wk2peak,'wktopeak','_r',ir,'.RData'))
      
    }  
    
    
  }
} # end for this location
  

if(F){
  theme.t = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,3,0)), 
                  strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                  axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                  axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                  axis.text.x = element_text(size=8,angle = 30),
                  plot.margin=unit(c(c(.3, 1, .1, .5)), units="line"), # top, right, bottom, left
                  legend.title = element_text(size=8), legend.text=element_text(size=8),
                  legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(-10,-10,-10,-10),
                  legend.key.size = unit(.2, 'cm'), #change legend key size
                  legend.key.height = unit(.2, 'cm'), #change legend key height
                  legend.key.width = unit(.2, 'cm')) #change legend key width)
  getPlotProj = function(train.t, proj.t){
    
    p = ggplot(train.t) +
      geom_line(aes(x = Week.start, y = median), color = 'blue') +  # no ctrl
      # geom_ribbon(aes(x = Week.start, ymin = ci95.lwr, ymax = ci95.upr), fill = 'blue', alpha = .1) +
      geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .2) +
      geom_line(data=proj.t, aes(x = Week.start, y = median), color = 'red') +  # no ctrl
      # geom_ribbon(data=proj.t, aes(x = Week.start, ymin = ci95.lwr, ymax = ci95.upr), fill = 'red', alpha = .1) +
      geom_ribbon(data=proj.t, aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .2) +
      geom_vline(data = train.t, aes(xintercept = max(train.t$Week.start)), linetype = 'dashed')+
      geom_point(data = train.t, mapping = aes(x = Week.start, y=obs)) + 
      facet_rep_wrap(~ loc, scales = 'free_y', repeat.tick.labels = T, ncol = 1) + 
      labs(x = 'Week Start', y = 'Estimate/Projection (median, IQR)') +
      scale_x_date(breaks = seq(min(train.t$Week.start)-7, max(proj.t$Week.start)+7, by = 'week'),
                   labels = format(seq(min(train.t$Week.start)-7, max(proj.t$Week.start)+7, by = 'week'),'%m/%d')) +
      theme_minimal() +  theme.t # + theme(strip.text = element_text(size = 10), axis.title = element_text(size =10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10,angle = 45))
    
    p
  }
  
  train.proj$newVstat
  train.proj$newVstat.Rtx
  plot(train.proj$xpost_mean$beta, type = 'l')
  plot(train.proj$xpost_mean$S1, type = 'l')
  plot(train.proj$xpost_mean$alpha, type = 'l')
  # View(train.proj$xpost_mean)
  xpost.t = train.proj$xpost_mean
  # View(xpost.t)
  tail(train.proj$Rt_stats)
  
  plot(train.proj$xpost_mean$newIobs1, type = 'l')
  
  da.full.t = DAT.EPI[country == loc.t] %>% dcast(., date + year + week ~ data.type, value.var = 'value')
  da.full.t$date = da.full.t$date %>% as.Date()
  da.full.t = da.full.t[date >= as.Date(date.start) & date <= as.Date(tail(fcast.wk.starts,1))]
  train.t = train.proj$states_stats[state == 'newIobs1' & Week.start < fcast.start]
  train.t = merge(train.t, da.t[,c('date', 'case'), with=F] %>% setnames(c('date', 'case'), c('Week.start', 'obs')), 
                  by = 'Week.start')
  proj.t = train.proj$fcast_stats[measure == 'Cases']
  proj.t = merge(proj.t, da.full.t %>% setnames(c('date', 'case'), c('Week.start', 'obs')), by = 'Week.start')
  train.t$loc = loc.t
  proj.t$loc = loc.t
  getPlotProj(tail(train.t, 20), proj.t)
  
}


