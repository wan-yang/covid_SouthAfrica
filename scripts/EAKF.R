# EAKF: for model-inference


library("truncnorm"); library("tgp"); library('mvtnorm'); # for lhs
library("MASS"); # for multivariate normal distribution

# first version, without considering ve for natural infection + 1dose
EAKF = function(epi.model=epi.model, num_ens=num_ens,inflat=1.03, 
                obs_i=obs_i, obs_vars_i=obs_vars_i, # case
                obs_d=obs_d, obs_vars_d=obs_vars_d,
                weeks=weeks,Week.starts=Week.starts,
                parm.bounds=parm.bounds, DAbounds=DAbounds, SRbounds=SRbounds, 
                parm.names = rownames(parm.bounds), rel.mob = rel.mob,
                state0=state0, state.names=rownames(state0),
                severity = severity,
                tm.ini=1, tmstep=7,
                newI.previous = NULL,
                parm.bound_vec = parm.bound_vec
){
  # parm.bounds: prior bounds for the parms
  # x.prior: priors for all state variables, and model parameters
  # obs_i: observations
  # idx.obs: idx for obseverations to filter through
  
  # save inital condition passed in
  state00 = state0
  DAbounds00=DAbounds;
  SRbounds00=SRbounds;
  newI.previous00 = newI.previous;
  
  DAbounds.t = DAbounds;
  SRbounds.t = SRbounds;
  
  
  if(!exists('redn.priority')) redn.priority = 1
  
  if(is.null(dim(obs_i))){ # only 1 observation (no subgroup)
    num_times_i=length(obs_i);  
  } else {
    num_times_i = nrow(obs_i)
  }
  if(is.null(dim(obs_d))){ # only 1 observation (no subgroup)
    num_times_d=length(obs_d);  
  } else {
    num_times_d = nrow(obs_d)
  }
  num_var = nrow(state0)
  
  cumlike=NULL; # cummulative likelihood
  
  xprior=array(0,c(num_var,num_ens,num_times_i+1));
  xpost=array(0,c(num_var,num_ens,num_times_i));
  dimnames(xpost)[1] = list(state.names)
  
  # to estimate daily cases
  xprior.daily = matrix(0,(num_times_i+1)*tmstep, num_ens);
  xpost.daily = matrix(0,(num_times_i)*tmstep, num_ens);
  
  Eprior = Iprior = Sprior = numeric(num_times_i+1)
  Epost = Ipost = Spost = Itotpost = numeric(num_times_i)
  
  stat.dS = NULL  # to record the changes in S - immune evasion
  s.lwr.t = .3; s.upr.t = .8 # level of imm evasion, for SR on S
  # cntSR.S = 0  # need to be reset for subsequent imm evasive wave
  cntSR.Scut = 4 # no more than 4
  cntSR.S = cntSR.Scut  # set the initial value to the max so that the weeks can be specified seperately
  dS.cut.mn = .85; # threshold, if >75% already, no more major SR on this one
  dS.cut.ens = .85; 
  
  {
    dist_tm.to.detect = NULL
    for(ii in 1:num_ens){
      tmp = generation.time(dist_tm.to.detect.name,c(state0['Td.mean',ii],state0['Td.sd',ii]),truncate = tm.to.detect.max)
      dist_tm.to.detect=cbind(dist_tm.to.detect,tmp$GT[-1]); 
    }
    
    
    dist_tm.to.death = NULL  # time to death
    for(ii in 1:num_ens){
      # tmp = generation.time(dist_tm.to.death.name,c(state0['Td.mean',ii]+diff.dd,state0['Td.sd',ii]+diff.sd2),truncate = tm.to.death.max)
      # do not link it to Td
      tmp = generation.time(dist_tm.to.death.name,c(tm.to.outcome.mn['tm.to.death',ii], tm.to.outcome.sd['tm.to.death',ii]),truncate = tm.to.death.max)
      dist_tm.to.death=cbind(dist_tm.to.death,tmp$GT[-1]); 
    }
    if(tm.to.deathFromDiag){ # if the distribution of time to death is from diagnosis, not infectious
      # add time from infectious to diagnosis
      dist_tm.to.death = rbind(matrix(0,round(tm.to.diag,0),num_ens),dist_tm.to.death)
    }
  }
  
  # integrate forward 1 step to get the prior
  tm_strt = tm.ini+1; tm_end = tm.ini + tmstep;
  cur.wk = weeks[1];
  seed = seed
  vdate.t = Week.starts[1] %>% as.Date()
  
  severity.t = severity
  
  tmp = getVE(date.t = vdate.t,
              dates_wt = dates_wt,
              dates_alpha = dates_alpha,
              dates_beta = dates_beta,
              dates_gamma = dates_gamma,
              dates_delta = dates_delta, 
              dates_omicron = dates_omicron,
              VE1wt = VE1wt,
              VE2wt = VE2wt,  # higher b/c we are using mortality data too
              VE1beta = VE1beta,
              VE2beta = VE2beta,  # higher b/c we are using mortality data too
              VE1gamma = VE1gamma,
              VE2gamma = VE2gamma,  # higher b/c we are using mortality data too
              VE1delta = VE1delta,
              VE2delta = VE2delta,  # higher b/c we are using mortality data too
              VE1omicron = VE1omicron,
              VE2omicron = VE2omicron
  )
  VE1 = tmp$VE1; VE2 = tmp$VE2
  # get prior for week 1
  {
    if(!is.null(newI.previous) & vdate.t >= vax.start){
      tm.t = nrow(newI.previous)
      # cumI.t = apply(newI.previous00[,,1:(dim.t[3]-14),drop=F],c(1,2),sum) #  %>% apply(1, median) # excl last two weeks and get the median
      # t1 = (as.Date('2020/12/14') - as.Date('2020/3/1')) %>% as.numeric() # 1st day of vaccination
      # 2/5/21 set t1 to 1 yr given the slow rollout
      t1 = 365
      cumI.t = colSums(newI.previous[pmax(1,tm.t-t1) : (tm.t),]) #  %>% apply(1, median) # excl last two weeks and get the median
      # only count the last 12 months? so as the epidemic unfold, you don't over count cum infect?
      # higher infection rate for the priority group
      # tm.t = pmax(1, tm.t - t1 + 1) # re-aline timing with start of vac
      tm.imm = 365*2.5 # assume 3 yr immunity
      p.imm.wane.max = .8; k = .015  # 1/tm.imm  
      # since only the last year is included, should be:
      p.imm.wane = 1 - p.imm.wane.max / (1+exp(-k*(pmin(t1, tm.t) + 60 - tm.imm/2))) # not all infected from day 1
      # earlier infections are likely to be in the high priority groups 
      p.imm = 1 *  p.imm.wane * redn.priority # assume 100% prior infection provide immunity, but wane over time
      # and multiple by % excluded if there is prior testing before vax: p.prior.test
      percSmax.t = 1 - cumI.t / N * p.imm
      # also compare to current susceptibility, in case of immune evasion that increases susceptiblity
      percSmax.t = pmax(percSmax.t, state0[paste0('S',1:num_gr),] / N)
      # no lower than 50%, in case of outliers
      # percSmax.t = pmax(percSmax.t, .5)
      # print(c('cohort %S:',round(summary(mean(percSmax.t)),2)), quote = F)
    } else {
      percSmax.t = 0
      # print('no vax yet')
    }
    
    beta_tt = state0['beta',] * fn_getRelMob(rel.mob[1,], state0['p.mob',])
    if(seasonality) {
      # beta_tt = state0['beta',] * seasonal.cycle[seasonal.cycle$week==cur.wk,]$relR0 
      beta_tt = beta_tt * relR0[cur.wk,] 
    } 
    
    if(epi.model == 'SEIRS'){
      
      simEpi=SEIRS(tm_strt, tm_end, tm_step=1, # 1 day time-step
                   tmstep = tmstep,
                   state0 = state0,
                   S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                   I0=state0[paste0('I',1:num_gr),], 
                   beta=beta_tt, 
                   Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                   seed=seed, stoch=stoch, 
                   severity = severity.t,
                   newI.previous = newI.previous,
                   dist_tm.to.detect = dist_tm.to.detect,
                   dist_tm.to.death = dist_tm.to.death) # include the delay reporting etc.
      
    } else if(epi.model == 'SEIRSV') {
      
      daVacc.t = da.vacc[as.Date(date) >= as.Date(vdate.t) & as.Date(date) < as.Date(vdate.t)+tm_end-tm_strt+1] # vaccination data
      
      if(nrow(daVacc.t)<1){  # no data yet
        V1.t = V2.t = matrix(0, tm_end - tm_strt + 1, num_ens)
      } else { # yes data
        
        daVacc.t$date = daVacc.t$date %>% as.Date
        
        # make sure it includes a full week
        dates.t = data.table(date = seq(as.Date(vdate.t), length.out = tm_end-tm_strt+1, by='day'))
        daVacc.t = merge(daVacc.t, dates.t, all = T, by = 'date')
        daVacc.t[is.na(daVacc.t)] = 0
        V1.t = as.matrix(daVacc.t$n.v1, tmstep, num_ens)
        V2.t = as.matrix(daVacc.t$n.v2, tmstep, num_ens)
        
        # print('start vacc!')
        
      }
      simEpi=SEIRSV(tm_strt, tm_end, tm_step=1, # 1 day time-step
                    tmstep = tmstep,
                    state0 = state0,
                    S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                    I0=state0[paste0('I',1:num_gr),], 
                    beta=beta_tt, 
                    Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                    seed=seed, stoch=stoch, 
                    severity = severity.t,
                    newI.previous = newI.previous,
                    dist_tm.to.detect = dist_tm.to.detect,
                    dist_tm.to.death = dist_tm.to.death,
                    percSmax.t = percSmax.t,
                    V1 = V1.t, V2 = V2.t, # add vaccination for dose 1 and dose 2 -
                    # these are total number of vaccinees with unknown immunity
                    # but pre-ajust for time lag from vaccination to immune protection
                    VE1 = VE1, VE2=VE2 # Vaccine efficacy, need further adjustment by prior immunity 
      ) # include the delay reporting etc.
    }
    
    # re-assemble to the same order as the prior: state0
    n.end = tm_end - tm_strt + 2
    state.new = NULL
    for(i in 1:(length(simEpi)-1)){
      tmp = simEpi[[i]][n.end,,drop=F]; 
      rownames(tmp)=gsub('cumI','newI',paste0(names(simEpi)[i],1:num_gr))
      state.new = rbind(state.new,tmp)
    }
    
    state.new = rbind(state.new, state0[parm.names,])
    state.new = state.new[rownames(state0),] # make sure the order is the same
    
    
    xprior[,,1]= state.new
    xprior.daily[1:tmstep,] = simEpi$daily.newItot
    # save prior for E and I
    Eprior[1] = state.new['E1',] %>% mean
    Iprior[1] = state.new['I1',] %>% mean
  }
  
  dx.t.newitot = dx.t.newiobs = dx.t.e = dx.t.i = numeric(num_times_i)
  #### Begin looping through observations
  for (tt in 1:num_times_i){ # num_times_i
    
    # print(tt)
    # only inflate the observed
    # inflat=diag(x=c(1,1,1,1,1,1,lambda),7,7);
    # inflat all states and parameters
    xmn=rowMeans(xprior[,,tt]); 
    xnew=inflat*(xprior[,,tt]-xmn%*%matrix(1,1,num_ens))+xmn%*%matrix(1,1,num_ens);
    
    for(H in idx.obs_i){  # LOOP THROUGH OBSERVATIONS
      ####  Get the variance of the ensemble
      ####  Define the mapping operator H
      ####  HH is the number of variables and parameters in state space
      ####  H is only the number of columns being observed
      io = H - min(idx.obs_i) + 1
      obs_var = obs_vars_i[tt,io];
      prior_var = pmax(var(xnew[H,]),1);
      # post_var = 1/(1/prior_var + 1/obs_var);
      post_var = prior_var*obs_var/(prior_var+obs_var);
      
      prior_mean = mean(xnew[H,]);
      post_mean = post_var*(prior_mean/prior_var + obs_i[tt,io]/obs_var);
      
      #### Compute alpha and adjust distribution to conform to posterior moments
      alp = sqrt(obs_var/(obs_var+prior_var));
      
      dy = post_mean + alp*(xnew[H,]-prior_mean)-xnew[H,];
      ###  Getting the covariance of the prior state space and
      ###  observations  (which could be part of state space, e.g. infections)
      rr=NULL;
      for (j in 1:num_var){
        C=cov(xnew[j,],xnew[H,])/prior_var;  # covariance/varance of x.obs
        
        if(donotUpdateS & vdate.t %in% tm_rednUpdateS){  # restrict updating of S - before the surge of immune evasive variant
          if(grepl('S',state.names[j])) C = C / 10
        }
        if(rednUpdateEI  & vdate.t %in% tm_rednUpdateEI){  # key period of time to restrict updating of E and I
          if((grepl('E',state.names[j]) | grepl('I',state.names[j])) &
             !grepl('new',state.names[j])){
            C = C / 10 # reduce the level of update by a factor of 10
          }
        }
        
        rr=append(rr,C);
      }
      dx=rr%*%t(dy);
      
      # record the level of adjustment
      dx.t = rowMeans(dx); names(dx.t) = state.names
      dx.t.newitot[tt] = dx.t['newItot1']
      dx.t.newiobs[tt] = dx.t['newIobs1']
      dx.t.e[tt] = dx.t['E1']
      dx.t.i[tt] = dx.t['I1']
      
      ###  Get the new ensemble and save prior and posterior
      xnew = xnew + dx;  # ADJUSTED NUM_OBS TIMES, ONCE BY EACH OBSERVATION
    }
    
    # filter using the mortability data
    if(tt %in% 1:num_times_d){
      for(H in idx.obs_d){  # LOOP THROUGH OBSERVATIONS
        ####  Get the variance of the ensemble
        ####  Define the mapping operator H
        ####  HH is the number of variables and parameters in state space
        ####  H is only the number of columns being observed
        io = H - min(idx.obs_d) + 1
        obs_var = obs_vars_d[tt,io];
        prior_var = pmax(var(xnew[H,]),1);
        # post_var = 1/(1/prior_var + 1/obs_var);
        post_var = prior_var*obs_var/(prior_var+obs_var);
        
        prior_mean = mean(xnew[H,]);
        post_mean = post_var*(prior_mean/prior_var + obs_d[tt,io]/obs_var);
        
        #### Compute alpha and adjust distribution to conform to posterior moments
        alp = sqrt(obs_var/(obs_var+prior_var));
        
        dy = post_mean + alp*(xnew[H,]-prior_mean)-xnew[H,];
        ###  Getting the covariance of the prior state space and
        ###  observations  (which could be part of state space, e.g. infections)
        rr=NULL;
        for (j in 1:num_var){
          C=cov(xnew[j,],xnew[H,])/prior_var;  # covariance/varance of x.obs
          
          if(donotUpdateS & vdate.t %in% tm_rednUpdateS){
            if(grepl('S',state.names[j])) C = C / 10
          }
          if(rednUpdateEI & vdate.t %in% tm_rednUpdateEI){ # & tt> end1stWave
            if((grepl('E',state.names[j]) | grepl('I',state.names[j])) &
               !grepl('new',state.names[j])){
              C = C / 10 # reduce the level of update by a factor of 10
            }
          }
          
          rr=append(rr,C);
        }
        dx=rr%*%t(dy);
        
        ###  Get the new ensemble and save prior and posterior
        xnew = xnew + dx;  # ADJUSTED NUM_OBS TIMES, ONCE BY EACH OBSERVATION
      }
    }
    
    #  Corrections to DA produced aphysicalities
    xnew=Fn_checkDA(xnew, bound.low = DAbounds.t[,1], bound.up =  DAbounds.t[,2]);
    row.names(xnew)=state.names
    xpost[,,tt]=xnew;
    state0 = xnew
    
    # save post for E and I
    Epost[tt] = state0['E1',] %>% mean
    Ipost[tt] = state0['I1',] %>% mean
    Spost[tt] = state0['S1',] %>% mean
    Itotpost[tt] = state0['newItot1',] %>% mean
    
    # update daily estimates as well
    {
      
      # avoid dividing by 0 here!
      if(tmstep>1){
        f.adj = state0[idx.newItot,]/colSums(xprior.daily[1:tmstep+(tt-1)*tmstep,])
      } else {
        f.adj = state0[idx.newItot,]/(xprior.daily[1:tmstep+(tt-1)*tmstep,])
      }
      
      
      {
        i0=which(is.na(f.adj) | is.infinite(f.adj)); 
        xx=1:num_ens; 
        i.non0=xx[!(xx %in% i0)]
        if(length(i.non0)>0){
          f.adj[i0] = sample(f.adj[i.non0],size=length(i0),replace = T)
        } else {
          f.adj[i0] = 1 # all are 0
        }
        
      }
      f.adj_all = matrix(f.adj, tmstep, num_ens, byrow = T)
      # if obs=0 for the whole week, set all days to 0 - that is taken care of by default
      # so all the problem comes from obs!=0, but the prior says all days have 0 cases
      # in that case, distribute the cases evenly
      xpost.daily[1:tmstep+(tt-1)*tmstep,]=xprior.daily[(1:tmstep)+(tt-1)*tmstep,]*f.adj_all
      
    } # update daily estimates
    
    
    # compute the posterior of increase in S - immune evasion
    if(!(vdate.t %in% tm_rednUpdateS) # this week S can be adjusted
      ){ 
      
      if(Spost[tt] - Spost[tt-1] + Itotpost[tt] > .5/100 * N  # greater than .5% of the population
         ){ # there is a large adjustment to S - suggesting immune evasion
        dS.t = xpost['S1',,tt] - xpost['S1',, tt-1] + xpost['newItot1',,tt]  # not good b/c the eakf uses the mean for indiv variables
        # dS.t = mean(xpost['S1',,tt]) - mean(xpost['S1',, tt-1]) + mean(xpost['newItot1',,tt])
        
        xx1 = xpost['S1',,tt]; xx1mn = xx1 %>% mean; 
        xx2 = xpost['S1',, tt-1]; xx2mn = xx2 %>% mean; 
        xx3=xpost['newItot1',,tt]; xx3mn = xx3 %>% mean; 
        # var1 = var(xx1); var2 = var(xx2);  var3=var(xx3)
        m.cov = cov(x=cbind(xx1, xx2, xx3))
        
        dS.sd = deltamethod(~ x1 - x2 + x3, mean = c(xx1mn, xx2mn, xx3mn), 
                            cov = m.cov
        ) %>% sqrt
        
        dS.mn = dS.t  %>% mean
        
        stat.dS = rbind(stat.dS, 
                        data.table(week = tt, S0.mean = Spost[tt-1], # susceptiblity prior to the change
                                   dS.mean = dS.mn,
                                   dS.sd = dS.sd,
                                   dS.t %>% t
                        ))
      }
      
    }
    
    #  Integrate forward one time step
    tcurrent = tm.ini+tmstep*tt;
    tm_strt = tcurrent+1; tm_end = tcurrent + tmstep
    cur.wk = weeks[tt+1];
    if(is.na(cur.wk))
      cur.wk = weeks[tt];
    # print(paste('Week:',cur.wk),quote=F)
    vdate.t = Week.starts[pmin(tt+1,length(Week.starts))]
    
    newI.previous = xpost.daily[1:(tt*tmstep),] # these are at weekly level!
    
    tmp = getVE(date.t = vdate.t,
                dates_wt = dates_wt,
                dates_alpha = dates_alpha,
                dates_beta = dates_beta,
                dates_gamma = dates_gamma,
                dates_delta = dates_delta, 
                dates_omicron = dates_omicron,
                VE1wt = VE1wt,
                VE2wt = VE2wt,  # higher b/c we are using mortality data too
                VE1beta = VE1beta,
                VE2beta = VE2beta,  # higher b/c we are using mortality data too
                VE1gamma = VE1gamma,
                VE2gamma = VE2gamma,  # higher b/c we are using mortality data too
                VE1delta = VE1delta,
                VE2delta = VE2delta,  # higher b/c we are using mortality data too
                VE1omicron = VE1omicron,
                VE2omicron = VE2omicron
    )
    VE1 = tmp$VE1; VE2 = tmp$VE2
    
    # check the level of adjust for newItot to gauge how the filter has been working
    mn.pr.p = xprior[idx.newItot,,tt-1] %>% mean
    mn.po.p = xpost[idx.newItot,,tt-1] %>% mean
    mn.pr = xprior[idx.newItot,,tt] %>% mean
    mn.po = xpost[idx.newItot,,tt] %>% mean
    e.po = xpost[idx.e,,tt] %>% mean
    i.po = xpost[idx.i,,tt] %>% mean
    e.pr = xprior[idx.e,,tt] %>% mean
    i.pr = xprior[idx.i,,tt] %>% mean
    
    # SR - 
    if(doSR){
      
      # get SR/DA bounds
      tmp = getSRDAbounds(date.t = vdate.t, # date for the current week
                          SRbounds0 = SRbounds00, # original bounds
                          DAbounds0 = DAbounds00, # original bounds
                          parm.bound_vec = parm.bound_vec   # changes by period
      )
      SRbounds.t = tmp$SRbounds.t
      DAbounds.t = tmp$DAbounds.t
      percSR.t = tmp$percSR.t  # number of ensemble members to SR
      
      num_SR.t = round(num_ens*(SR.perc.local + SR.perc.full), 0)
      num_SR.local = round(num_ens*SR.perc.local, 0)
      num_SR.full = round(num_ens*SR.perc.full, 0)
      SR.idx.t = sample(1:num_ens, num_SR.t, replace = F)
      SR.idx.local.t = head(SR.idx.t, num_SR.local)
      SR.idx.full.t = tail(SR.idx.t, num_SR.full)
      
      sr.local.mean = rowMeans(state0[SR.var.local,]) # local SR bounds
      sr.local.lwr = apply(state0[SR.var.local,], 1, quantile, .1) # .2
      sr.local.upr = apply(state0[SR.var.local,], 1, quantile, .9) # .7
      sr.local.lwr = pmin(sr.local.mean * .7, sr.local.lwr) # .75
      sr.local.upr = pmax(sr.local.mean * 1.3, sr.local.upr) # 1.25
      sr.local.lwr = pmax(sr.local.lwr, DAbounds.t[SR.var.local,1]) # .75
      sr.local.upr = pmin(sr.local.upr, DAbounds.t[SR.var.local,2]) # 1.25
      
      state0[SR.var.local,SR.idx.local.t] = t(lhs(num_SR.local, rect =  cbind(sr.local.lwr, sr.local.upr)))
      
      state0[SR.var.full,SR.idx.full.t] = t(lhs(num_SR.full, rect = SRbounds.t[SR.var.full,]))
      
      # any parm need extra SR?
      # idxExtra = which(tolower(percSR.t$percSR) == 'extra')
      idxExtra = grep('extra', tolower(percSR.t$percSR))
      if(length(idxExtra)>0){
        for(idx.t in idxExtra){
          if(percSR.t[idx.t]$percSR == 'EXTRA'){
            num_SR.t = round(num_ens * SR.perc.EXTRA, 0)
          } else if (percSR.t[idx.t]$percSR == 'extra'){
            num_SR.t = round(num_ens * SR.perc.extra, 0)
          } else if (percSR.t[idx.t]$percSR == 'Extra'){
            num_SR.t = round(num_ens * SR.perc.Extra, 0)
          } else if(percSR.t[idx.t]$percSR == 'extra.lt'){
            num_SR.t = round(num_ens * SR.perc.extra.lt, 0)
          }
          
          parm.t = percSR.t[idx.t]$parm
          SR.idx.t = sample(1:num_ens, num_SR.t, replace = F)
          state0[parm.t, SR.idx.t] = runif(num_SR.t, min = SRbounds.t[parm.t,1], max = SRbounds.t[parm.t, 2])
        }
      }
      
      # when the new variant is immune evasive, beta tend to overshot then decrease, as it takes several weeks to get S up
      # very small probing on S, when the variant is immune evasive?
      {
        # check if it's a new imm evasive wave so that cntSR.S be reset to 0
        if(vdate.t %in% tm_reset_cntSR.S){
          print(paste(vdate.t, 'new wave, reset cntSR.S'), quote = F)
          cntSR.S = 0
        }
        if(!(vdate.t %in% tm_rednUpdateS) # this week S can be adjusted
        ){ 
          
          if(Spost[tt] - Spost[tt-1] + Itotpost[tt] > .8/100 * N &  # greater than .5% of the population
             cntSR.S < cntSR.Scut 
          ){ 
            print(paste0(tt, ': SR on S'))
            cntSR.S = cntSR.S + 1
            # allow SR.perc.imm.t to change with the filter adjustment
            SR.perc.imm.t = pmax(SR.perc.imm, pmin(SR.perc.imm * 4, 
                                 (Spost[tt] - Spost[tt-1] + Itotpost[tt]) / (1/100 * N) * SR.perc.imm * .8^cntSR.S
            ))
            print(paste(tt, '% SRimm', SR.perc.imm.t), quote = F)
          
            num_SR.t = round(num_ens * SR.perc.imm.t, 0)
            Sidx = sample(1:num_ens, num_SR.t, replace = F)
            
            # 
            
            # check current cumulative level of dImm first - if this already very high, do not allow further large probing
            tmp = fn_cum.dS.t(date.t = vdate.t,
                              stat.dS = stat.dS,
                              xpost = xpost,
                                   dS.cut.mn = dS.cut.mn, # threshold, if >75% already, no more major SR on this one
                                   dS.cut.ens = dS.cut.ens)
            
            if(tmp$allowSRprobe){
              if(!tmp$restrictSome){
                state0['S1', Sidx] = state0['S1', Sidx] + (N - state0['S1', Sidx]) * runif(num_SR.t, s.lwr.t, s.upr.t)
              } else {
                Sidx.t = Sidx[! Sidx %in% tmp$idx.no.update]
                # state0['S1', Sidx.t] = state0['S1', Sidx.t] + (N - state0['S1', Sidx.t]) * runif(length(Sidx.t), s.lwr.t, s.upr.t)
                state0['S1', Sidx.t] = state0['S1', Sidx.t] + pmin((N - state0['S1', Sidx.t]) * runif(length(Sidx.t), s.lwr.t, s.upr.t),
                                                                   tmp$ens_dImm.max[Sidx.t])
                
                print(paste('restrict SR on ', num_SR.t - length(Sidx.t), 'ens'))
              }
            }
            
            
          }
        }
        
        
      }
    }
    
    
    # get prior
    {
      if(!is.null(newI.previous) & vdate.t >= vax.start){
        tm.t = nrow(newI.previous)
        # cumI.t = apply(newI.previous00[,,1:(dim.t[3]-14),drop=F],c(1,2),sum) #  %>% apply(1, median) # excl last two weeks and get the median
        # t1 = (as.Date('2020/12/14') - as.Date('2020/3/1')) %>% as.numeric() # 1st day of vaccination
        # 2/5/21 set t1 to 1 yr given the slow rollout
        t1 = 365
        cumI.t = colSums(newI.previous[pmax(1,tm.t-t1) : (tm.t),]) #  %>% apply(1, median) # excl last two weeks and get the median
        # only count the last 12 months? so as the epidemic unfold, you don't over count cum infect?
        # higher infection rate for the priority group
        # tm.t = pmax(1, tm.t - t1 + 1) # re-aline timing with start of vac
        tm.imm = 365* 2.5 # assume 3 yr immunity
        p.imm.wane.max = .8; k = .015  # 1/tm.imm  
        # since only the last year is included, should be:
        p.imm.wane = 1 - p.imm.wane.max / (1+exp(-k*(pmin(t1, tm.t) + 60 - tm.imm/2))) # not all infected from day 1
        # earlier infections are likely to be in the high priority groups 
        p.imm = 1 *  p.imm.wane * redn.priority # assume 100% prior infection provide immunity, but wane over time
        # and multiple by % excluded if there is prior testing before vax: p.prior.test
        percSmax.t = 1 - cumI.t / N * p.imm
        # also compare to current susceptibility, in case of immune evasion that increases susceptiblity
        percSmax.t = pmax(percSmax.t, state0[paste0('S',1:num_gr),] / N)
        # no lower than 50%, in case of outliers
        # percSmax.t = pmax(percSmax.t, .5)
        # print(c('cohort %S:',round(summary(mean(percSmax.t)),2)), quote = F)
      } else {
        percSmax.t = 0
        # print('no vax yet')
      }
      
      beta_tt = state0['beta',] * fn_getRelMob(rel.mob[pmin(tt+1, nrow(rel.mob)),],state0['p.mob',]) 
      if(seasonality) {
        # beta_tt = state0['beta',] * seasonal.cycle[seasonal.cycle$week==cur.wk,]$relR0 # * rel.mob[tt]
        beta_tt = beta_tt * relR0[cur.wk,] # * rel.mob[tt]
      } 
      
      severity.t = severity
      severity.t['death',] = state0['ifr',]
      
      # [2/23/21] check this if problem!
      dist_tm.to.detect = NULL
      for(ii in 1:num_ens){
        tmp = generation.time(dist_tm.to.detect.name,c(state0['Td.mean',ii],state0['Td.sd',ii]),truncate = tm.to.detect.max)
        dist_tm.to.detect=cbind(dist_tm.to.detect,tmp$GT[-1]); 
      }
      
      
      # [3/7/22] also update time from infection to death, b/c longer time-lag for omicron
      if(vdate.t >= (date.beta+7) & vdate.t < (date.beta+14)){  # similar time-lag for beta and delta so use the same distributions
        tm.to.death.max = tm.to.death.max_beta
        if(tm.to.deathFromDiag){
          tm.from.inf.to.death.max = tm.to.diag + tm.to.death.max
        } else {
          tm.from.inf.to.death.max = tm.to.death.max
        }
        
        dist_tm.to.death = NULL  # time to death
        for(ii in 1:num_ens){
          # tmp = generation.time(dist_tm.to.death.name,c(state0['Td.mean',ii]+diff.dd,state0['Td.sd',ii]+diff.sd2),truncate = tm.to.death.max)
          # do not link it to Td
          tmp = generation.time(dist_tm.to.death.name,c(tm.to.outcome.mn['tm.to.death',ii] + tm2death_adj_beta.mn, tm.to.outcome.sd['tm.to.death',ii]) + tm2death_adj_beta.sd,truncate = tm.to.death.max)
          dist_tm.to.death=cbind(dist_tm.to.death,tmp$GT[-1]); 
        }
        if(tm.to.deathFromDiag){ # if the distribution of time to death is from diagnosis, not infectious
          # add time from infectious to diagnosis
          dist_tm.to.death = rbind(matrix(0,round(tm.to.diag,0),num_ens),dist_tm.to.death)
        }
        
        print('update time to death, beta wave')
        print(paste("tm.from.inf.to.death.max:", tm.from.inf.to.death.max), quote=F)
        print(nrow(dist_tm.to.death))
        
      } else if(vdate.t >= (date.omicron+7) & vdate.t < (date.omicron+14)){
        tm.to.death.max = tm.to.death.max_omicron
        if(tm.to.deathFromDiag){
          tm.from.inf.to.death.max = tm.to.diag + tm.to.death.max
        } else {
          tm.from.inf.to.death.max = tm.to.death.max
        }
        
        dist_tm.to.death = NULL  # time to death
        for(ii in 1:num_ens){
          # tmp = generation.time(dist_tm.to.death.name,c(state0['Td.mean',ii]+diff.dd,state0['Td.sd',ii]+diff.sd2),truncate = tm.to.death.max)
          # do not link it to Td
          tmp = generation.time(dist_tm.to.death.name,c(tm.to.outcome.mn['tm.to.death',ii] + tm2death_adj_omicron.mn, tm.to.outcome.sd['tm.to.death',ii]) + tm2death_adj_omicron.sd,truncate = tm.to.death.max)
          dist_tm.to.death=cbind(dist_tm.to.death,tmp$GT[-1]); 
        }
        if(tm.to.deathFromDiag){ # if the distribution of time to death is from diagnosis, not infectious
          # add time from infectious to diagnosis
          dist_tm.to.death = rbind(matrix(0,round(tm.to.diag,0),num_ens),dist_tm.to.death)
        }
        
        print('update time to death, omicron wave')
        print(paste("tm.from.inf.to.death.max:", tm.from.inf.to.death.max), quote=F)
        print(nrow(dist_tm.to.death))
      }
      
      if(epi.model == 'SEIRS'){
        
        simEpi=SEIRS(tm_strt, tm_end, tm_step=1, # 1 day time-step
                     tmstep = tmstep,
                     state0 = state0,
                     S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                     I0=state0[paste0('I',1:num_gr),], 
                     beta=beta_tt, 
                     Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                     seed=seed, stoch=stoch,
                     severity = severity.t,
                     newI.previous = newI.previous,
                     dist_tm.to.detect = dist_tm.to.detect,
                     dist_tm.to.death = dist_tm.to.death)
        
      } else if(epi.model == 'SEIRSV'){
        daVacc.t = da.vacc[as.Date(date) >= as.Date(vdate.t) & as.Date(date) < as.Date(vdate.t)+tm_end-tm_strt+1] # vaccination data
        
        if(nrow(daVacc.t)<1){  # no data yet
          V1.t = V2.t = matrix(0, tm_end - tm_strt + 1, num_ens)
        } else { # yes data
          
          daVacc.t$date = daVacc.t$date %>% as.Date
          
          # make sure it includes a full week
          dates.t = data.table(date = seq(as.Date(vdate.t), length.out = tm_end-tm_strt+1, by='day'))
          daVacc.t = merge(daVacc.t, dates.t, all = T, by = 'date')
          daVacc.t[is.na(daVacc.t)] = 0
          V1.t = as.matrix(daVacc.t$n.v1, tmstep, num_ens)
          V2.t = as.matrix(daVacc.t$n.v2, tmstep, num_ens)
          
          # print('start vacc!')
          
        }
        simEpi=SEIRSV(tm_strt, tm_end, tm_step=1, # 1 day time-step
                      tmstep = tmstep,
                      state0 = state0,
                      S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                      I0=state0[paste0('I',1:num_gr),], 
                      beta=beta_tt, 
                      Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                      seed=seed, stoch=stoch, 
                      severity = severity.t,
                      newI.previous = newI.previous,
                      dist_tm.to.detect = dist_tm.to.detect,
                      dist_tm.to.death = dist_tm.to.death,
                      percSmax.t = percSmax.t,
                      V1 = V1.t, V2 = V2.t, # add vaccination for dose 1 and dose 2 -
                      # these are total number of vaccinees with unknown immunity
                      # but pre-ajust for time lag from vaccination to immune protection
                      VE1 = VE1, VE2=VE2 # Vaccine efficacy, need further adjustment by prior immunity 
        ) # include the delay reporting etc.
      }
      
      # re-assemble to the same order as the prior: state0
      n.end = tm_end - tm_strt + 2
      state.new = NULL
      for(i in 1:(length(simEpi)-1)){
        tmp = simEpi[[i]][n.end,,drop=F]; 
        rownames(tmp)=gsub('cumI','newI',paste0(names(simEpi)[i],1:num_gr))
        state.new = rbind(state.new,tmp)
      }
      
      state.new = rbind(state.new, state0[parm.names,])
      state.new = state.new[rownames(state0),] # make sure the order is the same
      
      
      xprior[,,tt+1]= state.new
      xprior.daily[1:tmstep+tt*tmstep, ]=simEpi$daily.newItot # we want the daily total new cases, without delay, without under-report
      
      # save prior for E and I
      Eprior[tt+1] = state.new['E1',] %>% mean
      Iprior[tt+1] = state.new['I1',] %>% mean
    } # end get prior
    
  } # end for-loop 
  
  # calculate the mean of ensemble
  xprior_mean=xpost_mean=xprior_sd=xpost_sd=matrix(0,num_times_i,num_var)
  for (tt in 1:num_times_i){
    xprior_mean[tt,]=apply(xprior[,,tt],1,mean, na.rm=T)
    xprior_sd[tt,]=apply(xprior[,,tt],1,sd, na.rm=T)
    xpost_mean[tt,]=apply(xpost[,,tt],1,mean, na.rm=T)
    xpost_sd[tt,]=apply(xpost[,,tt],1,sd, na.rm=T)
    
  }
  colnames(xprior_mean)=colnames(xpost_mean)=colnames(xpost_sd)=colnames(xprior_sd)=state.names
  xpost_mean = data.table(Week.start = Week.starts, xpost_mean)
  xprior_mean = data.table(Week.start = Week.starts, xprior_mean)
  
  
  
  # get stats instead
  dimnames(xpost)[1] = list(state.names)
  states_stats = NULL
  for(var in state.names){
    tmp = xpost[var,,]
    tmp = tmp %>% apply(2, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
    colnames(tmp) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    
    tmp.mean = xpost[var,,] %>% apply(2, mean)
    states_stats = rbind(states_stats, 
                         data.table(Week.start = Week.starts, state = var, mean = tmp.mean, tmp))
  }
  
  
  # compute Rt
  Rt_ens = matrix(0, num_times_i, num_ens)
  R0_ens = matrix(0, num_times_i, num_ens)
  Rtx_ens = matrix(0, num_times_i, num_ens)
  for(tt in 1:num_times_i){
    
    cur.wk = weeks[tt]
    
    for(ii in 1:num_ens){
      
      beta.mean = xpost['beta',ii,tt] * fn_getRelMob(rel.mob[tt],xpost['p.mob',ii,tt]) 
      # add seasonality? 
      if(seasonality) {
        # beta.mean =beta.mean * seasonal.cycle[seasonal.cycle$week==weeks[tt],]$relR0
        beta.mean = beta.mean * relR0[cur.wk,ii] # * rel.mob[tt]
      }
      PARMS = list(beta.mean = beta.mean,  Tir.mean= xpost['Tir',ii,tt], S = xpost['S1',ii,tt], N = N)
      Rt_ens[tt, ii] = Fn_getRt_SEIR(PARMS)
      
      PARMS = list(beta.mean = xpost['beta',ii,tt] * ifelse(seasonality, relR0[cur.wk,ii], 1),  Tir.mean= xpost['Tir',ii,tt], S = xpost['S1',ii,tt], N = N) 
      R0_ens[tt, ii] = Fn_getR0_SEIR(PARMS) # cp Rtx: additionally account for seasonality
      
      Rtx_ens[tt, ii] = xpost['beta',ii,tt] * xpost['Tir',ii,tt] # no seasonality
    } # ens
  }
  
  Rt_stats = Rt_ens %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(Rt_stats) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  Rt_stats = data.table(Week.start = Week.starts, 
                        mean = Rt_ens %>% apply(1, mean),
                        sd = Rt_ens %>% apply(1, sd),
                        Rt_stats)
  
  R0_stats = R0_ens %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(R0_stats) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  R0_stats = data.table(Week.start = Week.starts, 
                        mean = R0_ens %>% apply(1, mean),
                        sd = R0_ens %>% apply(1, sd),
                        R0_stats)
  
  Rtx_stats = Rtx_ens %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(Rtx_stats) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  Rtx_stats = data.table(Week.start = Week.starts, 
                         mean = Rtx_ens %>% apply(1, mean),
                         sd = Rtx_ens %>% apply(1, sd),
                         Rtx_stats)
  
  xpost.last=xpost[,,tt] # save all the ens members
  row.names(xpost.last)=state.names
  
  # get the change in tx over diff time periods - diff variants
  {
    # parm.bound_vec = read_excel(paste0(dir_code, 'parm.bounds.xlsx'), sheet = 1) %>% data.table()
    tm_variant = parm.bound_vec[type == 'variant characterization']
    nv = nrow(tm_variant)
    # id the key period and char for each 'variant'
    newVstat = NULL
    newVstat.Rtx = NULL
    
    rrmse = data.table(variant = 'all', 
                       E = sqrt(mean((xprior_mean$E1 - xpost_mean$E1)^2)) / mean(xpost_mean$E1),
                       I = sqrt(mean((xprior_mean$I1 - xpost_mean$I1)^2)) / mean(xpost_mean$I1),
                       newItot = sqrt(mean((xprior_mean$newItot1 - xpost_mean$newItot1)^2)) / mean(xpost_mean$newItot1),
                       newIobs = sqrt(mean((xprior_mean$newIobs1 - xpost_mean$newIobs1)^2)) / mean(xpost_mean$newIobs1),
                       newD = sqrt(mean((xprior_mean$death1 - xpost_mean$death1)^2)) / mean(xpost_mean$death1),
                       obs.pr = sqrt(mean((xprior_mean$newIobs1 - obs_i)^2)) / mean(obs_i),
                       death.pr = sqrt(mean((xprior_mean$death1 - obs_d)^2)) / mean(obs_d),
                       obs.po = sqrt(mean((xpost_mean$newIobs1 - obs_i)^2)) / mean(obs_i),
                       death.po = sqrt(mean((xpost_mean$death1 - obs_d)^2)) / mean(obs_d))
                       
    for(iv in 1:nv){
      
      # evaluation performance during this period
      idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1)
      idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1)
      idx.full.t = idx0: idx1
      rrmse = rbind(rrmse, 
                    data.table(variant = tm_variant[iv]$parm, 
                         E = sqrt(mean((xprior_mean$E1 - xpost_mean$E1)[idx.full.t]^2)) / mean(xpost_mean$E1[idx.full.t]),
                         I = sqrt(mean((xprior_mean$I1 - xpost_mean$I1)[idx.full.t]^2)) / mean(xpost_mean$I1[idx.full.t]),
                         newItot = sqrt(mean((xprior_mean$newItot1 - xpost_mean$newItot1)[idx.full.t]^2)) / mean(xpost_mean$newItot1[idx.full.t]),
                         newIobs = sqrt(mean((xprior_mean$newIobs1 - xpost_mean$newIobs1)[idx.full.t]^2)) / mean(xpost_mean$newIobs1[idx.full.t]),
                         newD = sqrt(mean((xprior_mean$death1 - xpost_mean$death1)[idx.full.t]^2)) / mean(xpost_mean$death1[idx.full.t]),
                         obs.pr = sqrt(mean((xprior_mean$newIobs1 - obs_i)[idx.full.t]^2)) / mean(obs_i[idx.full.t]),
                         death.pr = sqrt(mean((xprior_mean$death1 - obs_d)[idx.full.t]^2)) / mean(obs_d[idx.full.t]),
                         obs.po = sqrt(mean((xpost_mean$newIobs1 - obs_i)[idx.full.t]^2)) / mean(obs_i[idx.full.t]),
                         death.po = sqrt(mean((xpost_mean$death1 - obs_d)[idx.full.t]^2)) / mean(obs_d[idx.full.t]))
      )
      
      if(iv == 1 & grepl('wildtype', tm_variant[iv]$parm)){
        idx.start = 5; 
        idx.end = which(as.Date(Week.starts) <  as.Date(tm_variant[iv]$date.end)) %>% tail(1)
        idx.t = idx.start: idx.end
        Rtx.t = Rtx_stats[idx.t, c("mean", 'sd', "median","iqr.lwr","iqr.upr","ci95.lwr","ci95.upr"), with=F] %>% colMeans()

      } else {
        # later variants, may take some time to reach the maximum
        idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1)
        idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1)
        # find the maximum
        idx.start = which.max(Rtx_stats[idx0:idx1]$mean) + idx0 -1  
        idx.end = idx1
        Rtx.t.max = max(Rtx_stats[idx0:idx1]$mean) 
        Rtx.t.mean = mean(Rtx_stats[idx0:idx1]$mean) 
        if(Rtx.t.max > Rtx.t.mean * 1.1 | idx.start > idx0+3){
          # the max is much higher than the mean during this period, 
          # then there is likely a gradual increase and that ramp-up should be excluded
          
          idx.t = (idx.start - 1): pmin(idx.start+ 7, idx.end) # look at the 1 wk prior 8 weeks following the peak
          Rtx.t = Rtx_stats[idx.t, c("mean", 'sd', "median","iqr.lwr","iqr.upr","ci95.lwr","ci95.upr"), with=F] %>% colMeans()
          
        } else {
          # not much difference, just use the mean
          idx.t = idx0: idx1
          Rtx.t = Rtx_stats[idx.t, c("mean", 'sd', "median","iqr.lwr","iqr.upr","ci95.lwr","ci95.upr"), with=F] %>% colMeans()
          
        }
        
      }
      
      newVstat.Rtx = rbind(newVstat.Rtx, data.table(variant = tm_variant[iv]$parm, idx.start = idx.t[1], idx.end = tail(idx.t,1), 
                                                    wk.start = Week.starts[idx.t[1]], wk.end = Week.starts[tail(idx.t,1)], 
                                                    Rtx.t %>% t))
      newVstat.dRtx = newVstat.dS = NULL
      # changes in Rtx, cp wildtype
      if(! grepl('wildtype', tm_variant[iv]$parm)){
        Rtx1 = newVstat.Rtx[variant == 'variant-wildtype']
        Rtx2 = newVstat.Rtx[variant == tm_variant[iv]$parm]
        idxW1main = Rtx1$idx.start : Rtx1$idx.end
        idxW2main = Rtx2$idx.start : Rtx2$idx.end
        
        tmp1 =  Rtx_ens[idxW1main,];
        tmp2 = Rtx_ens[idxW2main,];
        formula.t = paste0('x',1:nrow(tmp1), collapse = '+')
        mean.t = rowMeans(tmp1); cov.t = cov(t(tmp1))
        tmp1sd = eval(parse(text=paste('deltamethod(~', formula.t, ', mean= mean.t, cov=cov.t)', sep=''))) %>% sqrt
        formula.t = paste0('x',1:nrow(tmp2), collapse = '+')
        mean.t = rowMeans(tmp2); cov.t = cov(t(tmp2))
        tmp2sd = eval(parse(text=paste('deltamethod(~', formula.t, ', mean= mean.t, cov=cov.t)', sep=''))) %>% sqrt
        cov.t = matrix(c(tmp1sd^2, tmp1sd*tmp2sd,tmp1sd*tmp2sd, tmp2sd^2),2,2)
        dRtx.sd = (deltamethod(~(x2 - x1)/x1, mean = c(Rtx1$mean, Rtx2$mean) %>% unname, cov = cov.t) %>% sqrt) * 100
        
        newVstat.dRtx = data.table(perc.dRtx.mean = (Rtx2$mean - Rtx1$mean)/Rtx1$mean * 100, 
                          perc.dRtx.median = (Rtx2$median - Rtx1$median)/Rtx1$median * 100,
                          perc.dRtx.sd = dRtx.sd)
        
        # immunity - imm evasion
        idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1)
        idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1)
        idx.full.t = idx0: idx1
        dS.t = stat.dS[week %in% idx.full.t] # , c('week', 'S0.mean', 'dS.mean', 'dS.sd')
        # cum.dS.t = dS.t$dS.mean %>% sum()
        # cum.imm0.t = N - dS.t$S0.mean[1]
        # dImm.t = cum.dS.t / cum.imm0.t
        if(nrow(dS.t) > 0){
          dS.tot.mn = sum(dS.t$dS.mean)
          S0.t = dS.t$S0.mean[1] # susceptiblity when it started the adjustment
          wk.t = dS.t$week[1] - 1 # the prior week
          
          S0.2nd.strt = xpost['S1',,wk.t]
          S0.2nd.mn = S0.2nd.strt %>% mean; 
          S0.2nd.var = S0.2nd.strt %>% var
          # dS.ens = colSums(dS.t[,3+1:num_ens,with=F])  # probably not a good way
          Imm.ens = N - S0.2nd.strt; Imm.mn = Imm.ens %>% mean; 
          Imm.var = Imm.ens %>% var; Imm.sd = Imm.ens %>% sd
          
          # compute the combined SD for dS.tot.mn
          if(nrow(dS.t) > 1){
            xx.mean = numeric(nrow(dS.t))
            formula.t = 'x1'
            for(i in 1:nrow(dS.t)){
              xx.t = dS.t[i,4+1:num_ens,with=F] %>% unlist
              xx.t.mn =xx.t %>% mean; 
              # xx.t.var = var(xx.t); 
              xx.mean[i] = xx.t.mn
              # eval(parse(text=paste('xx',i,'=xx.t.mn',sep='')))
              if(i > 1)
                formula.t = paste0(formula.t,'+x',i)
            }
            
            m.cov = cov(x=dS.t[,4+1:num_ens,with=F]%>% t)
            eval(parse(text = paste('tmp = deltamethod(~', formula.t, ', mean = xx.mean, cov = m.cov)'))) 
            dS.tot.var = tmp; dS.tot.sd = dS.tot.var %>% sqrt
          } else {
            dS.tot.sd = dS.t$dS.sd; dS.tot.var = dS.tot.sd^2
          }
          cov.hat = dS.tot.sd * Imm.sd
          m.cov = matrix(c(dS.tot.var, cov.hat, cov.hat, Imm.var),2,2)
          
          # m.cov = cov(x=cbind(dS.ens, Imm.ens)) # probably not a good way
          perc.dImm.mn = dS.tot.mn / Imm.mn * 100
          perc.dImm.sd = (deltamethod(~x1/x2, mean = c(dS.tot.mn, Imm.mn), cov = m.cov) %>% sqrt) * 100
          
          # for changes in tx
          
          
          newVstat.dS = data.table(cum.dS = dS.tot.mn, cum.Imm = N - S0.t, 
                                   perc.dImm.mn = perc.dImm.mn, perc.dImm.sd = perc.dImm.sd)
        } else {
          S0.t = Spost[idx0]
          newVstat.dS = data.table(cum.dS = 0, cum.Imm = N - S0.t, 
                                   perc.dImm.mn = 0, perc.dImm.sd = 0)
        }
        
        newVstat = rbind(newVstat, 
                         data.table(variant = tm_variant[iv]$parm, newVstat.dS, newVstat.dRtx))
        
      } # changes due to new variant
    
    } 
  } # get the change in tx over diff time periods - diff variants
  
  if(T){ # don't need this
    # cumulative infection
    cumIperc = xpost['newItot1',,] %>% t %>% apply(2, cumsum)
    cumIperc = cumIperc / N * 100 # %
    cumIperc_stats = cumIperc %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
    colnames(cumIperc_stats) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    cumIperc_stats = data.table(Week.start = Week.starts, 
                                mean = cumIperc %>% apply(1, mean),
                                sd = cumIperc %>% apply(1, sd),
                                cumIperc_stats)
    # for susceptible
    Susceptibility = xpost['S1',,] %>% t 
    Susceptibility = Susceptibility / N * 100 # %
    Susceptibility_stats = Susceptibility %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
    colnames(Susceptibility_stats) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
    Susceptibility_stats = data.table(Week.start = Week.starts, 
                                      mean = Susceptibility %>% apply(1, mean),
                                      sd = Susceptibility %>% apply(1, sd),
                                      Susceptibility_stats)
  }
  
  
  return(list(Rt_stats = Rt_stats, R0_stats = R0_stats, 
              Rtx_stats = Rtx_stats, 
              Rtx_ens = Rtx_ens,
              states_stats = states_stats, 
              xpost_mean = xpost_mean, xpost_sd = xpost_sd, 
              xprior_mean = xprior_mean, xprior_sd = xprior_sd,
              cumIperc_stats = cumIperc_stats,
              cumIperc_ens = cumIperc,
              Susceptibility_stats = Susceptibility_stats,
              Susceptibility_ens = Susceptibility,
              xpost.last=xpost.last,
              newVstat = newVstat,
              newVstat.Rtx = newVstat.Rtx,
              rrmse = rrmse))
  
}

if(F){
  newVstat
  plot(xpost_mean$S1 / N, type = 'l')
  plot(xpost_mean$alpha, type = 'l')
  lines(xprior_mean$alpha, col = 'red')
  xpost_mean[which.max(xpost_mean$alpha)]
  plot(xpost_mean$ifr, type = 'l')
  lines(xprior_mean$ifr, col = 'red')
  xpost_mean[which.max(xpost_mean$ifr)]
  xprior_mean[which.max(xprior_mean$ifr)]
  plot(xpost_mean$beta, type = 'l')
  plot(xpost_mean$Td.mean, type = 'l')
  xpost_mean[which.max(xpost_mean$beta)]
  plot(xpost_mean$newIobs1, type = 'l')
  lines(xprior_mean$newIobs1, col = 'red')
  points(da.t$case)
  plot(xprior_mean$death1, ylim = c(0, max(c(xprior_mean$death1,xpost_mean$death1,da.t$death))), type = 'l')
  lines(xpost_mean$death1, col = 'red')
  points(da.t$death)
  
  xpost_mean[as.Date(xpost_mean$Week.start) < as.Date('2021/5/1')]$newItot1 %>% sum()
  xpost_mean[as.Date(xpost_mean$Week.start) < as.Date('2021/10/1')]$newItot1 %>% sum()
}


