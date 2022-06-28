# util functions for the EAKF or EAKF_rproj

Fn_checkDA<-function(xnew,bound.low,bound.up){
  b.low=bound.low;
  b.up=bound.up;
  n.var=nrow(xnew); n.ens=ncol(xnew);
  for(vi in 1:n.var){
    #  Corrects if <b.low
    ug=min(xnew[vi,]);
    if (ug<b.low[vi]){  
      for (jj in 1:n.ens){
        if (xnew[vi,jj]<b.low[vi]){
          # xnew[vi,jj]=b.low[vi];
          xnew[vi,jj]=pmax(b.low[vi],runif(1, min=pmax(b.low[vi],quantile(xnew[vi,],.25)), max=pmax(b.low[vi],quantile(xnew[vi,],.75)))); # biased high
        }
      }
    }
    ug=max(xnew[vi,]);
    if (ug>b.up[vi]){  
      for (jj in 1:n.ens){
        if (xnew[vi,jj]>b.up[vi]){
          # xnew[vi,jj]=b.up[vi];
          # apply to non-S variables
          if(grepl('S', names(b.up[vi]))){
            xnew[vi,jj]=b.up[vi];
          } else {
            xnew[vi,jj]=runif(1, min=min(b.up[vi]/2,quantile(xnew[vi,],.5)), max=min(quantile(xnew[vi,],.80),b.up[vi]));
          }
          
        }
      }
    }
  }
  xnew;
}

Fn_getR0_SEIR=function(PARMS){
  with(as.list(PARMS), {
    Ro = beta.mean * Tir.mean
    
    Ro
  })
}

Fn_getRt_SEIR=function(PARMS){
  with(as.list(PARMS), {
    Rt = beta.mean * Tir.mean * S/N
    
    Rt
  })
}

fn_getRelMob = function(rel.mob.t, p.mob.t){ # return the scaled moblity for adjusting tx
  # (rel.mob.t * p.mob.t) %>% pmin(1.5) # make sure it's <=1
  # some weeks have very high mobility and as a result likely underestimating beta
  (rel.mob.t * p.mob.t) %>% pmin(1.1)  # make it less extreme
}

fn_getImmLoss = function(N, S.t, E.t, I.t, tmstep, Trs.t, ts.ImmLoss.t, 
                         cum.ImmLoss.t){
  # Trs.t = mean(state0['Trs',])
  # ts.ImmLoss[tt] = (N - Spost[tt] - Epost[tt] - Ipost[tt])/Trs.t * tmstep
  ts.ImmLoss.t = ts.ImmLoss.t %>% unlist
  ts.ImmLoss = append(ts.ImmLoss.t, 
                      (N - S.t - E.t - I.t)/Trs.t * tmstep
  )
  
  wk.immloss = pmin(2*52, round(Trs.t / 7 * .75, 0)) # go back half of the immunity period? but no more than 2 years
  
  t.end = length(ts.ImmLoss)
  cum.ImmLoss = sum(ts.ImmLoss[pmax(1, t.end-wk.immloss):t.end])  # wk.immloss: how far do we wanna go back?
  print(paste('% cum.ImmLoss:', round(cum.ImmLoss / N * 100, 2)))
  
  return(list(ts.ImmLoss = ts.ImmLoss, cum.ImmLoss = cum.ImmLoss))
}

# to get the re
getSRDAbounds = function(date.t, # date for the current week
                         SRbounds0, # original bounds
                         DAbounds0, # original bounds
                         parm.bound_vec  # changes by period
                         # percSR.normal = .1,
                         # percSR.extra = .2
){
  
  # potential updates needed, alpha, ifr, beta
  SRbounds.t = SRbounds0
  DAbounds.t = DAbounds0
  
  parm.bound_vec$date.start = parm.bound_vec$date.start %>% as.Date()
  parm.bound_vec$date.end = parm.bound_vec$date.end %>% as.Date()
  date.t = date.t  %>% as.Date()
  percSR.t = NULL
  for(type.t in c('SR', 'DA')){
    for(parm.t in c('alpha', 'ifr', 'beta')){
      tmp = parm.bound_vec[parm == parm.t & type == type.t]
      
      i.t = which(tmp$date.start <= date.t & tmp$date.end >= date.t)
      eval(parse(text = paste(type.t,'bounds.t["',parm.t,'",]= unlist(tmp[',i.t,', c("lwr","upr"),with=F])', sep = '')))
      
      if(type.t == 'SR'){
        
        if(tmp[i.t]$SR.level == 'Extra.first1wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 1*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'Extra'))
        } else if(tmp[i.t]$SR.level == 'extra.allwks' & date.t %in% seq(tmp[i.t]$date.start, tmp[i.t]$date.end, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra.lt'))
        } else if(tmp[i.t]$SR.level == 'extra.first16wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 16*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra.lt'))
        } else if(tmp[i.t]$SR.level == 'extra.first8wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 8*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first7wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 7*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first6wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 6*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first5wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 5*7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first4wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 28, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first3wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 21, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if(tmp[i.t]$SR.level == 'extra.first2wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 14, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'extra'))
        } else if (tmp[i.t]$SR.level == 'EXTRA.first1wk' & date.t %in% seq(tmp[i.t]$date.start, length.out = 7, by = 'day')){
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.extra))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'EXTRA'))
        } else {
          # percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = percSR.normal))
          percSR.t = rbind(percSR.t, data.table(parm = parm.t, percSR = 'normal'))
        }
      }
    }
    
  }
  
  return(list(SRbounds.t = SRbounds.t, DAbounds.t = DAbounds.t, percSR.t = percSR.t))
}




# check immune evasion this wave - the guage level of SR on S
fn_cum.dS.t = function(date.t, 
                       stat.dS = stat.dS,
                       xpost = xpost,
                       dS.cut.mn = .85, # threshold, if >75% already, no more major SR on this one
                       dS.cut.ens = .6
){
  
  allowSRprobe = T; restrictSome = F; idx.no.update = NULL; ens_dImm.max = NULL
  
  parm.bound_vec$date.start = parm.bound_vec$date.start %>% as.Date()
  parm.bound_vec$date.end = parm.bound_vec$date.end %>% as.Date()
  date.t = date.t  %>% as.Date()
  
  tm_variant = parm.bound_vec[type == 'variant characterization']
  iv = which(tm_variant$date.start <= date.t & tm_variant$date.end >= date.t)
  # immunity - imm evasion
  idx0 = which(as.Date(Week.starts) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1)
  idx1 = which(as.Date(Week.starts) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1)
  idx.full.t = idx0: idx1
  
  if(! is.null(stat.dS)){
    dS.t = stat.dS[week %in% idx.full.t] # , c('week', 'S0.mean', 'dS.mean', 'dS.sd')
    # cum.dS.t = dS.t$dS.mean %>% sum()
    # cum.imm0.t = N - dS.t$S0.mean[1]
    # dImm.t = cum.dS.t / cum.imm0.t
    if(nrow(dS.t) > 0){
      dS.tot.mn = sum(dS.t$dS.mean)
      S0.t = dS.t$S0.mean[1] # susceptiblity when it started the adjustment
      wk.t = dS.t$week[1] - 1  # the week prior to the first adjustment
      
      S0.prior.strt = xpost['S1',,wk.t] 
      S0.prior.strt.mn = S0.prior.strt %>% mean
      
      # no further probing, if the mean already exceed the cutoff
      if(dS.tot.mn / (N - S0.prior.strt.mn) > dS.cut.mn){
        allowSRprobe = F
      } else {
        # look at if there are individual ens needing some restriction
        ens_dS.t = colSums(dS.t[,4+1:num_ens,with=F])
        
        perc.dImm.t = ens_dS.t / (N - S0.prior.strt)
        idx.no.update = which(perc.dImm.t > dS.cut.ens)
        
        # also recall the maximum residual dImm 
        # state0['S1', Sidx.t] = state0['S1', Sidx.t] + (N - state0['S1', Sidx.t]) * runif(length(Sidx.t), s.lwr.t, s.upr.t)
        ens_dImm.max = ((N - S0.prior.strt) * (.85 - perc.dImm.t)) %>% pmax(0) # don't go above 95%
        # idx = which(is.na(ens_dImm.max))
        ens_dImm.max[is.na(ens_dImm.max)] = 0
        
        if(length(idx.no.update)>0) 
          restrictSome = T
      }
      
    }
  }
  
  
  return(list(allowSRprobe = allowSRprobe, restrictSome = restrictSome, idx.no.update = idx.no.update, ens_dImm.max = ens_dImm.max))
}

# getVE may need update based on variants circulating locally
getVE = function(date.t, 
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
){
  if(date.t %in% dates_wt | date.t %in% dates_alpha){
    return(list(VE1 = VE1wt, VE2 = VE2wt))
  } else if(date.t %in% dates_beta){
    return(list(VE1 = VE1beta, VE2 = VE2beta))
  } else if(date.t %in% dates_gamma){
    return(list(VE1 = VE1gamma, VE2 = VE2gamma))
  } else if(date.t %in% dates_delta){
    return(list(VE1 = VE1delta, VE2 = VE2delta))
  } else if (date.t %in% dates_omicron){
    return(list(VE1 = VE1omicron, VE2 = VE2omicron))
  }
}

if(F){
  getVE = function(date.t, 
                   date.delta = date.delta, #  as.Date('2021/07/01'), 
                   date.omicron = date.omicron,
                   VE1beta = VE1beta,
                   VE2beta = VE2beta,  # higher b/c we are using mortality data too
                   VE1delta = VE1delta,
                   VE2delta = VE2delta,  # higher b/c we are using mortality data too
                   VE1omicron = VE1omicron,
                   VE2omicron = VE2omicron
  ){
    if(date.t < date.delta){
      return(list(VE1 = VE1beta, VE2 = VE2beta))
    } else if(date.t < date.omicron){
      return(list(VE1 = VE1delta, VE2 = VE2delta))
    } else {
      return(list(VE1 = VE1omicron, VE2 = VE2omicron))
    }
  }
}



