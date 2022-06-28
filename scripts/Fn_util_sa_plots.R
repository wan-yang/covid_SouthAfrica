# some utility functions for analysis and plotting
fn_format= function(x,roundit=T,roundigt=0){
  x=unlist(x)
  if(roundit==T){
    x=round(x,roundigt)
  }
  paste0(x[1],' (',x[2],', ',x[3],')')
}
fn_formatCI= function(x,roundit=T,roundigt=0){
  x=unlist(x)
  if(roundit==T){
    x=round(x,roundigt)
  }
  paste0('(',x[1],', ',x[2],')')
}

fn_rrmse = function(obs.t, est.t){
  sqrt(mean((obs.t - est.t)^2)) / mean(obs.t)
}

fn_corr = function(obs.t, est.t){
  cor(obs.t, est.t)
}

fn_acc = function(obs.t, est.t.lwr, est.t.upr){
  if(obs.t >= est.t.lwr & obs.t <= est.t.upr){
    acc = 1  # if within the upper and lower bound, deemed accurate
  } else {
    acc = 0
  } 
  acc
}

fn_getCumI = function(tda, loc.t, t.start, t.end){
  tda = tda[loc == loc.t & (date == as.Date(t.start) | date == as.Date(t.end))]
  tda2 = tda[, -c(1:2)] 
  tda2 = (tda2[2] - tda2[1]) %>% unlist
  data.table(mean = tda2 %>% mean,
             (tda2 %>% quantile(probs = c(.5, .25, .75, .025, .975))) %>% t)
}


theme.t = theme(plot.title = element_text(v=0, size = 9, margin=margin(0,0,3,0)), 
                strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                axis.text.x = element_text(size=8,angle = 0),
                plot.margin=unit(c(c(.1, 1, -.4, .3)), units="line"), # top, right, bottom, left
                legend.title = element_text(size=8), legend.text=element_text(size=8),
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.key.size = unit(.2, 'cm'), #change legend key size
                legend.key.height = unit(.2, 'cm'), #change legend key height
                legend.key.width = unit(.2, 'cm')) #change legend key width)

theme.t2 = theme(plot.title = element_text(v=0, size = 10, margin=margin(2,0,3,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 45, hjust = .9),
                 plot.margin=unit(c(c(.3, 1, .1, .5)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)
theme.t3 = theme(plot.title = element_text(v=0, size = 10, margin=margin(2,0,3,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 20, hjust = .9),
                 plot.margin=unit(c(c(.3, 1, -.7, .5)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)

theme.t4 = theme(plot.title = element_text(v=.5, size = 10, margin=margin(2,0,3,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 10, margin=margin(1.5,0,1.5,0), h = 0.5),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 35, hjust = .9),
                 plot.margin=unit(c(c(.3, 1, .5, .5)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)

theme.t5 = theme(plot.title = element_text(v=0, size = 9, margin=margin(0,0,3,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 45, hjust = .7),
                 plot.margin=unit(c(c(.3, 1, -1.2, 1.4)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)

theme.t6 = theme(plot.title = element_text(v=0, size = 9, margin=margin(0,0,1,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 60, hjust = .7),
                 plot.margin=unit(c(c(.3, 1.5, -.8, .2)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)
theme.t7 = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,2,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =8, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=7, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 45, hjust = .7),
                 plot.margin=unit(c(c(.3, 0.5, -1.2, 0.5)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)
theme.t8 = theme(plot.title = element_text(v=0, size = 9, margin=margin(0,0,1,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                 axis.text.x = element_text(size=8,angle = 45, hjust = .9),
                 plot.margin=unit(c(c(.3, 0.8, -.6, 0.8)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)
theme.t9 = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,2,0)), 
                 strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                 axis.title = element_text(size =8, margin=margin(0,0.2,0,0)), 
                 axis.text.y = element_text(size=7, margin=margin(0,0.2,0,0), angle = 90, hjust = .5), 
                 axis.text.x = element_text(size=8,angle = 45, hjust = .7),
                 plot.margin=unit(c(c(1, .5, -.2, .5)), units="line"), # top, right, bottom, left
                 legend.title = element_text(size=8), legend.text=element_text(size=8),
                 legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10),
                 legend.key.size = unit(.2, 'cm'), #change legend key size
                 legend.key.height = unit(.2, 'cm'), #change legend key height
                 legend.key.width = unit(.2, 'cm')) #change legend key width)

theme.tight = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,3,0)), 
                    strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                    axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                    axis.text.y = element_text(size=7, margin=margin(0,0.2,0,0)), 
                    axis.text.x = element_text(size=7,angle = 30),
                    plot.margin=unit(c(c(.5, 1, -1.2, .5)), units="line"), # top, right, bottom, left
                    legend.title = element_text(size=8), legend.text=element_text(size=8),
                    legend.margin=margin(0,0,0,0),
                    legend.box.margin=margin(-10,-10,-10,-10),
                    legend.key.size = unit(.2, 'cm'), #change legend key size
                    legend.key.height = unit(.2, 'cm'), #change legend key height
                    legend.key.width = unit(.2, 'cm')) #change legend key width)


getPlotProj = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
  
  dates.t = unique(tda$Week.start) %>% as.Date
  
  p = ggplot(tda)+
    geom_line(aes(x = Week.start, y = median, color = vx), size = 1) +  # no ctrl
    geom_point(data = obs, aes(x = Week.start, y = observed), size = 1) + 
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
    facet_rep_wrap(~ vx, # scales = 'free_y', 
                   repeat.tick.labels = T, ncol = 3) + 
    ggtitle(title.t) + labs(x = '', y = paste(y.lab, ''), color = 'Vaccination\n rate', fill = 'Vaccination\n rate') +
    scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                 labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
    theme_minimal() + theme.t3
  
  p
}

getPlotTotals = function(tda, title.t, y.lab = 'Projections, per 1 M popuplation (median)', ncol.t = 9, col.set, theme.tt = theme.t2){
  p = ggplot(tda, aes(fill=variant, y=v.median, x=sce.npi)) + 
    geom_bar(position="stack", stat="identity", alpha = .6) +
    ggtitle(title.t) + labs(x = '', y = y.lab) +
    facet_wrap(~seeding + sce.ve, ncol = ncol.t) + 
    # facet_wrap(~seed.ve, ncol = ncol.t) + 
    # scale_fill_brewer(palette = col.set) +
    theme_minimal() + theme.tt
  
  p
}

getPlotProj1 = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
  
  dates.t = unique(tda$Week.start) %>% as.Date
  
  p = ggplot(tda)+
    geom_line(aes(x = Week.start, y = median, color = vx), size = 1, alpha = .7) +  # no ctrl
    geom_point(data = obs, aes(x = Week.start, y = observed, shape = 'Observed'), size = 1.5) + 
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
    ggtitle(title.t) + labs(x = 'Week start', y = paste(y.lab, '')) + 
    guides(shape =guide_legend(title = '', order =1), color = guide_legend(title = ''), fill = guide_legend(title = '')) + 
    scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                 labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
    theme_minimal() + theme.t
  
  p
}

getPlotProjMedian = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
  
  dates.t = unique(tda$Week.start) %>% as.Date
  
  p = ggplot(tda)+
    geom_line(aes(x = Week.start, y = median, color = vx), size = .5, alpha = .7) +  # no ctrl
    geom_point(data = obs, aes(x = Week.start, y = observed, shape = 'Observed'), size = 1.5) + 
    # geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
    ggtitle(title.t) + labs(x = 'Week start', y = paste(y.lab, '')) + 
    guides(shape =guide_legend(title = '', order =1), color = guide_legend(title = 'VE1/VE2'), 
           fill = guide_legend(title = '')) + 
    scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                 labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
    ylim(0, max(c(tda$median, obs$observed)) * 1.1) + 
    theme_minimal() + theme.t
  
  p
}

getPlot_cpObs2 = function(tda){
  p = ggplot(tda) +
    geom_line(aes(x = Week.start, y = median), color = 'blue') +  # no ctrl
    geom_ribbon(aes(x = Week.start, ymin = ci95.lwr, ymax = ci95.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    geom_point(mapping = aes(x = Week.start, y=obs)) + 
    # geom_line(mapping = aes(x = Week.start, y=threshold), color = 'red') + 
    facet_rep_wrap(~loc + state, scales = 'free_y', repeat.tick.labels = T, ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) + 
    labs(x = '', y = 'Estimate (median, IQR, 95% CI)') +
    scale_x_date(breaks = seq(min(tda$Week.start), max(tda$Week.start), by = '6 months'),
                 labels = format(seq(min(tda$Week.start), max(tda$Week.start), by = '6 months'),'%Y %b')) +
    theme_minimal() + theme.t # theme(strip.text = element_text(size = 10), axis.title = element_text(size =10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10,angle = 30))
  
  p
}

getPlotMultiLoc = function(tda, ytitle = 'Estimate (median, IQR, 95% CI)', ptitle = '', ncol.t = 2){
  # truths.t = tda
  p = ggplot(tda) +
    geom_line(aes(x = date, y = median,color = loc)) +  # no ctrl
    # geom_ribbon(aes(x = date, ymin = ci95.lwr, ymax = ci95.upr, fill = loc),  alpha = .2) +
    # geom_ribbon(aes(x = date, ymin = iqr.lwr, ymax = iqr.upr, fill = loc), alpha = .35) +
    geom_point(mapping = aes(x = date, y=obs, color = loc, shape = loc),  size = 1.5) + 
    # geom_line(mapping = aes(x = Week.start, y=threshold), color = 'red') + 
    # scale_color_brewer(palette="Paired") + 
    scale_shape_manual(values=1:nlevels(tda$loc)) +
    facet_rep_wrap(~state, scales = 'free_y', repeat.tick.labels = T, ncol = ncol.t) + 
    labs(x = '', y = ytitle) + ggtitle(ptitle) +
    guides(color = guide_legend(title = 'Fitted', override.aes = list(size = 1, shape = NA)), fill = F,
           shape = guide_legend(title = 'Reported', override.aes = list(size = 1))) + 
    scale_x_date(breaks = seq(min(tda$date), max(tda$date), by = '6 months'),
                 labels = format(seq(min(tda$date), max(tda$date), by = '6 months'),'%Y %b')) +
    theme_minimal() + theme.t 
  p
}

plotCpHospExD = function(tda, tda.hosp, p.hosp, tda.exd, p.exd, corr.t, ptitle){
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
                       sec.axis = sec_axis(trans=~. * p.hosp, name="Hospitalizations per 100 K") # Add a second axis and specify its features
    ) + 
    geom_text(data = corr.t,
              mapping = aes(x = -Inf, y = -Inf, label = label.hosp),
              hjust   = -.5,vjust   = -8, col = 'red') +
    # facet_rep_wrap(~variant, ncol = 3, repeat.tick.labels = 'all') + # scales = 'free_y',
    labs(x = '') + # , y = 'Cumulative infection rate (%)'
    ggtitle(ptitle) +
    theme_minimal() + theme.t5
  
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
                       sec.axis = sec_axis(trans=~. * p.exd, name="Excess deaths per 100 K") # Add a second axis and specify its features
    ) + 
    geom_text(data = corr.t,
              mapping = aes(x = -Inf, y = -Inf, label = label.hosp),
              hjust   = -.5,vjust   = -8, col = 'red') +
    # facet_rep_wrap(~variant, ncol = 3, repeat.tick.labels = 'all') + # scales = 'free_y',
    labs(x = '') + # , y = 'Cumulative infection rate (%)'
    ggtitle('') +
    theme_minimal() + theme.t5
  
  return(list(pp.hosp = pp.hosp, pp.exd = pp.exd))
}


# for plotting the projections
sz.t = .7
getPlotProj5ci80 = function(train1, train2,  proj1, proj2, LettStart.t=1){
  
  # need to separate 1wk2peak and 2wk2peak as they have diff timing
  train1a =train1 %>% filter(fcast.tm == '2 weeks before peak incidence')
  train2a =train2 %>% filter(fcast.tm == '2 weeks before peak incidence')
  proj1a =proj1 %>% filter(fcast.tm == '2 weeks before peak incidence')
  proj2a =proj2 %>% filter(fcast.tm == '2 weeks before peak incidence')
  train1b =train1 %>% filter(fcast.tm == '1 week before peak incidence')
  train2b =train2 %>% filter(fcast.tm == '1 week before peak incidence')
  proj1b =proj1 %>% filter(fcast.tm == '1 week before peak incidence')
  proj2b =proj2 %>% filter(fcast.tm == '1 week before peak incidence')
  
  dates1a = c(train1a$Week.start, proj1a$Week.start) %>% unique %>% as.Date %>% sort
  train2a = train2a[! Week.start %in% dates1a]
  dates1n2a = c(train1a$Week.start,train2a$Week.start,
                proj1a$Week.start, proj2a$Week.start) %>% unique %>% as.Date %>% sort
  
  dates1b = c(train1b$Week.start, proj1b$Week.start) %>% unique %>% as.Date %>% sort
  train2b = train2b[! Week.start %in% dates1b]
  dates1n2b = c(train1b$Week.start,train2b$Week.start,
                proj1b$Week.start, proj2b$Week.start) %>% unique %>% as.Date %>% sort
  
  train1 = rbind(train1a, train1b)
  train2 = rbind(train2a, train2b)
  proj1 = rbind(proj1a, proj1b)
  proj2 = rbind(proj2a, proj2b)
  
  dates.t = c(train1$Week.start, train2$Week.start, 
    proj1$Week.start, proj2$Week.start) %>% unique %>% as.Date %>% sort
  
  if(length(dates.t) > 20)
    dates.t = dates.t[seq(1,length(dates.t), by = 4)]
  mea.t = 'Cases'
  p1 = ggplot(train1[measure==mea.t ]) +
    geom_line(aes(x = Week.start, y = median), color = 'blue', size = sz.t) +  # no ctrl
    geom_ribbon(aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    # 1st projection:
    geom_line(data=proj1[measure==mea.t ], aes(x = Week.start, y = median), color = 'red', size = sz.t) +  # no ctrl
    geom_ribbon(data=proj1[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'red', alpha = .1) +
    geom_ribbon(data=proj1[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .3) +
    geom_vline(data = train1[measure==mea.t ], aes(xintercept = max(train1$Week.start)), linetype = 'dashed')+
    # 3nd projection:
    geom_line(data = train2[measure==mea.t ], aes(x = Week.start, y = median), color = 'blue', size = sz.t) +  # no ctrl
    geom_ribbon(data = train2[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(data = train2[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    geom_line(data=proj2[measure==mea.t ], aes(x = Week.start, y = median), color = 'red', size = sz.t) +  # no ctrl
    geom_ribbon(data=proj2[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'red', alpha = .1) +
    geom_ribbon(data=proj2[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .3) +
    geom_vline(data = train2[measure==mea.t ], aes(xintercept = max(train2$Week.start)), linetype = 'dashed')+
    # geom_point(data = train1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    # geom_point(data = proj1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = train1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = proj1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = train2[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = proj2[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    facet_rep_wrap(~ fcast.tm, scales = 'fixed', repeat.tick.labels = T, ncol = 2, 
                   labeller = label_wrap_gen(multi_line=FALSE,width=60)) + 
    labs(x = '', y = mea.t, title = paste0('(', LETTERS[LettStart.t],') ', loc.t,': Projected vs. reported ', tolower(mea.t))) +
    scale_x_date(breaks = dates.t, labels = format(dates.t,'%Y/%m/%d')) +
    theme_minimal() +  theme.tight # + theme(strip.text = element_text(size = sz.t0), axis.title = element_text(size =10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10,angle = 45))
  
  mea.t = 'Deaths'
  p2 = ggplot(train1[measure==mea.t ]) +
    geom_line(aes(x = Week.start, y = median), color = 'blue', size = sz.t) +  # no ctrl
    geom_ribbon(aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    # 1st projection:
    geom_line(data=proj1[measure==mea.t ], aes(x = Week.start, y = median), color = 'red', size = sz.t) +  # no ctrl
    geom_ribbon(data=proj1[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'red', alpha = .1) +
    geom_ribbon(data=proj1[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .3) +
    geom_vline(data = train1[measure==mea.t ], aes(xintercept = max(train1$Week.start)), linetype = 'dashed')+
    # 3nd projection:
    geom_line(data = train2[measure==mea.t ], aes(x = Week.start, y = median), color = 'blue', size = sz.t) +  # no ctrl
    geom_ribbon(data = train2[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(data = train2[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    geom_line(data=proj2[measure==mea.t ], aes(x = Week.start, y = median), color = 'red', size = sz.t) +  # no ctrl
    geom_ribbon(data=proj2[measure==mea.t ], aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'red', alpha = .1) +
    geom_ribbon(data=proj2[measure==mea.t ], aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .3) +
    geom_vline(data = train2[measure==mea.t ], aes(xintercept = max(train2$Week.start)), linetype = 'dashed')+
    # geom_point(data = train1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    # geom_point(data = proj1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = train1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = proj1[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = train2[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = proj2[measure==mea.t ], mapping = aes(x = Week.start, y=obs)) + 
    facet_rep_wrap(~ fcast.tm, scales = 'fixed', repeat.tick.labels = T, ncol = 2, 
                   labeller = label_wrap_gen(multi_line=FALSE,width=60)) + 
    labs(x = '', y = mea.t, title = paste0('(', LETTERS[LettStart.t + 1],') ', loc.t,': Projected vs. reported ', tolower(mea.t))) +
    scale_x_date(breaks = dates.t, labels = format(dates.t,'%Y/%m/%d')) +
    theme_minimal() +  theme.tight # + theme(strip.text = element_text(size = sz.t0), axis.title = element_text(size =10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10,angle = 45))
  
  p = list()
  p[[1]] = p1
  p[[2]] = p2
  p
}

getPlotProj5ci80c = function(train.t,  proj.t, mea.t, date.start.t, title.t){ # plot proj for just 1 wave
  
  train.t =train.t %>% filter(Week.start >= date.start.t & measure == mea.t)
  proj.t =proj.t %>% filter(Week.start >= date.start.t & measure == mea.t)
  
  
  dates.t = c(train.t$Week.start, proj.t$Week.start) %>% unique %>% as.Date %>% sort
  
  if(length(dates.t) > 10)
    dates.t = dates.t[seq(1,length(dates.t), by = 2)]
  
  p = ggplot(train.t) +
    geom_line(aes(x = Week.start, y = median), color = 'blue', size = sz.t) +  # no ctrl
    geom_ribbon(aes(x = Week.start, ymin = ci95.lwr, ymax = ci95.upr), fill = 'blue', alpha = .1) +
    geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'blue', alpha = .3) +
    # projection:
    geom_line(data=proj.t, aes(x = Week.start, y = median), color = 'red', size = sz.t) +  # no ctrl
    geom_ribbon(data=proj.t, aes(x = Week.start, ymin = ci80.lwr, ymax = ci80.upr), fill = 'red', alpha = .1) +
    geom_ribbon(data=proj.t, aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr), fill = 'red', alpha = .3) +
    geom_vline(data = train.t, aes(xintercept = max(train.t$Week.start)), linetype = 'dashed')+
    geom_point(data = train.t, mapping = aes(x = Week.start, y=obs)) + 
    geom_point(data = proj.t, mapping = aes(x = Week.start, y=obs)) + 
    facet_rep_wrap(~ fcast.tm, scales = 'fixed', repeat.tick.labels = T, ncol = 2, 
                   labeller = label_wrap_gen(multi_line=FALSE,width=60)) + 
    labs(x = '', y = mea.t, title = title.t) +
    scale_x_date(breaks = dates.t, labels = format(dates.t,'%m/%d/%y')) +
    theme_minimal() +  theme.tight # + theme(strip.text = element_text(size = sz.t0), axis.title = element_text(size =10), axis.text.y = element_text(size=10), axis.text.x = element_text(size=10,angle = 45))
  
  p
}
