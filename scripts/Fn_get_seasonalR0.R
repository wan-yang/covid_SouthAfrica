# use weather data to represent seasonality
# 2/28/21

library('tgp')

Rwea_parm.bounds = rbind(
  c(2.34, 2.93), # R0max
  c(.86,1.18), # R0diff - this determines the magnitude of seasonality
  # reduce the leve of seasonality, b/c cp flu corona virus seasonality is less profound
  c(2.2,4.0)/1000, # qmin
  c(17,20)/1000, # qmax
  c(10.2,11.3)/1000, # qmid
  c(20.2,24), # Tc
  c(.4, 5.1), # Tdiff
  c(.95,1.54) # Texp
)
# use the mean instead to reduce uncertainty?
Rwea_parm.bounds = cbind(rowMeans(Rwea_parm.bounds),rowMeans(Rwea_parm.bounds))
rownames(Rwea_parm.bounds) = c('R0max','R0diff','qmin','qmax','qmid','Tc','Tdiff','Texp')




fn_getSnRt = function(loc.t, Rwea_parm.bounds, smooth = T, da_wea=NULL){
  
  if(is.null(da_wea))
    da_wea = read.csv(paste0(dir_data,'wea.by.week_',toupper(loc.t),'.csv')) %>% data.table()
  
  da_wea = da_wea[complete.cases(da_wea[,c('temp','spec.hum')])]
  da_wea = da_wea[order(week)]
  
  if(nrow(da_wea) < 52){
    return('incomplete data')
  } else {
    # calculate R0
    # num_ens = 500
    parm_Rwea = lhs(num_ens,rect = Rwea_parm.bounds) 
    colnames(parm_Rwea) = rownames(Rwea_parm.bounds)
    
    # calculate R0
    calc_R0wea <- function(da_wea, parm_Rwea) {
      
      spec.hum = da_wea$spec.hum;
      temp = da_wea$temp
      num_ens = nrow(parm_Rwea)
      
      # Create matrices for storing/returning results:
      res.temp = res.temp.red = matrix(0, nrow(da_wea), num_ens)
      
      # Loop through all ensemble members:
      for (ix in 1:num_ens) {
        
        # Assign parameters:
        q.mn <- parm_Rwea[ix, 'qmin']; q.mx <- parm_Rwea[ix, 'qmax']; q.md <- parm_Rwea[ix, 'qmid']
        R0.max <- parm_Rwea[ix, 'R0max']; R0.diff <- parm_Rwea[ix, 'R0diff']
        Tc <- parm_Rwea[ix, 'Tc']; Tdiff <- parm_Rwea[ix, "Tdiff"]; t.exp <- parm_Rwea[ix, 'Texp']
        
        
        q.mn.cut <- q.mn
        
        # Calculate and correct R0.min
        R0.min <- R0.max - R0.diff
        if (R0.min < 0) {
          R0.min <- 0.1
        }
        Tmin <- Tc - Tdiff
        
        # Calculate parabola params:
        
        # given the symmetry:
        q.mx.left = 2 * q.md - q.mn; 
        b.left <- ((R0.max - R0.min) * (q.mx.left + q.mn)) / ((q.mx.left - q.md) * (q.mn - q.md))
        a.left <- (-1 * b.left) / (q.mx.left + q.mn)
        c.left <- R0.min - a.left * q.md ** 2 - b.left * q.md
        
        q.mn.right = 2 * q.md - q.mx
        b.right <- ((R0.max - R0.min) * (q.mx + q.mn.right)) / ((q.mx - q.md) * (q.mn.right - q.md))
        a.right <- (-1 * b.right) / (q.mx + q.mn.right)
        c.right <- R0.min - a.right * q.md ** 2 - b.right * q.md
        
        fit1 = fit2 =numeric(nrow(da_wea))
        
        # split the data into two sets (those >=q.md, and those <q.md)
        idx.left = which(spec.hum < q.md); idx.right = which(spec.hum >= q.md)
        
        # Full model:
        q1.left <- spec.hum[idx.left]; q1.right = spec.hum[idx.right]
        t1.left <- temp[idx.left]; t1.right = temp[idx.right]
        fit1[idx.left] <- (a.left * q1.left ** 2 + b.left * q1.left + c.left) * (Tc / t1.left) ** t.exp
        fit1[idx.right] <- (a.right * q1.right ** 2 + b.right * q1.right + c.right) * (Tc / t1.right) ** t.exp
        
        # Reduced model:
        q1 <- spec.hum
        q1[q1 < q.mn.cut] <- q.mn.cut; q1[q1 > q.mx] <- q.mx
        t1 <- temp; t1[t1 < Tmin] <- Tmin
        q1.left <- q1[idx.left]; q1.right = q1[idx.right]
        t1.left <- t1[idx.left]; t1.right = t1[idx.right]
        fit2[idx.left] <- (a.left * q1.left ** 2 + b.left * q1.left + c.left) * (Tc / t1.left) ** t.exp
        fit2[idx.right] <- (a.right * q1.right ** 2 + b.right * q1.right + c.right) * (Tc / t1.right) ** t.exp
        
        
        # Store results in matrices:
        res.temp[, ix] <- fit1; res.temp.red[, ix] <- fit2
      }
      
      # Return results:
      return(list(res.temp, res.temp.red))
    }
    
    tmp = calc_R0wea(da_wea = da_wea, parm_Rwea)
    relR0 = tmp[[2]] 
    
    # relR0 = relR0 / matrix(colMeans(relR0[ref.wk + (-1:1),]), nrow(relR0), num_ens, byrow=T) # relative to to spring
    # relative to the mean?
    relR0 = relR0 / matrix(colMeans(relR0[-53,]), nrow(relR0), num_ens, byrow=T) # relative to to spring
    # smooth the curve
    relR0 = relR0 %>% apply(2, stats::filter, filter = rep(1/3,3), circular = T)
    
    relR0 = rowMeans(relR0) %>% as.numeric()
    

    
    return(relR0)
  }
  
  
}


# matplot(relR0, type = 'l', lty=1)  
# lines(relR0 %>% rowMeans(), lwd=2)
  
  
