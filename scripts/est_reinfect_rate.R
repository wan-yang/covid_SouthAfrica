# A proposed routine to compute reinfection rates 
# based on model-inference estimates here (immune erosion potential and attack rate for each wave)
# compute reinfection rate among the new wave
# compute reinfection rate among the entire population
# compute the cumulative ever infectees


dir_res = '../results/'


variants = c('Ancestral', 'Beta', 'Delta', 'Omicron (BA.1)')
zs = c(.3, .2, .5, .4)  # attack rate
thetas = c(0, .65, .4, .65) # immune erosion


res = data.table(variant = variants, `attack rate` = zs, `immune erosion` = thetas, 
                `cumulative percentage ever infected` = zs, 
                `percentage reinfection this wave, of those infected this wave` = 0, 
                `percentage reinfection this wave, of the entire population` = 0)

for(i in 2:nrow(res)){
  # reinfection rate among those infected:
  res[i]$`percentage reinfection this wave, of those infected this wave` = 
    (res[i-1]$`cumulative percentage ever infected` * res[i]$`immune erosion`) / (1 - res[i-1]$`cumulative percentage ever infected` + 
                                                                                    res[i-1]$`cumulative percentage ever infected` * res[i]$`immune erosion`)
  # reinfection rate among the entire population:
  res[i]$`percentage reinfection this wave, of the entire population` = res[i]$`attack rate`  * res[i]$`percentage reinfection this wave, of those infected this wave`
  # update cumulative % ever infected
  res[i]$`cumulative percentage ever infected` = res[i-1]$`cumulative percentage ever infected` + res[i]$`attack rate` - res[i]$`percentage reinfection this wave, of the entire population`
}

write.csv(res, paste0(dir_res, 'TableS4_ex_est_reinfect.rate.csv'), row.names = F)


