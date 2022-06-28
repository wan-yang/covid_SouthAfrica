# compile South Africa data, subnational
# 11/30/21
# data source: https://github.com/dsfsi/covid19za

# Limpopo, Mpumalanga, North West


dir_data = '../data/'
dir_code = '../scripts/'

if(F){
  
  # for computing the seasonal trend, done locally, b/c files are too large
  
  dir_sn = '/wea/'  # large files for local weather data
  dir_map = '../data/zaf_adm_sadb_ocha_20201109_SHP/'  # https://data.humdata.org/dataset/south-africa-admin-level-1-boundaries
  
}


loc.t = 'South Africa'  
date.tag = paste0('_',format(Sys.Date(),'%Y-%m-%d'))


library(data.table); library(magrittr)
library(rworldmap)
library(classInt)
library(RColorBrewer)
library(mapdata)
library(maptools) # for shapefiles
library(scales) # for transparency
library(plotrix); # for color.legend
# library(graphicsQC)
library(rgdal); # for projection system conversion
library(sp);
library(TeachingDemos);
library('readr')
library('readxl')
library('writexl')
library(MMWRweek)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(stationaRy)
library(jsonlite)
library(httr);


{
  fn_fillNA = function(tda.t, dv){
    
    tda.t$idx = 1:nrow(tda.t)
    tda.t$est = 0
    
    idx.all = which(is.na(tda.t[,dv,with=F]))
    
    if(length(idx.all) == 0){
      return(tda.t[,1:2,with=F])
    }
    
    consecutive =  idx.all[-1] - idx.all[-length(idx.all)]
    i.div = which(consecutive >=2) # 1/27/22 check here if err, change '>' to '>='
    w.div = idx.all[which(consecutive >=2)] # 1/27/22 check here if err, change '>' to '>='
    grps = list()
    if(length(i.div) == 0){  # only 1 group
      grps[[1]] = idx.all[1] : tail(idx.all,1)
    } else if(length(i.div) == 1){
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
    for(ig in 1: length(grps)){
      idx0 = grps[[ig]]
      
      n.add = ifelse(length(idx0) < 2, 1, 1)
      idx = seq((idx0[1] - n.add) %>% pmax(1), 
                (tail(idx0,1)+ n.add) %>% pmin(nrow(tda.t)),
                by = 1
      ) # add an additional week in case there is back adjustment
      
      tda.t0 = tda.t[idx]; 
      tda.t0 = tda.t0[complete.cases(tda.t0[,c('date', dv), with =F])]
      
      fit1 = lm({eval(parse(text = dv))} ~ idx, data = tda.t0)
      tda.t$est[idx0] = predict(fit1, newdata = tda.t[idx0])
      
    }
    tda.t[is.na({eval(parse(text = dv))}),2] = tda.t[is.na({eval(parse(text = dv))})]$est
    
    tda.t[,1:2,with=F]
  } 
}


url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv'
case = read_csv(url(url.t)) %>% data.table() 

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv'
death= read_csv(url(url.t)) %>% data.table() 

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/za_province_pop.csv'
pop = read_csv(url(url.t), col_names =F) %>% data.table() 

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_vaccination.csv'
vx = read_csv(url(url.t)) %>% data.table()

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv'
test = read_csv(url(url.t)) %>% data.table()  # testing data

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/combined_district_keys.csv'
keys = read_csv(url(url.t)) %>% data.table()  # location keys

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/mobility/google_mobility/mobility_report_ZA.csv'
covid19za.mob = read_csv(url(url.t)) %>% data.table()  # google mobility post on covid19za

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/mobility/apple_mobility/applemobilitytrends-zaf.csv' 
covid19za.applemob = read_csv(url(url.t), col_names = F) %>% data.table()  # mobility data based on apple map use
# The CSV file and charts on this site show a relative volume of directions requests per country/region, sub-region or city compared to a baseline volume on January 13th, 2020.

# provincial level = 0
# keys[District_level == 0 & Data_type == 'Death'] %>% View
provinces = data.table(code = c('WC', 'NC', 'EC', 'GP', 'KZN', 'MP', 'FS', 'LP', 'NW'), 
                       label = c('Western Cape', 'Northern Cape', 'Eastern Cape', 'Gauteng', 'KwaZulu-Natal', 
                       'Mpumalanga', 'Free State', 'Limpopo', 'North West'))

names(pop) = c('location','population')
pop[location == 'Northwest']$location = "North West"

# mobility 
mob.type.bus = c('retail_and_recreation_percent_change_from_baseline',
                 'transit_stations_percent_change_from_baseline',
                 'workplaces_percent_change_from_baseline')  
# ,'parks_percent_change_from_baseline',
# 'residential_percent_change_from_baseline'
mob.type.full = c('retail_and_recreation_percent_change_from_baseline',
                  'grocery_and_pharmacy_percent_change_from_baseline',
                  'parks_percent_change_from_baseline',
                  'transit_stations_percent_change_from_baseline',
                  'workplaces_percent_change_from_baseline',
                  'residential_percent_change_from_baseline')

url.mob = 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv'
d.mob = read_csv(url(url.mob)) %>% data.table() 

mob = d.mob[country_region %in% c("South Africa")] # there is regional mobility data
rm(d.mob)

mob = mob[, lapply(.SD, median, na.rm=T), by = c('date', 'sub_region_1'), 
            .SD = mob.type.full] #  %>% setnames('NA', 'South Africa')
mob$mob.bus = 1 + rowMeans(mob[,mob.type.bus,with=F])/ 100  # take the average for the 
mob$mob.full = 1 + rowMeans(mob[,mob.type.full,with=F])/ 100 

prov.t = 'Western Cape' # 'Eastern Cape' # 'North West' #  'Mpumalanga' # 'Limpopo' #  , North West #  "Gauteng"
provs = c("Eastern Cape","Free State","Gauteng","KwaZulu-Natal", "Limpopo","Mpumalanga","North West","Northern Cape", "Western Cape")
par(mfrow=c(3,3), mar=c(2, 2.5, 1.2, .5), cex=.8, mgp=c(1.2, .3, 0), tck = -.02)
for(prov.t in provs){
  matplot(mob[sub_region_1== prov.t, c('mob.bus','mob.full')], type='l', 
          lty =1, col = c('blue','black'), ylab = 'mobility')
  abline(h = 1)
  abline(h=0); legend('bottomleft',c('business','all'), lty=1, col = c('blue','black'), bty='n')
  mtext(prov.t)
}

if(F){  
  tmp = mob[sub_region_1== prov.t]
  # cross check with covid19za.mob - same pattern
  tmp = covid19za.mob[province==prov.t, c('retail and recreation','transit stations','workplaces'),with=F] %>% rowMeans()
  plot(tmp, type='l')
  
  # apple map based mobility
  tmp2 = covid19za.applemob[X2 == prov.t]
  dates.applemob = seq(as.Date('2020/1/13'), length.out = ncol(covid19za.applemob)-6, by = 'day')
  tmp2 = data.table(date = dates.applemob, mob = colMeans(tmp2[,-c(1:6)]))
  plot(tmp2$date, tmp2$mob, type = 'l')
  abline(h = 100, col = 'red')
}

# make it weekly
d.mob.bus = dcast(mob, date ~ sub_region_1, value.var = "mob.bus") %>% setnames('NA', 'SA')
d.mob.full = dcast(mob, date ~ sub_region_1, value.var = "mob.full") %>% setnames('NA', 'SA')

tmp = MMWRweek(d.mob.bus$date) %>% setnames(paste0('MMWR',c('year','week','day')),c('year','week','day'))
d.mob.bus = cbind(tmp, d.mob.bus) %>% data.table()
d.mob.bus = d.mob.bus[,lapply(.SD, mean, na.rm=T), by = c('year', 'week'), 
              .SDcols = 5:ncol(d.mob.bus)]
d.mob.bus$date = MMWRweek2Date(MMWRyear = d.mob.bus$year, MMWRweek = d.mob.bus$week, MMWRday = 1)


tmp = MMWRweek(d.mob.full$date) %>% setnames(paste0('MMWR',c('year','week','day')),c('year','week','day'))
d.mob.full = cbind(tmp, d.mob.full) %>% data.table()
d.mob.full = d.mob.full[,lapply(.SD, mean, na.rm=T), by = c('year', 'week'), 
                      .SDcols = 5:ncol(d.mob.full)]
d.mob.full$date = MMWRweek2Date(MMWRyear = d.mob.full$year, MMWRweek = d.mob.full$week, MMWRday = 1)



# check NA
for(da.type in c('d.mob.bus','d.mob.full')){
  tda = get(da.type)
  oldnames = colnames(tda)
  colnames(tda) = gsub(' ', '', colnames(tda))
  locs = colnames(tda)[-c(1,2,ncol(tda))]
  for(iloc in locs){
    tda.t = tda[,c('date', iloc), with=F] 
    tda.t = fn_fillNA(tda.t, iloc)
    tda[,iloc] = tda.t[,iloc, with=F]
  }
  colnames(tda) = oldnames
  eval(parse(text = paste(da.type, '=tda')))
}


d.mob.bus = melt(d.mob.bus, id.vars = c("date", "year","week")) %>% setnames('variable', 'country')
d.mob.full = melt(d.mob.full, id.vars = c("date", "year","week")) %>% setnames('variable', 'country')
d.mob.bus$data.type = 'business'
d.mob.full$data.type = 'all'


d.mob = rbind(d.mob.bus, d.mob.full)
write.csv(d.mob, paste0(dir_data, 'da_mobility_sa_subnational',date.tag,'.csv'), row.names = F)


N = 1e6

# aggregate to weekly intervals
# states = pop$Province_State  %>% unique



pop.t = pop 
res = NULL
for(da.type in c('case', 'death')){
  tda = get(da.type)
  tda$date = tda$date %>% as.Date(.,format = '%d-%m-%Y')
  # some days are missing, but minimum missing and mostly in early days
  dates = data.table(date = seq(as.Date(min(tda$date)), as.Date(max(tda$date)), by = 'day')) 
  tda = merge(tda, dates, all=T, by = 'date')
  tda$YYYYMMDD = NULL; tda$source = NULL
  
  # fill with interpolation - mostly 
  for(iloc in c(provinces$code, 'UNKNOWN', 'total')){
    tda.t = tda[,c('date', iloc), with=F]
    tda.t = fn_fillNA(tda.t, iloc)
    tda[,iloc] = tda.t[,iloc, with=F]
  }


  # compute the increment for each date
  tda1 = data.table(date = tda$date[-1], 
                    tda[-1,2:ncol(tda), with=F] - tda[-nrow(tda),2:ncol(tda), with=F])
  
  # correct for an error in SA data
  # source John Burn-Murdoch @jburnmurdoch, tweet 11/25/21, 3:11pm
  # the spike on Nov 23rd is an error in the data. it should be 868 cases, not 18,586
  # https://sacoronavirus.co.za/2021/11/23/update-on-covid-19-tuesday-23-november-2021/
  # very high deaths in Mpumalanga, and Northern cape, but the data matched with the official website
  # https://sacoronavirus.co.za/2022/02/28/update-on-covid-19-monday-28-february-2022/ 183 deaths - this match with the record
  # https://sacoronavirus.co.za/2022/02/17/update-on-covid-19-thursday-17-february-2022/ 435 deaths - this match with the record
  # https://sacoronavirus.co.za/2022/02/18/update-on-covid-19-friday-18-february-2022/ 343 deaths  - this match with the record
  # https://sacoronavirus.co.za/2022/02/19/update-on-covid-19-saturday-19-february-2022/  319 deaths - this match with the record
  
  # error in case data for 11/23/21 - set it to NA and use interpolation instead
  if(da.type=='case'){
    tda1[date == as.Date('2021/11/23'), 2:ncol(tda1)] = NA
    for(iloc in c(provinces$code, 'UNKNOWN', 'total')){
      tda.t = tda1[,c('date', iloc), with=F]
      tda.t = fn_fillNA(tda.t, iloc)
      tda1[,iloc] = tda.t[,iloc, with=F]
    }
    # adjust it by scaling
    tda1[date == as.Date('2021/11/23'), 2:ncol(tda1)] = tda1[date == as.Date('2021/11/23'), 2:ncol(tda1),with=F] * 868 / tda1[date == as.Date('2021/11/23')]$total
    
  }
  
  
  tda1 = tda1 %>% melt(data = ., id.vars = 'date') %>% setnames('variable', 'location') %>% 
    mutate(value = value %>% as.numeric(), 
           date = date %>% as.Date(format = '%m/%d/%y'))  %>% data.table()
  tda1$location = factor(tda1$location, levels = provinces$code, labels = provinces$label)
  
  
  # normalize to per 1 M pop
  tda1 = merge(tda1, pop.t, x.all = T, by = 'location') %>% 
    mutate(value = value / population * N)
  

  tda2 = tda1 %>% dcast(date ~ location, value.var = 'value')
  

  
  # exclude impcomplete week
  tda2 = tda2 %>% filter(date < Sys.Date() - (format(Sys.Date(), '%w') %>% as.numeric()))
  
  
  tmp = tda2 %>% .$date %>% MMWRweek %>% setnames(c('MMWRyear','MMWRweek','MMWRday'),c('year','week','day'))
  tda2 = cbind(tmp, tda2) %>% data.table()
  tda2 = tda2[, lapply(.SD, sum, na.rm=T), .SDcol = 5:ncol(tda2), by = c('year', 'week')]
  tda2 = data.table(date = MMWRweek2Date(MMWRyear = tda2$year, MMWRweek = tda2$week), tda2)
  
  matplot(tda2[,4:ncol(tda2), with=F], type = 'l')
  tda2 = tda2 %>% melt(., id.vars = c('date', 'year', 'week')) %>% setnames('variable', 'country')
  tda2[value < 0]  # some <0
  tda2[value < 0]$value = 0 # set it to 0
  
  tda2$data.type = da.type
  
  res = rbind(res, tda2)
  # save(tda2, file = paste0(dir_data, 'da_wkly.',da.type,'_us.RData'))
}
write.csv(res, paste0(dir_data, 'da_case_death_sa_subnational',date.tag,'.csv'), row.names = F)

# look at vaccination data
# problem: only have total vaccination data, no break down on 1st v 2nd dose
# check national data, dist on 1st / 2nd dose by date
# read vaccination data
DAT.VAC = read.csv(paste0(dir_data,'da_vx_perM_global_nolag',date.tag,'.csv')) %>% data.table()
da.vac = DAT.VAC[country == 'South Africa']
da.vac$perc1 = da.vac$n.v1 / rowSums(da.vac[,c('n.v1', 'n.v2')])
da.vac$perc2 = da.vac$n.v2 / rowSums(da.vac[,c('n.v1', 'n.v2')])
da.vac[is.na(da.vac)] = .5 # div by 0
da.vac[1:21]$perc1 = 1; # no 2nd dose for first 21 days
da.vac[1:21]$perc2 = 0; 
da.vac$date = da.vac$date %>% as.Date
# use the national data for distr
colnames(vx)
vx[1000] 
vx$date = vx$date %>% as.Date # (format='%d-%m-%Y')
res = NULL
for(loc.t in provinces$code){
  
  prov.t = provinces[code == loc.t]$label
  
  tda = vx[,c('date', loc.t), with=F] 
  dates = data.table(date = seq(as.Date(min(tda$date)), as.Date(max(tda$date)), by = 'day')) 
  tda = merge(tda, dates, all=T, by = 'date')
  tda = tda %>% fn_fillNA(tda.t = ., dv = loc.t)
  
  # convert from cum vax to daily vax
  tda = data.table(date = tda$date[-1], 
                     tda[-1,2, with=F] - tda[-nrow(tda),2, with=F])
  tda = merge(tda, da.vac, by = 'date', all.x = T)
  tda[is.na(tda)] = .5
  tda$n.v1 = tda[,loc.t,with=F] * tda$perc1 / pop.t[location==prov.t]$population * N
  tda$n.v2 = tda[,loc.t,with=F] * tda$perc2 / pop.t[location==prov.t]$population * N
  
  tda$location = prov.t
  
  res = rbind(res, tda[,c('location','date', 'n.v1', 'n.v2')])
}
res[n.v1 < 0]$n.v1 = 0
res[n.v2 < 0]$n.v2 = 0
write.csv(res, paste0(dir_data, 'da_vx_perM_sa_subnational_nolag',date.tag,'.csv'), row.names = F)

# lag it
lagV1 = 14 # 
lagV2 = 7

res1daily = res[,c('location', 'date','n.v1'), with=F]
res1daily = res1daily[order(date)]
res1daily$date = res1daily$date + lagV1
res2daily = res[,c('location', 'date','n.v2'), with=F]
res2daily = res2daily[order(date)]
res2daily$date = res2daily$date + lagV2
res = merge(res1daily, res2daily, all=T,by= c('location', 'date'))
res[is.na(res)] = 0
# res[n.v1 < 0]$n.v1 = 0
# res[n.v2 < 0]$n.v2 = 0
write.csv(res, paste0(dir_data,'da_vx_perM_sa_subnational_lagged',date.tag,'.csv'), row.names = F)

# check data
plot(res[location == 'Gauteng']$n.v1, type = 'l')
plot(res[location == 'Gauteng']$n.v2, type = 'l')
sum(res[location == 'Gauteng']$n.v1)
sum(res[location == 'Gauteng']$n.v2)
tail(vx[location == 'Gauteng'],1)


# compute seasonal trend - only do it once
if(F){
  wea.cn = read.csv(paste0(dir_data,'wea.station.cn.code.csv')) %>% data.table
  source(paste0(dir_code, 'Fn_get_seasonalR0.R'))
  parms=c("temp",'spec.hum',"rh" ,"dew_point", "atmos_pres")
  wea.files = list.files(dir_sn)
  wea.files = "wea.raw_SF2000-2021.RData"  # do south africa only
  # ff = "wea.raw_SF2000-2021.RData"
  stations = get_station_metadata() %>% data.table()
  # wea.files = wea.files[grepl('wea.by.week', wea.files)]
  
  # find the province /state for each station base on lat / long
  # using https://developers.google.com/maps/documentation/geocoding/overview?csw=1
  # need an account for this
  reqUrl <- url(paste0('https://maps.googleapis.com/maps/api/geocode/json?latlng=',-23.683,',', 27.7, '&sensor=true'))
  fromJSON(reqUrl) %>% data.table
  
  
  {
    library(sp)
    library(rworldmap)
    library(rworldxtra)
    
    # The single argument to this function, points, is a data.frame in which:
    #   - column 1 contains the longitude in degrees
    #   - column 2 contains the latitude in degrees
    coords2loc = function(points){  
      # countriesSP <- getMap(resolution='low')
      # countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
      countriesSP <- readShapePoly(paste0(dir_map, 'zaf_admbnda_adm1_sadb_ocha_20201109.shp'))
      # convert our list of points to a SpatialPoints object
      
      # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      
      #setting CRS directly to that from rworldmap
      pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
      
      
      # use 'over' to get indices of the Polygons object containing each point 
      indices = over(pointsSP, countriesSP)
      # return the ADMIN names of each province
      indices$ADM1_EN 
      #indices$ISO3 # returns the ISO3 code 
      #indices$continent   # returns the continent (6 continent model)
      #indices$REGION   # returns the continent (7 continent model)
    }
  }
  sn.res = NULL
  for(ff in wea.files){
    
    load(paste0(dir_sn, ff))
    
    sta.t = stations[id %in% res$id]
    # find the state based on station lat long
    points = data.frame(lon = sta.t$lon, lat = sta.t$lat)
    sta.t$location = coords2loc(points)
    
    res = merge(res, sta.t[,c('id','country','location')], by = 'id')
    
    res$day = res$date %>% strftime(format = "%j") %>% as.numeric()
    tmp = res$date %>% MMWRweek 
    res$week = tmp[,'MMWRweek'] %>% as.numeric()
    res$year = res$date %>% strftime(format = '%Y') %>% as.numeric()
    # aggregate by day
    # res.d = res[, lapply(.SD, mean, na.rm =T), by = c('day'), .SDcols = parms]  # 
    # res.d = res.d[order(day)]
    
    # aggregate by week
    res.w = res[, lapply(.SD, mean, na.rm =T), by = c('location', 'week'), .SDcols = parms]
    res.w = res.w[order(location, week)]
    rm(res)
    
    cn.t = ff %>% stringi::stri_split(., regex =  '_') %>% unlist %>% tail(1) %>% substr(1,2)
    cn.name.t = wea.cn[country == cn.t]$country.name
    print(cn.t)
    # compute seasonal trend, for each location
    for(loc.t in unique(res.w$location)){
      if(is.na(loc.t))
        next
      num_ens = 2
      sn.res = rbind(sn.res, data.table(cn.code = cn.t, country =cn.name.t, location = loc.t, # ifelse(length(cn.name.t) == 0, 'NA', cn.name.t), 
                                        week = 1:53, value = fn_getSnRt(loc.t = loc.t, Rwea_parm.bounds, smooth = T, da_wea=res.w[location == loc.t])))
      
    }
    
  }
  
  write.csv(sn.res, paste0(dir_data, 'est.sn.2000t2020_sa_subnational.csv'), row.names = F)
  
  da.t = dcast(sn.res, week ~ location, value.var = 'value')
  matplot(da.t[, 2:ncol(da.t)], type = 'l')
  lines(da.t$Gauteng, col='red')
}

# check country data
DAT.EPI = read.csv(paste0(dir_data, 'da_case_death_sa_subnational',date.tag,'.csv')) %>% data.table()
DAT.EPI$date = DAT.EPI$date %>% as.Date
da.t = dcast(DAT.EPI[data.type == 'case'], date + year + week ~ country, value.var = 'value')

da.t = dcast(DAT.EPI[country=='Gauteng'], country + date + year + week ~ data.type, value.var = 'value')
matplot(da.t[, 3:ncol(da.t)], type = 'l')
lines(da.t$`KwaZulu-Natal`, col = 'red')

loc.t = 'KwaZulu-Natal'  # 'Eastern Cape'
loc.t = 'Free State'
loc.t = 'Western Cape'
loc.t = 'Eastern Cape'
loc.t = 'Gauteng'
da.t2 = dcast(DAT.EPI[country==loc.t], country + date + year + week ~ data.type, value.var = 'value')
da.t2 = da.t2[date >= as.Date('2020/3/15')]
da.t2$cfr = da.t2$death / da.t2$case * 100
matplot(da.t2[, 4:ncol(da.t2)], type = 'l')
abline(v = which(da.t$date == as.Date('2021/8/15')))



# hospitalization data:
loc.names = DAT.EPI$country %>% unique
# read the parm bounds for different stage
parm.bound_VEC = read.csv(paste0(dir_code, 'parm.bounds.csv'))  %>% data.table()

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/za_province_pop.csv'
pop = read_csv(url(url.t), col_names =F) %>% data.table() 
names(pop) = c('location','population')
pop[location == 'Northwest']$location = "North West"

url.t = 'https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_raw_hospitalization.csv'
hosp = read_csv(url(url.t)) %>% data.table() 
hosp = hosp[Owner=='Total' & variable == 'AdmissionstoDate']
hosp = dcast(hosp, Date ~ Province, value.var = 'value')
dwks = hosp$Date %>% as.Date
cum.hosp = NULL # cumulative hospitalization
for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  
  for(iv in 1:nrow(tm_variant)){
    idx0 = which(as.Date(dwks) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) # %>% dwks[.]
    idx1 = which(as.Date(dwks) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) # %>% dwks[.]
    
    # NOTE: NUMBERS ARE CUMULATIVE - convert to PER 100,000 POPULATION
    tmp = (hosp[idx1, gsub(' ','', loc.t),with=F] %>% unlist %>% as.numeric) - 
      (hosp[idx0, gsub(' ','', loc.t),with=F] %>% unlist %>% as.numeric)
    tmp = tmp / pop[location == loc.t]$population * 100e3
    cum.hosp = rbind(cum.hosp, 
                     data.table(loc = loc.t, variant = tm_variant[iv]$type, date.start = dwks[idx0], date.end =  dwks[idx1], hosp.per100k = tmp)
    )
    
  }
}
write.csv(cum.hosp, paste0(dir_data,'data_cum.hosp_byloc_bywave.csv'), row.names = F)

# excess mortality
provinces = data.table(code = c('WC', 'NC', 'EC', 'GT', 'KZN', 'MP', 'FS', 'LM', 'NW'), 
                       label = c('Western Cape', 'Northern Cape', 'Eastern Cape', 'Gauteng', 'KwaZulu-Natal', 
                                 'Mpumalanga', 'Free State', 'Limpopo', 'North West'))

# NOTE: NUMBERS ARE CUMULATIVE
# The South African Medical Research Council (SAMRC)
# data source: https://www.samrc.ac.za/reports/report-weekly-deaths-south-africa
d.excess = read_xlsx(paste0(dir_data,'Estimated deaths for SA 07 Mar 2022 with adj2.xlsx'), 
                     sheet = 'Total excess deaths per capita') %>% data.table()
# use adjusted (for population age structure) excess mortality 
d.excess = d.excess[2:nrow(d.excess),13:21]
colnames(d.excess) = d.excess[1,] %>% unlist
d.excess = d.excess[-1,]
d.excess$date = seq(as.Date('2020/5/3'), length.out = nrow(d.excess), by = 'week')
d.excess[date < as.Date('2020/12/1') & is.na(d.excess)] = 0
d.excess = d.excess[complete.cases(d.excess),]
dwks = d.excess$date %>% as.Date
cum.excess.d = NULL # cumulative excess death

for(loc.t in loc.names){
  
  # rough timing of diff waves
  tm_variant = parm.bound_VEC[country == 'South Africa' & grepl(loc.t, location) & parm == 'wave.start']
  tm_variant$date.start = tm_variant$date.start %>% as.Date()
  tm_variant$date.end = tm_variant$date.end %>% as.Date()
  
  for(iv in 1:nrow(tm_variant)){
    idx0 = which(as.Date(dwks) >=  as.Date(tm_variant[iv]$date.start)) %>% head(1) # %>% dwks[.]
    idx1 = which(as.Date(dwks) <=  as.Date(tm_variant[iv]$date.end)) %>% tail(1) # %>% dwks[.]
    
    # NOTE: NUMBERS ARE CUMULATIVE - PER 100,000 POPULATION
    tmp = (d.excess[idx1, provinces[label==loc.t]$code,with=F] %>% unlist %>% as.numeric) - 
      (d.excess[idx0, provinces[label==loc.t]$code,with=F] %>% unlist %>% as.numeric)
    
    cum.excess.d = rbind(cum.excess.d, 
                         data.table(loc = loc.t, variant = tm_variant[iv]$type, date.start = dwks[idx0], date.end =  dwks[idx1], excess.death.per100k = tmp)
    )
    
  }
}
# RES.CUMI$variant = factor(RES.CUMI$variant, levels = c('wt','beta','delta','omicron'), labels = c('Ancestral', 'Beta','Delta','Omicron'))
write.csv(cum.excess.d, paste0(dir_data,'data_cum.excess.death_byloc_bywave.csv'), row.names = F)



# for waves with large increase in S, check when S reach the peak
date.start = '2020/3/15' %>% as.Date
Week.starts = DAT.EPI$date %>% unique %>% sort %>% .[.>=date.start]



