# script to combine multiple runs

num_runs = 100
stoch = T


date.tag = '_2021-12-19'  # v1
date.tag = '_2022-03-07' # v2


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

# SET DIRECTORY
dir_data = '../data/'
dir_code = '../scripts/'
dir_res = '../results/test/'

N = 1e6 # population size, for getting susceptibility, i.e. S/N

## LARGE FILES, SO THESE ARE DONE ON LOCAL MACHINE

# combine all model-inference estimates
source(paste0(dir_code,'getCombinedRes.R'))
res.train00 = res.train
save(newVstat, res.train, file = paste0(dir_res, 'res.summary',date.tag,'.RData'))

# get posterior estimates of cumulative infection rate (cumI), 
# these are values for individual ensemble members for comoputing summary stats
source(paste0(dir_code,'getCombinedRes_cumI.R'))  # very large
save(cumIperc_ens, file = paste0(dir_res, 'res.cumIperc_ens',date.tag,'.RData'))


## combine all retrospective predictions
source(paste0(dir_code,'getCombinedRes_rproj.R'))
