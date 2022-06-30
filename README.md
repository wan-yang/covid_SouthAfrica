## Code and data for: Yang & Shaman. 2022 COVID-19 pandemic dynamics in South Africa and epidemiological characteristics of three variants of concern (Beta, Delta, and Omicron). medRxiv:2021.2012.2019.21268073.


### data
This folder includes all relevant data needed to run the model-inference and simulations

### scripts
This folder includes all model code used in this study. 
Note that some of the supporting scripts may be found in https://github.com/wan-yang/covid_voc_study

### results
This folder includes compiled model-inference results, as labeled. 
Note that because the files are too large, we do not include results from individual runs. For interested readers, please run those using the provided code on your local machine and then compile the results using the analysis code. 
Note: result file "res.cumIperc_ens.RData" is too large to include - similar to other results, it can be compiled using your own runs and the provided scripts.

### analysis steps
1. compile data, using the script "compile_sa_subnational_data.R"
2. run model-inference using the script "driver_sa_inference.R"
3. run retrospective predictions using the script "driver_sa_inferenceNproj.R"
4. combine individual runs, using "combruns.R"
5. analyze results, using "analysis_all_runs_sa.R"
6. visualize results, using "plotRes_sa.R"
7. (optional) preliminary estimation of re-infection rate, using "est_reinfect_rate.R"

### contact
wy2202 at cumc.columbia.edu

### reference
Version 1: 
Yang W & Shaman J (2021) SARS-CoV-2 transmission dynamics in South Africa and epidemiological characteristics of the Omicron variant. medRxiv:2021.2012.2019.21268073.

Version 2:
Yang W & Shaman J (2022) COVID-19 pandemic dynamics in South Africa and epidemiological characteristics of three variants of concern (Beta, Delta, and Omicron). medRxiv:2021.2012.2019.21268073.

Version 3: 
Yang W & Shaman J (2022) COVID-19 pandemic dynamics in South Africa and epidemiological characteristics of three variants of concern (Beta, Delta, and Omicron). medRxiv:2021.2012.2019.21268073.
https://www.medrxiv.org/content/10.1101/2021.12.19.21268073v3
