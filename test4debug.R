#source("Config_files/vast_config.R")
#library(VAST)
#library(compiler)
#library(dplyr)
require(TMB)
require(VAST)
Version = get_latest_version()

#Drop 1977 from temperature data
#Repull data with 2017 ages
#Combine eureka columbia & vancouver - keep monterey and conception separate
#VBGF K relationship with temp for tim's models
# length at age 1 vs growth increment 0 - 1
#Where are young fish? Tim used lat/long to get the temp
#lag temp from previous year
# spatial random effect on k parameter - length at age be the
# change in size from age to next
load("./data/CalCurrDataListOlder.Rmd")

#run_safe_spp <- function()
#  args <- list(Data_Geostat=data_for_mod[[1]], config_file="vast_config_hake.R",
#               folder_name="hake_rw_age0", region = "California_current", Version = Version)
#callr::r(run_one_spp,args)
devtools::load_all(".")
for(i in 1:length(data_for_mod)){
  data_for_mod[[i]]$spp <- i-1
}
require(dplyr)
multi_cc <- bind_rows(data_for_mod)
  Species_set = unique(multi_cc$spp)
  
  
  remotes::install_github("nwfsc-assess/VASTWestCoast")
  library(VASTWestCoast)
  
  run_one_spp(data_for_mod[[5]], config_file="vast_config_CC.R",
              folder_name=paste0("onespp_test_CC"), region = "California_current", Version = Version)
run_one_spp(multi_cc, config_file="vast_config_CC.R",
            folder_name=paste0("multi_CC_rw_age0"), region = "California_current", Version = Version)
#}
