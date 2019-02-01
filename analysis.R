############# ONLY TO INSTALL TMB & local VAST copy
source("setup_pkgs.R")
#########################

source("Config_files/vast_config.R")
library(VAST)
library(compiler)
library(dplyr)
require(TMB)
require(VAST)
require(here)
Version = get_latest_version()

#Read in raw data
raw_data <- read.csv("./data/ebs_Updated.csv")

#Data Check
clean_data <- raw_data %>% filter(!is.na(LENGTH..cm.))

#Vector to rename columns to
renames <- c('Year',
             'Lat','Lon',
             'length', 'depth', 'temp')

#Code to get age with largest sample sze for a species 
#filter(clean_data, SPECIES_CODE==10261, !is.na(AGE)) %>% group_by(AGE) %>% mutate(count=n()) %>%
 # select(AGE, count) %>% filter(count>861)


#Build Data_Geostat for each spp
# sablefish = build_corrected_df(clean_data,species_code = 20510,
#                       sex=2, age="4", renames)
pcod = build_corrected_df(clean_data,species_code = 21720,
                                sex=2, age="4", renames)

yellowfin = build_corrected_df(clean_data,species_code = 10210,
                                     sex=2, age="7", renames)
flathead = build_corrected_df(clean_data,species_code = 10130,
                               sex=2, age="4", renames)

# atka = build_corrected_df(clean_data,species_code =21921,
#                              sex=2, age="3", renames)

arrow = build_corrected_df(clean_data,species_code =10110,
                          sex=2, age="5", renames)
pollock = build_corrected_df(clean_data,species_code =21740,
                           sex=2, age="5", renames)

# pop = build_corrected_df(clean_data,species_code =30060,
#                           sex=2, age="8", renames)
nrocksole = build_corrected_df(clean_data, species_code=10261, sex = 2, age = "4", renames)


data_list <- list(pcod, pollock, yellowfin, nrocksole, arrow, flathead)
spp <- c("pcod", "pollock", "yellowfin", "nrocksole",
         "arrowtooth", "flathead")
config_list <- paste("vast_config_", spp, ".R", sep="")

possible_covars <- c("temp", "depth","")
covariance <- c("spatio", "spatiotemp")

run_one_spp(data_list[[i]], config_file=config_list[[i]],
            folder_name=paste(spp[i],covariance[2],possible_covars[2],sep="_"), 
            covar_columns=c(possible_covars[2]))

run_one_spp(nrocksole, config_file=config_list[[4]],
            folder_name="nrock_spatiotemp_poisson_depth", 
            covar_columns=c(possible_covars[2]))





