############# ONLY TO INSTALL TMB & local VAST copy
source("setup_pkgs.R")
#########################


library(VAST)
library(compiler)
library(dplyr)
Version = "VAST_v5_3_0"

#Read in raw data
raw_data <- read.csv("./data/EBSLengths.csv")

#Data Check
clean_data <- raw_data

#Vector to rename columns to
renames <- c('Year', 'station',
             'Lat','Lon',
             'length', 'depth', 'temp')

#Build Data_Geostat for each spp
arrowtooth = build_corrected_df(clean_data,species_code = 10110,
                      sex=2, age="7", renames)



#Best pollock model for largest sample size
pollock = build_corrected_df(clean_data,species_code =21740,
                             sex=2, age="7", renames)
Data_Geostat <- pollock

#Format covariates for pollock model
covsperknot <- suppressMessages(FishStatsUtils::format_covariates(
  Lat_e = pollock$Lat,
  Lon_e = pollock$Lon,
  t_e = pollock$Year,
  Cov_ep = pollock[,"depth"],
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  FUN = mean,
  Year_Set = sort(unique(pollock$Year)),
  na.omit = "time-average"))
X_xtp <- array(data=NA, dim=c(100,33,2))
X_xtp[,,1] <- apply(covsperknot$Cov_xtp, 2:3, scale)
X_xtp[, , 2] <- scale(exp(Dens_xt))
dimnames(X_xtp)[[1]] <- dimnames(covsperknot$Cov_xtp)[[1]]

run_one_spp(Data_Geostat, config_file="vast_config_pollock",
            folder_name="Pollock_Spatiotemp_Depth",
                        covars=X_xtp)


#Best pollock model for largest sample size
pollock = build_corrected_df(clean_data,species_code =21740,
                             sex=2, age="7", renames)
Data_Geostat <- pollock

#Format covariates for pollock model
covsperknot <- suppressMessages(FishStatsUtils::format_covariates(
  Lat_e = pollock$Lat,
  Lon_e = pollock$Lon,
  t_e = pollock$Year,
  Cov_ep = pollock[,"depth"],
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  FUN = mean,
  Year_Set = sort(unique(pollock$Year)),
  na.omit = "time-average"))
#X_xtp <- array(data=NA, dim=c(100,33,2))
X_xtp[,,1] <- apply(covsperknot$Cov_xtp, 2:3, scale)
#X_xtp[, , 2] <- scale(exp(Dens_xt))
dimnames(X_xtp)[[1]] <- dimnames(covsperknot$Cov_xtp)[[1]]

run_one_spp(Data_Geostat, config_file="vast_config_pollock",
            folder_name="Pollock_Spatiotemp_Depth",
            covars=X_xtp)


pcod = build_corrected_df(clean_data, species_code =21720, 
                          sex=2, age="4", renames)
pcod = filter(pcod, Year>=1988)

#Format covariates for pollock model

run_one_spp(Data_Geostat=pcod, config_file="vast_config_pcod.R",
            folder_name="Pcod_Spatiotemp_Depth",
            covar_columns=c("depth"))
