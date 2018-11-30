############# ONLY TO INSTALL TMB & local VAST copy
source("setup_pkgs.R")
#########################

source("vast_config.R")
library(VAST)
library(compiler)
library(dplyr)
Version = "VAST_v5_3_0"

check_dat <- function(sp_code){
tmp<-filter(clean_data, SPECIES_CODE==sp_code)
png(paste0("datacheck",sp_code,".png"))
plot(LENGTH..cm.~AGE, data=tmp)
dev.off()
}
check_dat(10261)
spp<- unique(clean_data$SPECIES_CODE)
lapply(spp,check_dat)
#Read in raw data
raw_data <- read.csv("./data/ebs_Updated.csv")

#Data Check
clean_data <- raw_data %>% filter(!is.na(LENGTH..cm.))

#Vector to rename columns to
renames <- c('Year',
             'Lat','Lon',
             'length', 'depth', 'temp')
filter(clean_data, SPECIES_CODE==10130, !is.na(AGE)) %>% group_by(AGE) %>% mutate(count=n()) %>%
  select(AGE, count) %>% filter(count>1310)
#Build Data_Geostat for each spp
sablefish = build_corrected_df(clean_data,species_code = 20510,
                      sex=2, age="4", renames)
pcod = build_corrected_df(clean_data,species_code = 21720,
                                sex=2, age="4", renames)

yellowfin = build_corrected_df(clean_data,species_code = 10210,
                                     sex=2, age="7", renames)
flathead = build_corrected_df(clean_data,species_code = 10130,
                               sex=2, age="4", renames)

#Best pollock model for largest sample size
atka = build_corrected_df(clean_data,species_code =21921,
                             sex=2, age="3", renames)

arrow = build_corrected_df(clean_data,species_code =10130,
                          sex=2, age="5", renames)
arrow4 = build_corrected_df(clean_data,species_code =10130,
                           sex=2, age="4", renames)

pop = build_corrected_df(clean_data,species_code =30060,
                          sex=2, age="8", renames)
nrocksole
pcod <- filter(pcod, Year>1985)
Data_Geostat <- arrow



run_one_spp(pcod, config_file="vast_config_pcod.R",
            folder_name="pcod_spatiot")


#Best pollock model for largest sample size
pollock = build_corrected_df(clean_data,species_code =21740,
                             sex=2, age="7", renames)
Data_Geostat <- pollock


pos <- filter(pcod, Lon<0)
for(i in unique(pcod$Year)){
  png(paste("pcod",i,".png"))
plot(Lat~Lon, cex = 0.01, main = i, data=filter(pos, Year==i))
map("world", add=T)
dev.off()
}


#Format covariates for pollock model
covsperknot <- suppressMessages(FishStatsUtils::format_covariates(
  Lat_e = yellowfin$Lat,
  Lon_e = yellowfin$Lon,
  t_e = yellowfin$Year,
  Cov_ep = yellowfin[,"depth"],
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  FUN = mean,
  Year_Set = sort(unique(yellowfin$Year)),
  na.omit = "time-average"))
#X_xtp <- array(data=NA, dim=c(100,33,2))
X_xtp <- apply(covsperknot$Cov_xtp, 2:3, scale)
#X_xtp[, , 2] <- scale(exp(Dens_xt))
dimnames(X_xtp)[[1]] <- dimnames(covsperknot$Cov_xtp)[[1]]

run_one_spp(Data_Geostat, config_file="vast_config_pollock",
            folder_name="Pollock_Spatial",
            covars=X_xtp)


pcod = build_corrected_df(clean_data, species_code =21720, 
                          sex=2, age="4", renames)
pcod = filter(pcod, Year>=1988)

#Format covariates for pollock model

run_one_spp(Data_Geostat=pcod, config_file="vast_config_pcod.R",
            folder_name="Pcod_Spatiotemp_Depth",
            covar_columns=c("depth"))


bestmods <- c("pcod_spatiotemp", "VAST_output_pollock_spatiotemp_depth", "yellowfin_spatio_depth", "nsole_spatio_depth")
filedirs<-paste0(here(),"/",bestmods, "/parameter_estimates.RData")

sd_vect <- paramest <- ts <- vector("list")
ind2 <- c(32, 39, 39, 25)
ind1 <- c(3,4,4,4)
for(i in 1:4){
 load(filedirs[i])
  sd_vect[[i]] <- parameter_estimates$SD
  paramest[[i]] <- parameter_estimates$par
  ts[[i]] <- paramest[[i]][ind1[i]:ind2[i]]
}

results_tx<-rbind(c(rep(NA,6),
                    unname(ts[[1]])),
                  unname(ts[[2]]),
                  unname(ts[[3]]),
                  c(rep(NA,14),
                    unname(ts[[4]])))

png("pollockvyellowfin.png")
plot(results_tx[2,]~years, type="l", ylim=c(3,4.1))
lines(results_tx[3,]~years, col="blue")
dev.off()

strsplit(outputs[[1]]$X.par, " ")
unlist(outputs[[2]]$X.par)[1:18]
unlist(outputs[[3]]$X.par)[1:22]
unlist(outputs[[4]]$X.par)[1:12]
?read.delim
