############# ONLY TO INSTALL TMB & local VAST copy
source("setup_pkgs.R")
#########################

library(VAST)
library(compiler)

require(TMB)
library(VAST)
require(here)
devtools::install_github("james-thorson/FishStatsUtils" ,ref ="development",  dependencies=FALSE)
Version = get_latest_version()

library(dplyr)
#Read in raw data
#
make_data <- function(){
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
devtools::load_all()
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


pollock$spp <- 0
pcod$spp <- 1
yellowfin$spp <- 2
flathead$spp <- 3

multi <- rbind(pollock, pcod, yellowfin, flathead)
save(multi, file="JoinedEBSData.RData")
return(multi)
}


load("data/JoinedEBSData.RData")
cold_pool<-read.csv("./data/cpa_areas2018.csv")
pollock_bio <- read.csv("./data/PollockDensity.csv")
enso_pdo <- read.csv("./data/ENSOIndex.csv")
enso_pdo_mean <- enso_pdo %>%
  rowwise() %>%
  mutate(mean_enso = mean(c(DECJAN,JANFEB,FEBMAR,MARAPR,APRMAY,MAYJUN,JUNJUL,JULAUG,AUGSEP,SEPOCT,OCTNOV,NOVDEC), na.rm=T), 
         mean_pdo = mean(c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec), na.rm=T)) %>%
  select(YEAR,mean_enso, mean_pdo)

multi <- get(load(file="./data/JoinedEBSData.RData"))
cold_pool$TEMP <- scale(cold_pool)
#scale df
scaled_env<-  cold_pool[,-1] %>%
  transmute_all(scale) %>%
  cbind(year = cold_pool$YEAR)

#Plot
require(reshape2)
require(ggplot2)
Molten <- melt(scaled_env, id.vars = "year")
ggplot(Molten, aes(x = year, y = value, colour = variable)) + geom_line()


#cold_pool$AREA_SUM_LTE2 <- scale(cold_pool$AREA_SUM_LTE2)
#cold_pool$unscaled_temp <- scale(cold_pool$TEMP)

multi <-  left_join(multi, cold_pool, by = c("Year" = "YEAR"))
multi <- left_join(multi, enso_pdo_mean, by=c("Year"="YEAR"))
multi$CPTemp <- scale(multi$TEMP)
multi <- left_join(multi, pollock_bio, by = c("Year"="Year"))
multi$ScaledDens <- scale(multi$Nfish)

run_one_spp(multi, config_file="vast_config_multi_2fac_3s_4st_finescale.R",
            folder_name="twot_3s_4st_rw_finescale", region = "Eastern_Bering_Sea", fine_scale=TRUE)

run_one_spp(multi, config_file="vast_config_multi_2fac_3s_4st.R",
            folder_name="twot_3s_4st_PDO_rw", region = "Eastern_Bering_Sea", covar_columns = c("mean_pdo"), annual_ts = enso_pdo_mean$mean_pdo)


run_one_spp(multi, config_file="vast_config_multi_2fac_3s_4st.R",
            folder_name="twot_3s_4st_dens_rw", region = "Eastern_Bering_Sea", covar_columns = c("ScaledDens"), annual_ts = scale(pollock_bio$Nfish))

run_one_spp(multi, config_file="vast_config_multi_2fac_3s_4st_finescale.R",
            folder_name="twot_3s_4st", region = "Eastern_Bering_Sea",
            fine_scale = TRUE)

run_one_spp(multi, config_file="vast_config_multi_IID.R",
            folder_name="four_factor")

# pop = build_corrected_df(clean_data,species_code =30060,
#                           sex=2, age="8", renames)
nrocksole = build_corrected_df(clean_data, species_code=10261, sex = 2, age = "4", renames)


data_list <- list(pcod, pollock, yellowfin, nrocksole, arrow, flathead)
spp <- c("pcod", "pollock", "yellowfin", "nrocksole",
         "arrowtooth", "flathead")
config_list <- paste("vast_config_", spp, ".R", sep="")

possible_covars <- c("temp", "depth","")
covariance <- c("spatio", "spatiotemp")

run_one_spp(pcod, config_file=config_list[[i]],
            folder_name=paste(spp[i],covariance[1],possible_covars[2],sep="_"))

run_one_spp(nrocksole, config_file=config_list[[4]],
            folder_name="nrock_spatiotemp_poisson_depth", 
            covar_columns=c(possible_covars[2]))

devtools::install_github("james-thorson/VAST" , dependencies=FALSE)

best_out<- get(load("Save.RData"))
MapDetails_List <- get(load("MapDetails.RData"))


Dens_xt = plot_maps(plot_set=c(7), MappingDetails=MapDetails_List[["MappingDetails"]], Report=best_out$Report, Sdreport=best_out$Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=sort(unique(multi$Year)), Years2Include=c(5,19,33), Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

factors <- FishStatsUtils::plot_factors(Report= best_out$Report, ParHat = best_out$ParHat, Data = best_out$TmbData, SD = best_out$Opt$SD, mapdetails_list = MapDetails_List,
                                        Year_Set = seq(1982,2018), category_names=c(0,1,2,3), plotdir="")

factors <- FishStatsUtils::plot_factors(Report= best_out$Report, ParHat = best_out$ParHat, Data = best_out$TmbData, SD = best_out$Opt$SD, Year_Set = seq(1982,2018), category_names=c(0,1,2,3), plotdir="")



png("Beta2_1.png")
plot(factors$Rotated_factors$Beta2[,1,]~sort(unique(multi[,'Year'])), type="l", ylab = "Factor", xlab = "Year")
save(factors, file="BestModFactors.RData")

png("Beta2_2.png")
plot(factors$Rotated_factors$Beta2[,2,]~sort(unique(multi[,'Year'])), type="l",  ylab = "Factor", xlab = "Year")
dev.off()

L_pj <- factors$Rotated_loadings$Omega2
par(mai = c(1, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0), mfcol=c(2,2))
#plot(factors$Rotated_factors$Beta2[,1,]~tot_years, type="l", ylab = "Factor", xlab = "Year", las=1)
#abline(h=0)

#plot(factors$Rotated_factors$Beta2[,2,]~tot_years, type="l",  ylab = "Factor", xlab = "Year", las=1)
#abline(h=0)
png("SpatialLoadings.png")
plot_loadings(L_pj, tot_years,3)
dev.off()


res <- read_results("WhichModels.csv")

MDFAdirs <- c("./two_temp_3s_4st_factor", "./twot_3s_4st_cpa",
              "./twot_3s_4st_cptemp") 
output_list <- vector("list")
AIC <- rep(0,length(MDFAdirs))
for(i in 1:length(MDFAdirs)){
  output_list[[i]] <- get(load(paste0(MDFAdirs[i],"/Save.Rdata")))
  AIC[i] <- output_list[[i]][["Opt"]][["AIC"]][1]
}

res <- read_results("WhichModels.csv")
params <- names(res)[10:53]

param_types <- c("H", "omega2","epsilon2","beta2",
                 "Beta_mean2","logSigmaM", "gamma",
                 "Beta_rho2","kappa", "AIC", "max_gradient")
loong <- tidyr::pivot_longer(res, cols=params)
loong$type=NA
for(i in param_types){
  ind <- grep(i,loong$name)
  loong$type[ind]<-i
}

loong$shortmod <- sub("twot_3s_4st_","",loong$Folder)

toplot <- c("beta2","epsilon2","omega2")
loong %>% 
  filter(type==toplot[1]) %>%
 ggplot(aes(x=shortmod, y=value, color=name, type)) +
  geom_point() 


loong_nofine <- filter(loong, Fine.scale==FALSE)
for(i in 1:length(toplot)){
each[[i]] <- loong_nofine %>% filter(type==toplot[i]) %>% 
  group_by(Covariates) %>%
  summarise(mean(abs(value)))
}
names(each) <- toplot


