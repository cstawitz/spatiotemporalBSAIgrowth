source("Config_files/vast_config.R")
library(VAST)
library(compiler)
library(dplyr)
require(TMB)
devtools::install_local("C:/Users/chris/Downloads/VAST-3.5.1.tar.gz")
devtools::install_github("james-thorson-noaa/VAST", dependencies=FALSE)
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
load("./data/CalCurrDataList.Rmd")
data_for_mod[[1]]$spp <- 1

#run_safe_spp <- function()
#  args <- list(Data_Geostat=data_for_mod[[1]], config_file="vast_config_hake.R",
#               folder_name="hake_rw_age0", region = "California_current", Version = Version)
#callr::r(run_one_spp,args)

run_one_spp(data_for_mod[[1]], config_file="vast_config_hake.R",
            folder_name="hake_rw_age0", region = "California_current", Version = Version)

CC_temp<-read.csv("./data/CC_temps.csv")


library(tidyr)
fill_in <- CC_temp %>% complete(area, depth_bin, year=1977:2018)


sable3<- fill_in %>% filter(area =="ECV", depth_bin=="184-550m")
sable3[which(sable3$avg_temp==-9),"avg_temp"] <- NA
na.inds <- which(is.na(sable3$avg_temp))
na.inds <- c(na.inds, which(sable3$avg_temp==-9))
non.nas <- which(!(1:nrow(sable3)) %in% na.inds)
for(i in na.inds){
  sable3$avg_temp[i] <- mean(sable3$avg_temp, na.rm=T)
}

hake1 <- filter(sable, year<=2013)
png("temp.png")
plot(sable3$avg_temp ~ sable3$year, type="l", xlab="Year", ylab="Average temperature", las = 1)
dev.off()

haketemps
age1t <-  left_join(age1, haketemps, by = c("Year" = "year"))
data_for_mod$spp <- 1
pet5 <- filter(pet5, year>=2003)
sable3 <- filter(sable3, year>=1983)

run_one_spp(data_for_mod, config_file="vast_config_sable.R",
            folder_name="sable3_s_rw", region = "California_current")
, covar_columns = c("temperature"), annual_ts = sable3$avg_temp-mean(sable3$avg_temp))  

bestmods <- paste0("CalCurrent/",c("sable3_s_rw", "dbrock_rw_age4"))
get_trend_plot(bestmods, 1982:2018, c("sablefish", "darkblotched"))
