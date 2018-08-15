#Config VAST
#Header
require(dplyr)
# devtools command to get TMB from GitHub
require(devtools)
install_github("kaskr/adcomp/TMB") 
# source script to get INLA from the web
source("http://www.math.ntnu.no/inla/givemeINLA.R")  
library(TMB)
with_libpaths(new = "C:/Users/Christine.Stawitz/R_LIBS", install_github("cstawitz/VAST"))
devtools::install_github("ropensci/drake")
devtools::load_all("C:/Users/chris/Documents/VAST")
library(VAST)
library(drake)
library(compiler)
library(dplyr)
Version = "VAST_v5_2_0"


source("R/vast_config.R")


#Read in raw data
raw_data <- read.csv("./data/EBSLengths.csv")
renames <- c('Year', 'station',
             'Lat','Lon','AreaSwept_km2',
             'length')
pollock_raw <- filter(raw_data, SPECIES_CODE==21740, Sex==2)
pol.lengths <- get_unbiased_lengths(pollock, "AGE", "LENGTH..cm.", "YEAR", "STATIONID")
corrected_age6<- data.frame(cbind(row.names(pol.lengths),select(pol.lengths, "6")))
names(corrected_age6) <- c("ID", "lengths")
yrs <- unique(substr(rownames(pol.lengths),1,4))
row.index <- lapply(1:length(yrs), function(x) which(substr(rownames(pol.lengths),1,4)==yrs[x]))

plot(NA, xlim=as.numeric(c(min(yrs),max(yrs))), ylim=c(min(pol.lengths[,7:10], na.rm=T), max(pol.lengths[,7:10], na.rm=T)), 
     las=1, xlab="Years", ylab="Size (cm)")
lapply(7:10, function(y)
  lapply(1:length(yrs), function(x) points(pol.lengths[row.index[[x]],y]~rep(yrs[x],length(row.index[[x]])), col=y)))

cod <- filter(raw_data, SPECIES_CODE==21720, Sex==2)
arth <- filter(raw_data, SPECIES_CODE==10110, Sex==2)

sample.sizes <-purrr::map(list(pollock, cod, arth), get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))
sample.sizes.yr <- split(arth, arth$YEAR) %>% purrr::map(get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))

  pollock = create_data(raw_data,species=21740,
                         sex=2, age=6, renames)
  pcod = create_data(raw_data, species=21720, sex=2, age=4, renames)
  arrowtooth = create_data(raw_data, species=10110, sex=2, age=7, renames)

Data_Geostat <- left_join(pollock, corrected_age6, by="ID") %>% select(Year, station, Lat, Lon, AreaSwept_km2,Vessel, Catch_KG=lengths)

Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )

Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn(grid_size_km=grid_size_km, n_x=n_x, Method=Method, 
                                                         Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], 
                                                         Extrapolation_List=Extrapolation_List, 
                                                         randomseed=Kmeans_Config[["randomseed"]], 
                                                         nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
                                                         DirPath=DateFile, Save_Results=FALSE)
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                  "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                  "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], 
                  "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                  "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                  "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )


TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                       "loc_x"=Spatial_List$loc_x, "Method"=Method)
