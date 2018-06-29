#Config VAST
#Header
require(dplyr)
library(TMB)
devtools::install_github("james-thorson/VAST")
devtools::install_github("ropensci/drake")
library(VAST)
library(drake)
library(compiler)
Version = "VAST_v2_5_0"


source("R/vast_config.R")


#Read in raw data
raw_data <- read.csv("./data/EBSLengths.csv")
renames <- c('Year', 'station',
             'Lat','Lon','Area_Swept_km2',
             'length')


data_plan <- drake_plan(
  pollock = create_data(raw_data,species=21740,
                         sex=2, age=7),
  pcod = create_data(raw_data, species=21720, sex=2, age=4),
  arrowtooth = create_data(raw_data, species=10110, sex=2, age=4)
)


Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateFile, Save_Results=FALSE )
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, 
                  "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "c_i"=rep(0,nrow(Data_Geostat)), 
                  "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], 
                  "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, 
                  "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                  "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )

methods <- drake_plan(
  

)

my_analyses <- plan_analyses(methods, data=data_plan)


