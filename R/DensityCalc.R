require(here)
require(dplyr)
setwd(here())
densEBS <- read.csv(here("data/FocalSpp.csv"))
pcod <- filter(densEBS, COMMON=="Pacific cod")
Data_Geostat <- data_process(pcod, renames = c("Lat", "Lon", "Year", "CPUE", "Depth", "Temp"),
                             id.vars = c("Lat",
                                         "Lon",
                                         "Year"),
                             response.var = "CPUE", null.values=NULL,
                             LATITUDE, LONGITUDE, YEAR, NUMCPUE, BOT_DEPTH, BOT_TEMP)

strata.limits <- data.frame('STRATA'="All_areas")
Region = "Eastern_Bering_Sea"

Extrapolation_List = make_extrapolation_info( Region=Region, strata.limits=strata.limits)
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 50
n_x = 100   # Specify number of stations (a.k.a. "knots")
DateFile = paste0(getwd(),'/VAST_output/')
dir.create(DateFile)

Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, DirPath=DateFile, Save_Results=FALSE )

# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) 
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
ObsModel = c(2,3) 
Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1, "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_Kg'], "s_i"=Data_Geostat[,'knot_i']-1, "a_i" = rep(1,nrow(Data_Geostat)),"c_iz" = rep(0,nrow(Data_Geostat)), "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )


