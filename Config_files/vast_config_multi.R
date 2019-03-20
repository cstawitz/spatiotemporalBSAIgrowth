
#Spatial settings - might need to change?
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 50
n_x = c(50, 100, 250, 500, 1000, 2000)[2] # Number of stations
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )

OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
# Using 4 factors for spatial, spatio-tempral and intercepts for beta2, and no variation in beta1
FieldConfig = matrix( c(0,4, 0,4, "IID",4), byrow=TRUE, ncol=2 )
# Using AR1 for beta2
RhoConfig = c("Beta1"=0, "Beta2"=4, "Epsilon1"=0, "Epsilon2"=0)
# Using conventional delta-model so that I can fix beta1 = 20 to have Pr(B>0)->1, but doesn't affect interpretation of beta2
#Lognormal instead of normal?
ObsModel = c(1,0)   

#Postprocessing
Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, 
             "Calculate_Range"=0, "Calculate_evenness"=0, 
             "Calculate_effective_area"=1, "Calculate_Cov_SE"=0, 
             'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

#Strata limits
strata.limits <- data.frame('STRATA'="All_areas")

#Set region and species
Region = "Eastern_Bering_Sea"
Species_set = c(0, 1, 2, 3) 
strata.limits <- data.frame('STRATA'="All_areas")

#We then set the location for saving files.
unlink(paste0(getwd(),'/VAST_output/'), recursive = T)
DateFile = paste0(getwd(),'/VAST_output/')
dir.create(DateFile)

#I also like to save all settings for later reference, although this is not necessary.
Record = list("Version"=Version,"Method"=Method,
              "grid_size_km"=grid_size_km,"n_x"=n_x,
              "FieldConfig"=FieldConfig,"RhoConfig"=RhoConfig,
              "OverdispersionConfig"=OverdispersionConfig,
              "ObsModel"=ObsModel,"Kmeans_Config"=Kmeans_Config,
              "Region"=Region,"Species_set"=Species_set,
              "strata.limits"=strata.limits)
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))