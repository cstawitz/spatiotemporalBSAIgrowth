#Config VAST
#Header
require(dplyr)
# devtools command to get TMB from GitHub
require(devtools)
install_github("kaskr/adcomp/TMB") 
# source script to get INLA from the web
source("http://www.math.ntnu.no/inla/givemeINLA.R")  
library(TMB)
devtools::install_github("james-thorson/VAST", ref="development")
devtools::install_github("james-thorson/FishStatsUtils")
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
             'Lat','Lon',
             'length')
pollock <- filter(raw_data, SPECIES_CODE==21740, Sex==2)
cod <- filter(raw_data, SPECIES_CODE==21720, Sex==2)
arrow <- filter(raw_data, SPECIES_CODE==10110, Sex==2)
cod.lengths <- get_unbiased_lengths(cod, "AGE", "LENGTH..cm.", "YEAR", "STATIONID")
corrected_age4<- data.frame(cbind(row.names(cod.lengths),select(cod.lengths, "4")))
arrow <- filter(raw_data, SPECIES_CODE==10110, Sex==2)
arw.lengths <- get_unbiased_lengths(arrow, "AGE", "LENGTH..cm.", "YEAR", "STATIONID")
corrected_age7<- data.frame(cbind(row.names(arw.lengths),select(arw.lengths, "7")))
names(corrected_age7) <- c("ID", "lengths")
yrs <- unique(substr(rownames(pol.lengths),1,4))
row.index <- lapply(1:length(yrs), function(x) which(substr(rownames(pol.lengths),1,4)==yrs[x]))

plot(NA, xlim=as.numeric(c(min(yrs),max(yrs))), ylim=c(min(pol.lengths[,7:10], na.rm=T), max(pol.lengths[,7:10], na.rm=T)), 
     las=1, xlab="Years", ylab="Size (cm)")
lapply(7:10, function(y)
  lapply(1:length(yrs), function(x) points(pol.lengths[row.index[[x]],y]~rep(yrs[x],length(row.index[[x]])), col=y)))

sample.sizes <-purrr::map(list(pollock, cod, arth), get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))
sample.sizes.yr <- split(arth, arth$YEAR) %>% purrr::map(get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))


cv.length.age <- split(arrow, arrow$AGE) %>% purrr::map(get_length_cv, name=c("YEAR")) %>% unlist()
cv.length.age <- purrr::map(list(pollock, cod, arrow),
                            get_length_cv, name="AGE", length.name="LENGTH..cm.")
pdf("LengthAge.pdf")
for(i in 1:3){
  plot(CV~AGE, data=cv.length.age[[i]])
}
dev.off()
  pollock = create_data(raw_data,species=21740,
                         sex=2, age=6, renames)
  pcod = create_data(raw_data, species=21720, sex=2, age=4, renames)
  arrowtooth = create_data(raw_data, species=10110, sex=2, age=7, renames)

Data_Geostat <- left_join(arrowtooth, corrected_age7, by="ID") %>% select(Year, station, Lat, Lon, AreaSwept_km2,Vessel, Catch_KG=lengths)

Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region=Region, strata.limits=strata.limits )

Spatial_List = FishStatsUtils::make_spatial_info(grid_size_km=grid_size_km, n_x=n_x, Method=Method, 
                                                         Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], 
                                                         Extrapolation_List=Extrapolation_List, 
                                                         randomseed=Kmeans_Config[["randomseed"]], 
                                                         nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
                                                         DirPath=DateFile, Save_Results=FALSE)
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                  "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                  "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1, 
                  "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                  "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                  "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )

require(TMB)
TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                       "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize(obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl"))

Report = Obj$report()
Save=list("Opt"=Opt, "Report"=Report, "ParHat"= Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile, "Save.RData"))
cod.obj <- load("Save.RData")
plot_data(Extrapolation_List, Spatial_List, Data_Geostat,PlotDir=DateFile)



## Convergence

pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 

## Diagnostics for positive-catch-rate component

Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP=paste0(DateFile,"Posterior_Predictive.jpg"),
                             FileName_Phist=paste0(DateFile,"Posterior_Predictive-Histogram.jpg"), 
                             FileName_QQ=paste0(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=paste0(DateFile,"Q-Q_hist.jpg"))


## Diagnostics for plotting residuals on a map


# Get region-specific settings for plots
MapDetails_List = make_map_info( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
Years2Include = Year_Set

plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

 Report <- Save$Report
 Opt <- Save$Opt
Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )

pander::pandoc.table( Dens_DF[1:6,], digits=3 )



## Index of abundance
Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )
pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 
