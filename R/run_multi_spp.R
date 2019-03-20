run_multi_spp <- function(Data_Geostat, config_file, folder_name,
                        covar_columns=NA){
  orig_dat <- Data_Geostat
  setwd(here())
  source(paste0("Config_files/",config_file))
  
  Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region="Eastern_Bering_Sea", strata.limits=strata.limits )
  Spatial_List = FishStatsUtils::make_spatial_info(grid_size_km=grid_size_km, n_x=n_x, Method=Method, 
                                                   Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], 
                                                   Extrapolation_List=Extrapolation_List, 
                                                   randomseed=Kmeans_Config[["randomseed"]], 
                                                   nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
                                                   DirPath=DateFile, Save_Results=FALSE)
  # Add knots to Data_Geostat
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

  if(!is.na(covar_columns)){
  covsperknot <- suppressMessages(FishStatsUtils::format_covariates(
    Lat_e = orig_dat$Lat,
    Lon_e = orig_dat$Lon,
    t_e = orig_dat$Year,
    Cov_ep = orig_dat[,covar_columns],
    Extrapolation_List = Extrapolation_List,
    Spatial_List = Spatial_List,
    FUN = mean,
    Year_Set = seq(min(unique(orig_dat$Year)),max(unique(orig_dat$Year))),
    na.omit = "time-average"))
  #X_xtp <- array(data=NA, dim=c(100,33,2))
#  X_xtp <-apply(covsperknot$Cov_xtp, 2:3, scale)
  X_xtp <-(covsperknot$Cov_xtp/mean(covsperknot$Cov_xtp))
  #X_xtp[, , 2] <- scale(exp(Dens_xt))
  dimnames(X_xtp)[[1]] <- dimnames(covsperknot$Cov_xtp)[[1]]


 
  TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                    "RhoConfig"=RhoConfig, "ObsModel"=ObsModel,
                    "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1,
                    "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                    "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList,
                    "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, X_xtp=X_xtp)
  Xconfig_zcp = TmbData$Xconfig_zcp
  Xconfig_zcp[1,,] = 0
  
  TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                    "RhoConfig"=RhoConfig, "ObsModel"=ObsModel,
                    "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1,
                     "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                     "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList,
                     "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, X_xtp=X_xtp, Xconfig_zcp = Xconfig_zcp)
  } else{

    TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                      "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                      "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1, 
                      "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                      "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                      "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options,
                      Aniso=1)
    Xconfig_zcp = TmbData$Xconfig_zcp
    Xconfig_zcp[1,,] = 0
    
    TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                      "RhoConfig"=RhoConfig, "ObsModel"=ObsModel,
                      "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1,
                      "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                      "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList,
                      "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, Xconfig_zcp = Xconfig_zcp)
  }
  
  TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                         "loc_x"=Spatial_List$loc_x, "Method"=Method)
  Obj = TmbList[["Obj"]]
  Opt = TMBhelper::Optimize(obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl"), loopnum=5)
  #, control = list(abs.tol = 1e-20))

  OutFile = paste0(getwd(),"/",folder_name)
  dir.create(OutFile)
  setwd(OutFile)
  Report = Obj$report()
  Save=list("Opt"=Opt, "Report"=Report, "ParHat"= Obj$env$parList(Opt$par), "TmbData"=TmbData)
  save(Save, file=paste0(DateFile, "Save.RData"))

  load(here("VAST_output/Save.RData"))
  Report <- Save$Report
  plot_data(Extrapolation_List, Spatial_List, Data_Geostat,PlotDir=DateFile)
  
  
  ## Convergence
  
  pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 
  
  ## Diagnostics for positive-catch-rate component
  
  Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive.jpg",
                                FileName_Phist="Posterior_Predictive-Histogram.jpg", 
                                FileName_QQ="Q-Q_plot.jpg", FileName_Qhist="Q-Q_hist.jpg")
  
  
  ## Diagnostics for plotting residuals on a map
  

  # Get region-specific settings for plots
  MapDetails_List = make_map_info( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
  # Decide which years to plot                                                   
  Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
  Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

  # plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

  plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )
  Report <- Save$Report
  Opt <- Save$Opt
  ParHat <- Save$ParHat

  Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
  
  save(MapDetails_List, file="MapDetails.RData")
  
  Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )
  
  pander::pandoc.table( Dens_DF[1:6,], digits=3 )
  
  
  
  ## Index of abundance
  Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )
  pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 
  
  
  
  
}