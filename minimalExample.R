require(TMB)
require(VAST)
load("function_arrowtooth.RData")

TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig,
                  "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                  "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1, 
                  "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = rep(0,nrow(Data_Geostat)),
                  "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                  "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, X_xtp=X_xtp)



TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                       "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize(obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl"))
