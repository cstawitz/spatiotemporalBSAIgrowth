library(TMB)
library(VAST)


#Dataset using three species which share positive loadings in other DFA
load("minReproCC.RData")

#Dataset using all four species, including flathead which has a negative loading on trend
#load("minRepro4spp.RData")



TmbData = make_data("Version"=Version, "FieldConfig"=FieldConfig,
                    "RhoConfig"=RhoConfig, "ObsModel"=ObsModel,
                    "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2']+1,
                    "s_i"=Data_Geostat[,'knot_i']-1, "c_iz" = Data_Geostat[,'spp'],
                    "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, spatial_list = Spatial_List,
                    "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, CheckForErrors = FALSE)

TmbList = make_model("TmbData"=TmbData, "Version"=Version, "RhoConfig"=RhoConfig,
                     "loc_x"=Spatial_List$loc_x )

Params = TmbList$Parameters
Params$beta1_ft = array(20, dim=dim(TmbList$Parameters$beta1_ft))

Map = TmbList$Map
Map$beta1_ft = factor( array(NA,dim=dim(TmbList$Parameters$beta1_ft)) )

TmbList = make_model("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig,
                     "loc_x"=Spatial_List$loc_x, "Method"=Method, "Parameters"=Params, "Map"=Map)

Obj = TmbList[["Obj"]]
Opt = TMBhelper::fit_tmb(obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=5, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl"), loopnum=5)
