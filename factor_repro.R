best_out<- get(load("Save.RData"))
mapdetails <- get(load("MapDetails.RData"))
factors <- FishStatsUtils::plot_factors(Report= best_out$Report, ParHat = best_out$ParHat, Data = best_out$TmbData, SD = best_out$Opt$SD, mapdetails_list = mapdetails,
                                        Year_Set = seq(1982,2018), category_names=c(0,1,2,3), plotdir="")
