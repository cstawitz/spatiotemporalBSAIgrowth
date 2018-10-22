#Config VAST
#Header
require(dplyr)
# devtools command to get TMB from GitHub
require(devtools)
install_github("kaskr/adcomp/TMB") 
# source script to get INLA from the web
source("http://www.math.ntnu.no/inla/givemeINLA.R")  
library(TMB)
devtools::install_github("james-thorson/VAST", ref="development", dep=FALSE)
devtools::install_github("james-thorson/FishStatsUtils")
devtools::install_github("ropensci/drake")
withr::with_libpaths(new = "C:/Users/Christine.Stawitz/R_LIBS", install_github("cstawitz/VAST"))
devtools::load_all("C:/Users/chris/Documents/VAST")