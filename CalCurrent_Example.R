library(VAST)
library(compiler)
library(dplyr)
library(here)
setwd(here())

Version = "VAST_v7_0_0"

name <- load("./data/CC_Oct2018.RData")
CC <- get(name)

check_dat(clean_data = CC, "Eopsetta jordani", scientific_name, length_cm, age_years, weight_kg)

