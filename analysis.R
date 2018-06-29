raw_data <- read.csv("./data/EBSLengths.csv")
require(dplyr)
pollock <- raw_data %>% filter(SPECIES_CODE==21740) %>% filter(Sex==2) %>% filter(age==7)
require(dplyr)
renames <- c('year', 'station',
                  'lat','long','area_swept','age',
                  'length', 'weight')

data_process(pollock, renames, id.vars=c("age", "station", "year"),
             YEAR, STATIONID, START_LATITUDE, START_LONGITUDE, AREA_SWEPT..km.2., AGE, LENGTH..cm., WEIGHT..g.)
