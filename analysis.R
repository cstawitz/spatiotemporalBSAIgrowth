raw_data <- read.csv("./data/EBSLengths.csv")
require(dplyr)

pollock <- raw_data %>% filter(SPECIES_CODE==21740) %>% filter(Sex==2) %>% filter(AGE==7)
renames <- c('Year', 'station',
                  'Lat','Lon','Area_Swept_km2',
                  'length')

data_process(pollock, renames, id.vars=c("station", "Year"), response="length",
             YEAR, STATIONID, START_LATITUDE, START_LONGITUDE, AREA_SWEPT..km.2., LENGTH..cm.)
