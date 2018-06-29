#create data
create_data <- function(dataset__,species, sex, age, renames){
  subsetted <- filter(dataset__, SPECIES_CODE==species,
                      Sex==sex, AGE==age)
  #Format data
  Data_Geostat <- data_process(subsetted, renames, id.vars=c("station", "Year"), response="length", 
                               null.values=c("Vessel","foo"),
                               YEAR, STATIONID, START_LATITUDE, START_LONGITUDE, AREA_SWEPT..km.2., LENGTH..cm.)
  return(Data_Geostat)
}