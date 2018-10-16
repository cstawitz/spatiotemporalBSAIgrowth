#create data
create_data <- function(dataset__,species, sex, age, renames){
  subsetted <- filter(dataset__, SPECIES_CODE==species,
                      Sex==sex, AGE==age)
  #Format data
  Data_Geostat <- data_process(subsetted, renames, id.vars=c("Year", "station"), response="length", 
                               null.values=c("Vessel", "AreaSwept_km2"),
                               YEAR, STATIONID, START_LATITUDE, START_LONGITUDE, LENGTH..cm.)
  return(Data_Geostat)
}