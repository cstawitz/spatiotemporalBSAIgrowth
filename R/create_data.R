#create data - deprecated
create_data <- function(dataset__,species, sex, age, renames){
  subsetted <- filter(dataset__, SPECIES_CODE==species,
                      Sex==sex, AGE==age)
  #Format data

  
  return(Data_Geostat)
}