#'@name build_corrected_df
#'@description Function to call the get_unbiased_lengths function for each species of interest. This will extract the corrected length-at-age data and assign a unique primary key containing the station ID and year. This is EBS specific: TODO: anonymous column names 
#'@param dataset__ a data.frame containing age-at-length data separated by year and station sampled from
#'@param age - character for which age you'd like to analyze
#'@param species_code - species code
#'@param sex - code for fish sex
#'@param renames - vector of renames 
build_corrected_df <- function(dataset__, age, species_code, sex, renames){
  species_data <- filter(dataset__, SPECIES_CODE==species_code, 
                         Sex==sex)
  corrected_lengths <- get_unbiased_lengths(species_data, 
                      age.name="AGE", length.name = "LENGTH..cm.",
                      year.name = "YEAR", 
                      lat.name = "START_LATITUDE",
                      lon.name = "START_LONGITUDE")
  corrected_age<- data.frame(cbind(row.names(corrected_lengths),select(corrected_lengths, age)))
  names(corrected_age) <- c("ID", "lengths")
  corrected_age <- filter(corrected_age, lengths>0)
  species_age = species_data %>% filter(AGE==age) %>%
    mutate("START_LATITUDE" = round(START_LATITUDE,2), "START_LONGITUDE" = round(START_LONGITUDE,2)) 

  renames_set =  data_process(species_age, renames, 
                              id.vars=c("Year", "Lat","Lon"), response="length", 
                                               null.values=c("Vessel", "AreaSwept_km2"),
                                               YEAR,START_LATITUDE, START_LONGITUDE, LENGTH..cm., GEAR_DEPTH, GEAR_TEMPERATURE)
  
  Data_Geo<- left_join(renames_set, corrected_age, by="ID") %>% select(Year, Lat, Lon, AreaSwept_km2,Vessel, depth, temp, Catch_KG= lengths)
  return(Data_Geo)
}