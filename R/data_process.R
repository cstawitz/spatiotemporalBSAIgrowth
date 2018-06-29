#'@author Christine Stawitz
#'@description Function to take a 
#'of species-specific dataset and filter
#'out non-useful variables, aggregate datasets, and return data in a form acceptable to VAST.
#'@param dataset__ = data.frame containing raw data
#'@param age = which age of the data to analyse
#'@param renames = list of new names
#'@return a tibble that can be input into VAST
data_process<- function(dataset__, age, orig.names, renames){
  #Group by a set of pertinent variables
  subsetted <- dataset__ %>% filter(agecol == age) %>%
    mutate("ID_Age"=paste(orig.names[,grep("long")],
                          orig.names[,grep("lat")], 
                          orig.names[,grep("species")],
                          sep="_")) %>%
    rename(orig.names[,grep("len")]=LENGTH_cm,
           orig.names[,grep("swept")]=AREA_SWEPT_km_2,
          orig.names[,grep("weight")]=WEIGHT_g)
  
    DF = subsetted %>% select('YEAR','START_LATITUDE',
                                'START_LONGITUDE','LENGTH_cm',
                                'AGE', 'AREA_SWEPT_km_2', 'SPECIES_CODE') %>%
      split(.$ID_Age) %>%
      map_dfr(~mean(.x))
  return(subsetted)
  
}