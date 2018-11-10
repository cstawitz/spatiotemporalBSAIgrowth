#'@author Christine Stawitz
#'@description Function to take a 
#'of species-specific dataset and filter
#'down to only columns of interest, average across a unique set of id.vars, generate null values for empty values, 
#'and return data in a form acceptable to VAST.
#'@param dataset__ = data.frame containing raw data
#'@param renames = character vector of new names to rename the columns specified in ...
#'@param id.vars = character vector with names of columns to use to construct a unique ID
#'@param response = name of the column that contains your response variable (i.e. length, catch)
#'@param null.values = names of columns to create dummy values for
#'@param ... = the columns that correspond to renames
#'@return clean_data = a tibble 
#'@examples
#' \dontrun{ 
#' data <- read.csv(data/EBSLengths.csv, header=T)
#' subsetted <- filter(data SPECIES_CODE==species, Sex==sex, AGE==age)
#'  #Format data
#'  renames <- renames <- c('Year', 'station', 'Lat','Lon','AreaSwept_km2','length')
#' Data_Geostat <- data_process(subsetted, renames, id.vars=c("station", "Year"), response.var="length", 
#'                             null.values="Vessel",
#'                             YEAR, STATIONID, START_LATITUDE, START_LONGITUDE, AREA_SWEPT..km.2., LENGTH..cm.)
#' }
data_process<- function(dataset__,  renames, id.vars, response.var, null.values = NULL ,...){
  #Subset to only useful columns and rename them
  qs <- rlang::quos(...)
  subsetted <- select(dataset__,rlang::UQS(qs))
  names(subsetted) <- renames
  
  #Filter selected age and get ID columns
  clean_data <- subsetted %>%
    mutate(ID=paste(!!!rlang::syms(id.vars),sep="_")) %>%
    group_by(ID) %>%
    mutate(Catch_Kg=mean(!!!rlang::sym(response.var))) %>%
    select(-response.var) %>%
    distinct() %>%
    ungroup()
  
  clean_data[,null.values] <- 0
  
  return(as.data.frame(clean_data))
}

