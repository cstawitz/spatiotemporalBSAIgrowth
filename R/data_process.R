#'@author Christine Stawitz
#'@description Function to take a 
#'of species-specific dataset and filter
#'out non-useful variables, aggregate datasets, and return data in a form acceptable to VAST.
#'@param dataset__ = data.frame containing raw data
#'@param renames = list of new names
#'@param id.vars = names of columns to construct a unique ID for
#'@param response = name of the column that contains your response variable (i.e. length, catch)
#'@return a tibble that can be input into VAST
data_process<- function(dataset__,  renames, id.vars, response, ...){
  #Subset to only useful columns and rename them
  qs <- rlang::quos(...)
  subsetted <- select(dataset__,rlang::UQS(qs))
  names(subsetted) <- renames
  
  #Filter selected age and get ID columns
  munged <- subsetted %>%
    mutate(ID=paste(!!!rlang::syms(id.vars),sep="_"))
  
  #Group by unique ID and calculate average length and weight
  DF <- munged %>%
    group_by(ID) %>%
    mutate(Catch_KG=mean(!!!rlang::sym(response)), Vessel=0) %>%
    select(-response) %>%
    distinct() %>%
    ungroup()
  return(DF)
  
}

