#'@author Christine Stawitz
#'@description Function to take a 
#'of species-specific dataset and filter
#'down to only columns of interest, average across a unique set of id.vars, generate null values for empty values, 
#'and return data in a form acceptable to VAST.
#'@param dataset__ = data.frame containing raw data
#'@param renames = character vector of new names to rename the columns specified in ...
#'@param id.vars = character vector with names of columns to use to construct a unique ID
#'@param response = name of the column that contains your response variable (i.e. length, catch)
#'@param null.values = names of columns that VAST wants that don't have values in your dataset
#'@param ... = the columns that correspond to renames
#'@return clean_data = a tibble that can be input into VAST
data_process<- function(dataset__,  renames, id.vars, response.var, null.values = NULL ,...){
  #Subset to only useful columns and rename them
  qs <- rlang::quos(...)
  subsetted <- select(dataset__,rlang::UQS(qs))
  names(subsetted) <- renames
  
  #Filter selected age and get ID columns
  clean_data <- subsetted %>%
    mutate(ID=paste(!!!rlang::syms(id.vars),sep="_")) %>%
    group_by(ID) %>%
    mutate(Catch_KG=mean(!!!rlang::sym(response.var))) %>%
    select(-response.var) %>%
    distinct() %>%
    ungroup()
  
  clean_data[,null.values] <- 0
  
  return(clean_data)
}

