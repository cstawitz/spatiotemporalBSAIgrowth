#'@author Christine Stawitz
#'@return table with details of all fit models

read_results <- function(csv_path){
  results_table <- read.csv(csv_path)
  get_outputs <- function(x){
    save_out <- get(load(paste0("./",
                                x,
                                "/Save.Rdata")))
    obj_out <- save_out[["Opt"]]
    AIC <- obj_out[["AIC"]][1]
    par <- obj_out[["par"]]
    names(par) <- make.names(names(par), 
                             unique=TRUE)
    
    max_grad <- obj_out[["max_gradient"]]
    return_vector <- c(AIC, par, max_grad)
    names(return_vector) <- c("AIC", names(par),"max_gradient")
    return(return_vector)
  }

  #Get outputs we need from each model
  out_list <- purrr::map(as.character(results_table$Folder), get_outputs) 
  

  out_df <- bind_rows(out_list[[1]],out_list[[2]])
  for(i in 3:6){
  out_df <- bind_rows(out_df,out_list[[i]]) 
  }
  
  meld_df <- cbind(results_table,out_df)
  return(meld_df)
}
