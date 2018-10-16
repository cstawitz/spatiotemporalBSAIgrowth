get_length_cv <- function(dataset__, name, length.name){
  filt.data <- group_by(dataset__,!!!rlang::syms(name)) %>%
    summarise("CV"=sd(!!!rlang::syms(length.name))/mean(!!!rlang::syms(length.name)))
  return(filt.data)
}