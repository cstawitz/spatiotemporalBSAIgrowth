get_length_weight <- function(dataset__, name){
  n.each <- select(dataset__,!!!rlang::syms(name)) 
  return(apply(is.na(n.each),2,sum))
}