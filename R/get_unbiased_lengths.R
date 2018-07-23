#'@name get_unbiased_lengths
#'@description A function that takes a dataset and an age-length sample size matrix and returns unbiased mean lenghts at age
#'@param dataset__ a dataset containing age and length data
#'@param age_lengths a 3 dimensional array matrix with the rows corresponding to the number of length categories,
#' columns corresponding to number of ages+1, and depth corresponding to years. The last column is the number of unaged fish.
#'@return a matrix of mean ages per year
get_unbiased_lengths <- function(dataset__, age.name, length.name, year.name, station.name){
  #initialize dimension scalars
  
  l <- round(select(dataset__,length.name),0)
  length_groups <- min(l):max(l)
  ages <- min(select(dataset__,age.name), na.rm=T):max(select(dataset__,age.name), na.rm=T)
  
  bystation_yr<- dataset__ %>% filter(!!sym(station.name)!="") %>%
    mutate("ID"=paste(!!sym(year.name), !!sym(station.name), sep="_")) %>%
    group_by(ID, !!sym(age.name), "r.length" = round(!!sym(length.name),0)) %>%
    summarise("sample.size"=n())


  get_each_station_yr <- function(station_yr_df){
    age_lengths <- array(dim=c(length(length_groups), length(ages)+1))
    browser()
    x <- station_yr_df %>% purrr::map(~ age_lengths[which(length_groups==.$r.length),which(ages==.$AGE)] <- .$sample.size)

  #Do math
  Nj <- apply(age_lengths,1,sum)
  nj <- apply(age_lengths[,-(ages+1)],1,sum)
  Ni <- apply(age_lengths,2,sum)
  
  for(i in 1:ages){
    Nij <- Nj*(age_lengths[i,]/nj)
    avgLength <- group_by(station_yr_df, !!!rlang::syms(column.names))
    mutate("avg"=mean())
    
    unbiased_l[i,] <- sum(Nij*avgLength[i,])/Ni[i]    
  }
  }
  
  
  x <- split(bystation_yr, bystation_yr$ID) %>%
    purrr::map(~get_each_station_yr(.x))
  
  
}