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
  browser()
  bystation_yr<- dataset__ %>% filter(!!sym(station.name)!="") %>%
    mutate("ID"=paste(!!sym(year.name), !!sym(station.name), sep="_")) %>%
    group_by(ID, !!sym(age.name), "r.length" = round(!!sym(length.name),0)) %>%
    summarise("sample.size"=n(), "mean.l"=mean(!!sym(length.name)))


  get_each_station_yr <- function(station_yr_df){
    age_lengths <- array(dim=c(length(length_groups), length(ages)+1))
    browser()

  #Do math
  LengthSums <- station_yr_df %>% group_by(r.length) %>% summarise("l.sum"=sum(sample.size))
  Subsampled <- station_yr_df %>% filter(!is.na(!!sym(age.name))) %>% group_by(r.length) %>%
    summarise("l.sum"=sum(sample.size))
  
  
  for(i in 1:ages){
    tmp.dat <- filter(station_yr_df, !!sym(age.name)==i)
    lens <- tmp.dat$r.length
    Nj <- LengthSums %>% filter(r.length %in% lens) %>% select(l.sum)
    nj <- Subsampled %>% filter(r.length %in% lens) %>% select(l.sum)

    Nij <- Nj*(tmp.dat$sample.size/nj)
    

    Ni <- sum(tmp.dat$sample.size)
    #avgLength <- group_by(tmp.dat, r.length) %>%
    #mutate("avg"=mean(r.length))
    
    unbiased_l[i,] <- sum(Nij*avgLength[i,])/Ni[i]    
  }
  }
  
  
  x <- split(bystation_yr, bystation_yr$ID) %>%
    purrr::map(~get_each_station_yr(.x))
  
  
}
