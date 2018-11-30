#'@name get_unbiased_lengths
#'@description A function that takes a dataset and an age-length sample size matrix and returns unbiased mean lenghts at age
#'@param dataset__ a dataset containing age and length data
#'@param age_lengths a 3 dimensional array matrix with the rows corresponding to the number of length categories,
#'@param l.bin the size of the length bins used to stratify  - 0 means 1 cm, -1 means 10 cm
#' columns corresponding to number of ages+1, and depth corresponding to years. The last column is the number of unaged fish.
#'@return a matrix of mean ages per year
get_unbiased_lengths <- function(dataset__, age.name, length.name, year.name, lat.name, lon.name, l.bin = 0){
  #initialize dimension scalars
  l <- round(select(dataset__,length.name),0)
  length_groups <- min(l):max(l)
  ages <- min(select(dataset__,age.name), na.rm=T):max(select(dataset__,age.name), na.rm=T)

  #aggregate data by station & year
  bystation_yr<- dataset__ %>% 
    mutate(lat = round(!!sym(lat.name),2), 
           lon=round(!!sym(lon.name),2)) %>%
    mutate("ID"=paste(!!sym(year.name), lat, lon, 
                      sep="_")) %>%
    group_by(ID, !!sym(age.name), "r.length" = round(!!sym(length.name),digits = l.bin), add=T) %>%
    summarise("sample.size"=n(), "mean.l"=mean(!!sym(length.name)))

#Function to get unbiased lengths for each station and year
  get_each_station_yr <- function(station_yr_df){
    size_at_age <- rep(0,length(ages))

    #Do mathpol.l
    #sample size of all length measurements in each 1cm length bin
    LengthSums <- station_yr_df %>% group_by(r.length) %>% summarise("l.sum"=sum(sample.size))
    #sample size of length measurements in each 1cm length bin for lengths of fish subsampled for ageing
    Subsampled <- station_yr_df %>% filter(!is.na(age.name)) %>% group_by(r.length) %>%
      summarise("l.sum"=sum(sample.size))
  
  
    for(i in 1:length(ages)){
      size_at_age[i] <- get_each_age(station_yr_df, ages[i],  age.name, LengthSums, Subsampled)
      #Each age data frame
     
    }
    return(size_at_age)
  }
  
  #Apply unbiased length calculation to each station:year stratum
  x <- split(bystation_yr, bystation_yr$ID) %>%
    purrr::map(~get_each_station_yr(.x)) %>%
    purrr::map_dfr(~as.data.frame(rbind(.)))
  
  #Add names
  row.names(x) <- unique(bystation_yr$ID)
  colnames(x) <- ages

  return(x)
}
