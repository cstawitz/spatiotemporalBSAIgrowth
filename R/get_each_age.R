get_each_age <- function(station_yr_df, age, age.name, LengthSums, Subsampled){
  tmp.dat <- filter(station_yr_df, !!sym(age.name)==age)

if(nrow(tmp.dat)>0){
  #number of length classes with observations for each age
  lens <- tmp.dat$r.length
  #Sample size of all fish
  Nj <- LengthSums %>% filter(r.length %in% lens) %>% select(l.sum)
  #Sample size of all aged fish
  nj <- Subsampled %>% filter(r.length %in% lens) %>% select(l.sum)
  
  #Sample size of number of age i fish subsampled in the jth length group
  Nij <- Nj*(tmp.dat$sample.size/nj)
  #Sample size of all age i fish
  Ni <- sum(tmp.dat$sample.size)
  #Corrected mean length at age i
  size_at_age <- sum(Nij*tmp.dat$mean.l)/Ni
} else{
  size_at_age <- 0
}
  return(size_at_age)
}