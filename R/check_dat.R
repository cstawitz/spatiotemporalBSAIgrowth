#'@name check_dat
#'@description A function that creates a number of exploratory data plots 
#'@author Christine Stawitz
#'@param clean_data - a species-aggregated dataset
#'@param sp_code - unique identifier to filter each species/stock
#'@param spp.col - the column of your dataset corresponding to species/stock
#'@param length.col - column of the dataset corresponding to length
#'@param age.col - column of the dataset corresponding to age
#'@param year.col -column of the dataset corresponding to year
#'@param weight.col = column of the dataset corresponding to weight
check_dat <- function(clean_data, sp_code, spp.col, length.col, age.col, year.col, weight.col){
  #Get enquoted objects
  quo_sp <- rlang::enexpr(spp.col)
  quo_ln <- enquo(length.col)
  quo_age <- enquo(age.col)
  quo_yr <- enquo(year.col)
  quo_wt <- enquo(weight.col)
  
  #filter to species-specific data
  for_spec<-filter(clean_data, !! quo_sp==sp_code)
  
  #Plot of length-at-age
  png(paste0("lengthage",sp_code,".png"))
  p <- ggplot(for_spec, aes(y = !! quo_ln, 
                       x= !! quo_age, 
                       colour = !! quo_yr)) +
    geom_point() + theme_classic()
  p
  dev.off()
  
  #Plot of weight-at-length
  png(paste0("weightlength",sp_code,".png"))
  p <- ggplot(for_spec, aes(y = !! quo_wt, 
                       x= !! quo_ln, 
                       colour = !! quo_yr)) +
    geom_point() + theme_classic()
  p
  dev.off()
  
  #Aggregate by age and year
  summary -> for_spec %>%
    group_by(!! quo_age, !! quo_yr) %>%
    summarise(mean_l = mean(!! quo_ln), 
              mean_w = mean(!! quo_wt))

}