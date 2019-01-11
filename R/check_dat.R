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
check_dat <- function(clean_data, sp_code, spp.col, length.col, age.col, year.col, weight.col, sex.col, sex){
  #Get enquoted objects
  quo_sp <- rlang::enexpr(spp.col)
  quo_ln <- enquo(length.col)
  quo_age <- enquo(age.col)
  quo_yr <- enquo(year.col)
  quo_wt <- enquo(weight.col)
  quo_sex <- enquo(sex.col)
  
  #filter to species-specific data
  for_spec<-filter(clean_data, !! quo_sp==sp_code) %>%
    filter(!! quo_sex=="F")
  
  #Plot of length-at-age
  
  p <- ggplot(for_spec, aes(y = !! quo_ln, 
                       x= !! quo_age, 
                       colour = !! quo_yr)) +
    geom_point() + theme_classic()
  p
  ggsave(file=paste0("lengthage",sp_code,sex,".png"))
  
  #Plot of weight-at-length
  
  p <- ggplot(for_spec, aes(y = !! quo_wt, 
                       x= !! quo_ln, 
                       colour = !! quo_yr)) +
    geom_point() + theme_classic()
  p
  ggsave(paste0("weightlength",sp_code,sex,".png"))
  
  weight_lm <- lm(log(weight_kg)~log(length_cm), 
                  data=for_spec)
  ind <- which(!is.na(for_spec$weight_kg))
  lens <- slice(for_spec,ind)$length_cm
  pars <- weight_lm$coefficients
  pwt <- exp(pars[1])*lens^pars[2]
  predicted_df <- data.frame(len=lens ,
                             predwt = pwt)
  p + geom_line(color='pink',
                data = predicted_df, 
                aes(x=len, y=predwt))
  ggsave(paste0("weightlength",sp_code,sex,"line.png"))

  
  #Aggregate by age and year
  summary <- for_spec %>%
    group_by(!! quo_age, !! quo_yr) %>%
    summarise(mean_l = mean(!! quo_ln), 
              mean_w = mean(!! quo_wt))

}