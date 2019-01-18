mean_l_list<-lapply(1:6, function(x) data_list[[x]] %>% group_by(Year) %>% dplyr::summarise(meanl=mean(Catch_KG)))

get_vect<-function(x){
  v <- rep(NA, 37)
  v[(tot_years %in% mean_l_list[[x]]$Year)]<-mean_l_list[[x]]$meanl
  return(v)
}
pred_length <- matrix(unlist(lapply(1:6,get_vect)),byrow=T, nrow=6, ncol=37)
for(i in 1:6){
  pred_length[i,] <- scale(pred_length[i,], center=TRUE, scale=TRUE)
}