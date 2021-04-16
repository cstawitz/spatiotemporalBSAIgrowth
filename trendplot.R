#save(data_list, file="input_data.RData")
get_trend_plot<- function(bestmods, tot_years, spp.names){
  load("input_data.RData")

filedirs<-paste0("./",bestmods, "/Save.RData")
mapdatas <- paste0("./", bestmods,"/MapDetails.RData")
sd_vect <- paramest <- ts <- vector("list")
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

if(length(bestmods)==1){
  pred_length <- matrix(nrow=4, ncol=length(tot_years))
  pred_space <- matrix(nrow=4, ncol=100)
} else{
  
  pred_length <- matrix(nrow=length(bestmods), ncol=length(tot_years))
  pred_space <- matrix(nrow=length(bestmods), ncol=100)
}

for(i in 1:length(bestmods)){
  load(filedirs[i])
  Year_Set = seq(min(data_list[[i]][,'Year']),max(data_list[[i]][,'Year']))
  Years2Include = which( Year_Set %in% sort(unique(data_list[[i]][,'Year'])))
  new_mat <- Save$Report$D_xcy
  nspec <- dim(new_mat)[2]
  for(j in 1:(dim(new_mat)[3])){
    #weight by area
    if(dim(new_mat)[2]==1){
    new_mat[,,j] <- (Save$Report$D_xcy[,,j])
    } else{
      for(i in 1:nspec)
        new_mat[,i,j] <- Save$Report$D_xcy[,i,j]
    } 
  }
  if(nspec>1){
    total_d <- apply(new_mat,c(2,3),mean)
    pred_length <- total_d/max(total_d)
    for(j in 1:nrow(pred_length)){
      pred_length[j,] <- scale(pred_length[j,],
                               scale = TRUE,
                               center = TRUE)
    }
    
  } else{
    total_d <- apply(new_mat,3,mean)
    ts[[i]] <- total_d/max(total_d)
    ind <- which(tot_years %in% Year_Set[Years2Include])
    pred_length[i,ind] <- total_d[Years2Include]
    notind <- which(!tot_years %in% Year_Set[Years2Include])
    pred_length[i,notind] <- rep(NA,length(notind))
    pred_length[i,] <- scale(pred_length[i,], center=TRUE, scale=TRUE)
  }
  
  #spatial_d <- apply(Save$TmbData$D_xcy,1,mean)
  #pred_space[i,] <- spatial_d/max(spatial_d)
  
}

save(pred_length,file="pred_length.RData")
colors_to_use<-growth_cols(6)
#png("Figures/MatPlot_DFA.png", res=300)
par(mfrow=c(1,1), xpd=T, oma=c(1,1,1,1), mar=c(2,2,1,1))
matplot(y=t(pred_length),x=tot_years, type="l",
        xlab="Years", ylab="Scaled length", 
        las=1, col=colors_to_use, lty=1,
        lwd=2, bty="n")
legend(x=min(tot_years), y=3.2,legend=spp.names, 
       col=colors_to_use, pch=15, ncol=1, cex=0.8,bty="n", y.intersp=0.7)
mtext("Year",1, line=2)
mtext("Scaled length", 2, line=2)
#dev.off()
}
