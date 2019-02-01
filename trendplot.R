#save(data_list, file="input_data.RData")
load("input_data.RData")
bestmods <- c("pcod_spatiotemp_poisson", "Pollock_Spatiotemp_Poisson", "yellowfin_spatio_poisson_depth", "nrock_spatiotemp_poisson_depth", "arrow_spatio_poisson_depth", "flathead_spatiotemp_poisson_temp")

filedirs<-paste0("./",bestmods, "/Save.RData")
mapdatas <- paste0("./", bestmods,"/MapDetails.RData")
sd_vect <- paramest <- ts <- vector("list")
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
tot_years <- 1982:2018
spp.names <- c("P. cod", "pollock",
               "yellowfin", "N. rock sole", "arrowtooth", "flathead")
pred_length <- matrix(nrow=6, ncol=length(tot_years))
pred_space <- matrix(nrow=6, ncol=100)
for(i in 1:6){
  load(filedirs[i])
  Year_Set = seq(min(data_list[[i]][,'Year']),max(data_list[[i]][,'Year']))
  Years2Include = which( Year_Set %in% sort(unique(data_list[[i]][,'Year'])))
  new_mat <- Save$Report$D_xcy
  for(j in 1:(dim(new_mat)[3])){
    #weight by area
    new_mat[,,j] <- t(Save$TmbData$a_xl)*(Save$Report$D_xcy[,,j])/sum(t(Save$TmbData$a_xl))
  }
  total_d <- apply(new_mat,3,mean)
  #spatial_d <- apply(Save$TmbData$D_xcy,1,mean)
  #pred_space[i,] <- spatial_d/max(spatial_d)
  ts[[i]] <- total_d/max(total_d)
  ind <- which(tot_years %in% Year_Set[Years2Include])
  pred_length[i,ind] <- total_d[Years2Include]
  notind <- which(!tot_years %in% Year_Set[Years2Include])
  pred_length[i,notind] <- rep(NA,length(notind))
  pred_length[i,] <- scale(pred_length[i,], center=TRUE, scale=TRUE)
}
save(pred_length,file="pred_length.RData")
colors_to_use<-growth_cols(6)

#png("Figures/MatPlot_Data.png")
par(mfrow=c(1,1), xpd=T, oma=c(1,1,1,1), mar=c(2,2,3,1))
matplot(y=t(pred_length),x=tot_years, type="l",
        xlab="Years", ylab="Scaled length", 
        las=1, col=colors_to_use, lty=1,
        lwd=2)
legend(x=1983, y=3.75,legend=spp.names, 
       col=colors_to_use, pch=15, ncol=3, cex=0.9)
mtext("Year",1, line=2)
mtext("Scaled length", 2, line=2)
#dev.off()