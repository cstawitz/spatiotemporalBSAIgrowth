old_wd <- getwd()
setwd("./two_temp_3s_4st_factor")
best_out<- get(load("Save.RData"))
mapdetails <- get(load("MapDetails.RData"))

par(mai = c(1, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0), mfcol=c(2,2))


tot_years <- 1982:2018

factors <- get(load("BestModFactors.RData"))


plot(factors$Rotated_factors$Beta2[,1,]~tot_years, type="l", ylab = "Factor", xlab = "Year", las=1)

plot(factors$Rotated_factors$Beta2[,2,]~tot_years, type="l",  ylab = "Factor", xlab = "Year", las=1)

L_pj <- factors$Rotated_loadings$Beta2

clr <- growth_cols(6)[c(1,2,3,6)]
for(i in 1:2){
  lim1 <-  range(L_pj)
  Ylim = c(lim1[1]-.001,lim1[2]+.001)
  
  plot(1, type="n", xlim=c(0.5,nrow(L_pj)+0.5), ylim=Ylim, xlab="", ylab="", xaxt="n", xaxs="i",las=1)
  mtext( text=paste("Factor",i), side=3, line=0.1, adj=0)
  abline(h=0)
  Labels <- c("walleye pollock","Pacific cod", "yellowfin sole", "flathead sole")
  
  axis(labels = Labels, at=c(1:4), side=1, las=2)
  # Loop through categories and plot each
  for(p in 1:nrow(L_pj)){
    lines(y=c(0,L_pj[p,i]), x=rep(p,2), lwd=5, col=clr[p])
  }
  legend( "top", legend=paste0(round(100*sum(L_pj[,i]^2)/sum(L_pj^2),1),"%"), bty="n")
}
setwd(old_wd)