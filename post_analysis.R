require(here)
require(colorspace)

cold_pool<-read.csv("./data/cpa_areas2018.csv")
par(mfrow=c(1,1))
plot(AREA_SUM_LTE2~YEAR, data=cold_pool, type="l")
plot(TEMP~YEAR, data=cold_pool, type="l")

cor_mat <- matrix(nrow=6, ncol=6)
get_cor<- function(x,y) return(cor(pred_length[x,],pred_length[y,], use="complete.obs"))
cor_mat[1,2] <- get_cor(1,2)
cor_mat[1,3] <- get_cor(1,3)
cor_mat[1,4] <- get_cor(1,4)
cor_mat[1,5] <- get_cor(1,5)
cor_mat[1,6] <- get_cor(1,6)
cor_mat[2,3] <- get_cor(2,3)
cor_mat[2,4] <- get_cor(2,4)
cor_mat[2,5] <- get_cor(2,5)
cor_mat[2,6] <- get_cor(2,6)
cor_mat[3,4] <- get_cor(3,4)
cor_mat[3,5] <- get_cor(3,5)
cor_mat[3,6] <- get_cor(3,6)
cor_mat[4,5] <- get_cor(4,5)
cor_mat[4,6] <- get_cor(4,6)
cor_mat[5,6] <- get_cor(5,6)

results_tx<-rbind(unname(ts[[1]]),
                  unname(ts[[2]]),
                  unname(ts[[3]]),
                  unname(ts[[4]]),
                  unname(ts[[5]]),
                  unname(ts[[6]]))




MARSSaic(dfa_1_equal, output= c("AICbp"),
         Options = list(nboot = 1000, return.logL.star = FALSE, 
                        silent = FALSE))
dfa_1$AICc
dfa_1_equal$AICc
dfa_2$AICc
dfa_3$AICc
dfa_4$AICc



get_DFA_fits(dfa_1_equal)


require(broom)
require(ggplot2)
require(plyr)

folders <- list.dirs(".", recursive=FALSE)
out_folders <- folders[grep("poisson",folders)]
outfiles <- paste0(out_folders,"/Save.RData")
get_aic_tables(outfiles)

get_aic_tables(filedirs)

dev.off()
png("pollockvyellowfin.png")
plot(results_tx[2,]~years, type="l", ylim=c(3,4.1))
lines(results_tx[3,]~years, col="blue")
dev.off()


