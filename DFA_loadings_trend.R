load("BestDFAObj.RData")
load("pred_length.RData")

## get the estimated ZZ
mm<-1
N_ts<-6
tot_years <- 1982:2018
Z_est <- coef(dfa_1_equal, type = "matrix")$Z
colors_to_use <- growth_cols(6)
proc_rot = Z_est %*% dfa_1_equal$states
Z_rot = Z_est
clr <- c("brown", "blue", "darkgreen", "darkred", "purple", "pink")
ylbl <- pred_length
w_ts <- seq(dim(pred_length)[2])
layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))

par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(tot_years, proc_rot[i, ], type = "n", bty = "L", ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n", las=1)
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(tot_years, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext("A. State", side = 3, line = 0.5)
  axis(1, tot_years)
  mtext("Years", side=1, line=1.6)
}
## plot the loadings
minZ <- 0
ylbl = dfa_1_equal$coef[1:6]
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, 
                                                                    i]) > minZ, i]), type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col = colors_to_use, las=1)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, round(ylbl[j],2), srt = 90, adj = 1, cex = 1.2, 
           col = colors_to_use[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, round(ylbl[j],2), srt = 90, adj = 0, cex = 1.2, 
           col = colors_to_use[j])
    }
  }
  
  abline(h = 0, lwd = 1.5, col = "gray")
  mtext("B. Loadings", side = 3, line = 0.5)
}

