get_aic_tables <- function(dirs){
  
  omega_var <- spatial_var <- sigma_mat <- kappa <-
    gamma <- AIC <- maxgrad <- rep(0,length(dirs))
  for(i in 1:length(dirs)){
    if(!file.exists(dirs[i])){print(paste("Could not find", dirs[i]))}
    load(dirs[i])
    omega_var[i]<- exp(Save$ParHat$L_omega2_z)
    spatial_var[i]<- exp(Save$ParHat$L_epsilon2_z)
    sigma_mat[i] <- exp(Save$ParHat$logSigmaM[1,1])
    kappa[i] <- exp(Save$ParHat$logkappa2)
    gamma[i] <- Save$ParHat$gamma2_ctp[,1,1]
    AIC[i] <- Save$Opt$AIC
    maxgrad[i] <- Save$Opt$max_gradient
  }
  return(data.frame(Lomega=omega_var,
                    Lepsilon=spatial_var,
                    SigmaM = sigma_mat,
                    kappa = kappa,
                    gamma = gamma, 
                    AIC = AIC,
                    maxgrad = maxgrad))
}
