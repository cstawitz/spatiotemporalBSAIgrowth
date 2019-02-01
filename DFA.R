library(MARSS)


cold_pool<-read.csv("./data/cpa_areas2018.csv")


mm <- 3
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
## 'QQ' is identity
QQ <- "identity"  # diag(mm)


Z_vals <- list("z11", 0, 0, "z21", "z22", 0, "z31", "z32", "z33", 
               "z41", "z42", "z43", "z51", "z52", "z53", "z61", "z62", "z63")
N_ts = 6
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 3, byrow = TRUE)

## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and equal"

## list with specifications for model vectors/matrices
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa, D = DD, d = dd, R = RR,
                 tinitx=1)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 5000, allow.degen = TRUE)
require(MARSS)
load("pred_length.RData")
dfa_3 <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
               control = con_list)

mm <- 1
Z_vals <- list("z11", "z21", "z31","z41",  "z51",  "z61")
RR <- "diagonal and equal"
ZZ <- matrix(Z_vals, nrow = N_ts, ncol =mm, byrow = TRUE)

init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa, D = DD, d = dd, R = RR,
                 tinitx=1)
dfa_1_equal <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
                     control = con_list)
mod_list <- list(B = BB,  Q = QQ, 
                 A = aa, D = DD, R = "diagonal and unequal",
                 tinitx=1)
dfa_1_equal_cpa <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
                         control = con_list, form="dfa",
                         covariates=t(scale(cold_pool$AREA_SUM_LTE2)))

dfa_1_equal_meant <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
                           control = con_list, form="dfa",
                           covariates=t(scale(cold_pool$TEMP)))

## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))


RR <- "diagonal and unequal"
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa, D = DD, d = dd, R = RR,
                 tinitx=1)
dfa_1 <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
               control = con_list)


RR <- "diagonal and equal"
mm <- 2
Z_vals <- list("z11", "z12","z21","z22", "z31","z22","z41", "z42", "z51", "z52", "z61", "z62")
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
ZZ <- matrix(Z_vals, nrow = N_ts, ncol =mm, byrow = TRUE)
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa, D = DD, d = dd, R = RR,
                 tinitx=1)
dfa_2 <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
               control = con_list)

mm <- 4
Z_vals <- list("z11", 0, 0,0, "z21", "z22", 0,0, "z31", "z32", "z33","z34", 
               "z41", "z42", "z43", "z44","z51", "z52", "z53", "z54","z61", "z62", "z63", "z64")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol =mm, byrow = TRUE)
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa, D = DD, d = dd, R = RR,
                 tinitx=1)
dfa_4 <- MARSS(y = pred_length, model = mod_list, inits = init_list, 
               control = con_list)

mod.list

AICCvals<-c(dfa_1$AICc, dfa_1_equal$AICc,dfa_1_equal_cpa$AICc, dfa_1_equal_meant$AICc, dfa_2$AICc, dfa_3$AICc, dfa_4$AICc)

save(dfa_1_equal, file="BestDFAObj.RData")
save(AICCvals, file="DFAAICc.RData")
