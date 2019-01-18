sp.codes <- unique(clean_data$SPECIES_CODE)
for(i in 1:length(sp.codes)){
  sp.data <- clean_data %>% filter(SPECIES_CODE==sp.codes[i])
png(paste("Locs",sp.codes[i],".png"))
plot(START_LATITUDE~START_LONGITUDE, data=sp.data, xlim=c(-180,-150))
dev.off()
}

yrs <- unique(substr(rownames(pol.lengths),1,4))
row.index <- lapply(1:length(yrs), function(x) which(substr(rownames(pol.lengths),1,4)==yrs[x]))

plot(NA, xlim=as.numeric(c(min(yrs),max(yrs))), ylim=c(min(pol.lengths[,7:10], na.rm=T), max(pol.lengths[,7:10], na.rm=T)), 
     las=1, xlab="Years", ylab="Size (cm)")
lapply(7:10, function(y)
  lapply(1:length(yrs), function(x) points(pol.lengths[row.index[[x]],y]~rep(yrs[x],length(row.index[[x]])), col=y)))

sample.sizes <-purrr::map(list(pollock, cod, arth), get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))
sample.sizes.yr <- split(arth, arth$YEAR) %>% purrr::map(get_length_weight, name=c("LENGTH..cm.","WEIGHT..g.", "AGE"))


cv.length.age <- split(arrow, arrow$AGE) %>% purrr::map(get_length_cv, name=c("YEAR")) %>% unlist()
cv.length.age <- purrr::map(list(pollock, cod, arrow),
                            get_length_cv, name="AGE", length.name="LENGTH..cm.")
pdf("LengthAge.pdf")
for(i in 1:3){
  plot(CV~AGE, data=cv.length.age[[i]])
}
dev.off()