#devtools::install_local("C:/Users/chris/Downloads/VAST-3.4.1.tar.gz")
require(VAST)
#require(here)
Version = get_latest_version()

#Drop 1977 from temperature data
#Repull data with 2017 ages
#Combine eureka columbia & vancouver - keep monterey and conception separate
#VBGF K relationship with temp for tim's models
# length at age 1 vs growth increment 0 - 1
#Where are young fish? Tim used lat/long to get the temp
#lag temp from previous year
# spatial random effect on k parameter - length at age be the
# change in size from age to next




#require(dplyr)
rawCC <- get(load("./data/Warehouse.All.Ages.Env, 23 Jul 2019.RData"))
require(dplyr)
spp <- unique(rawCC$scientific_name)

for(i in 1:length(spp)){
summ <- rawCC %>% group_by(scientific_name, age_years) %>%
  summarise(count= n()) %>% filter(scientific_name==spp[i])
write.csv(summ, paste("Datafor",spp[i],"age.csv"))
}

clean_CC <- rawCC %>% filter(!is.na(length_cm))

renames <- c('Year',
             'Lat','Lon',
             'length', 'depth', 'temp')
max_num<-vector("list")
age <- rep(0,29)
for(i in 1:length(spp)){
max_num[[i]]<- filter(clean_CC, scientific_name==spp[i], !is.na(age_years)) %>% group_by(age_years) %>% mutate(count=n()) %>%
  select(age_years, count)
age[i] <- max_num[[i]][which.max(max_num[[i]]$count),'age_years']
}

names(clean_CC)[c(1,4:7,11,12,17,20)] <- c("AGE",
                                           "GEAR_DEPTH",
                                           "START_LATITUDE","LENGTH..cm.","START_LONGITUDE","SPECIES_CODE","Sex","YEAR","GEAR_TEMPERATURE")



species_ <- c("Merluccius productus", "Ophiodon elongatus", "Sebastes jordani", "Eopsetta jordani",
              "Anoplopoma fimbria", "Sebastes crameri", "Citharichthys sordidus")

spp_ind<-which(spp %in% species_, arr.ind=TRUE)
ages <- rep(0,length(species_))

sp_name <- ages
for(i in 1:length(species_)){
  sp_name[i] <- which(spp==species_[i])
}

ages <- unlist(age)[sp_name]

i <- 5
devtools::load_all(".")
data_for_mod <- vector("list")
for(i in 1:length(species_)){
  data_for_mod[[i]] = build_corrected_df(clean_CC,species_code = species_[i],
                                         sex="F", age=ages[i], renames)
}
lapply(1:length(species_), function(x) nrow(data_for_mod[[x]])
save(data_for_mod, file="./data/CalCurrDataListOlder.Rmd")
