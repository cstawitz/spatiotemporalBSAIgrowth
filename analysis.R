#Config VAST
#source("R/vast_config.R")


#Read in raw data
raw_data <- read.csv("./data/EBSLengths.csv")
renames <- c('Year', 'station',
             'Lat','Lon','Area_Swept_km2',
             'length')


data_plan <- drake_plan(
  pollock = create_data(raw_data,species=21740,
                         sex=2, age=7),
  pcod = create_data(raw_data, species=21720, sex=2, age=4),
  arrowtooth = create_data(raw_data, species=10110, sex=2, age=4)
)


methods <- drake_plan(
  
)

my_analyses <- plan_analyses(methods, data=data_plan)


