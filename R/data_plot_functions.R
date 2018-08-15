
mapsSetup<- function(df){
  df$posLongs <- df$START_LONGITUDE
  df$posLongs[df$START_LONGITUDE<0] <- ((df$START_LONGITUDE[df$START_LONGITUDE<0])+360)
  return(df)
}

doGrid <- function(df){
  longs <-c(min(df$posLongs), max(df$posLongs))
  lats <- c(min(df$START_LATITUDE), max(df$START_LATITUDE))
  grid<-makeGrid(x=seq(longs[1],longs[2],by=.4),y=seq(lats[1],lats[2],by=.4), projection="LL")
  return(grid)
}


makeMap<- function(df, textstr){
  datEvent <- data.frame(X=df$posLongs, Y=df$START_LATITUDE, EID = seq(1,nrow(df)),
                         Size=df$LENGTH..cm.)
  events <- as.EventData(x=datEvent, projection="LL")
  locData<-findCells(events,grid)
  events$Z <- events$Size
  pdata <- combineEvents(events,locData, FUN=median)
  clrs <- brewer.pal(9,"YlGn")
  brks <- seq(min(pdata$Z),max(pdata$Z),length.out=9)
  pdata <- makeProps(pdata, brks, "col", clrs)
  par(mar=c(2,2,2,2))
  plotMap(worldLLhigh, xlim=c(minLong,maxLong), ylim=c(minLat,maxLat))
  addPolys(grid, polyProps=pdata)
  legend("topleft", # position
         legend = as.character(round(brks,0)), 
         title = "Mean size",
         fill = clrs,
         cex = 0.56,
         bty = "n") 
  text(x=185, y=64,textstr)
}