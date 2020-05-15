library(jsonlite)
StatesDaily <- "https://api.covid19india.org/states_daily.json"
StatesDaily <- fromJSON(StatesDaily)
StatesDaily <- as.data.frame(StatesDaily)

#DistrictsDaily <- "https://api.covid19india.org/districts_daily.json"
#DistrictsDaily <- fromJSON(DistrictsDaily)

TestsDaily <- "https://api.covid19india.org/state_test_data.json"
TestsDaily <- fromJSON(TestsDaily)
TestsDaily <- as.data.frame(TestsDaily)

#TimeSeries <- "https://api.covid19india.org/data.json"
#TimeSeries <- fromJSON(TimeSeries)
#TodayData_Cases<-TimeSeries$statewise

library(reshape2)
StatesDaily<-melt(StatesDaily,id.vars = c("states_daily.status","states_daily.date"))
StatesDaily<-dcast(StatesDaily,states_daily.date + variable ~ states_daily.status )
StatesDaily$states_daily.date<-as.Date(as.character(StatesDaily$states_daily.date),format = "%d-%b-%y")
StatesDaily<-StatesDaily[with(StatesDaily, order(variable, states_daily.date)), ]
StatesDaily[,c("Confirmed","Deceased","Recovered")] <- sapply(StatesDaily[,c("Confirmed","Deceased","Recovered")],as.numeric)
StatesDaily$Confirmed_Cumsum <- ave(StatesDaily$Confirmed, StatesDaily$variable, FUN=cumsum)
StatesDaily$Deceased_Cumsum <- ave(StatesDaily$Deceased, StatesDaily$variable, FUN=cumsum)
StatesDaily$Recovered_Cumsum <- ave(StatesDaily$Recovered, StatesDaily$variable, FUN=cumsum)

StatesDaily$variable<-as.character(StatesDaily$variable)
library(stringr)
StatesDaily$statecode<-substring(StatesDaily$variable,nchar((StatesDaily$variable))-1,nchar((StatesDaily$variable)))
StatesDaily$statecode<-toupper(StatesDaily$statecode)

library(readxl)
LatLongMapper <- read_excel("LatLongMapper.xlsx")
StatesDaily<-merge(StatesDaily,LatLongMapper,by.x="statecode",by.y="statecode",all.x=TRUE)

StatesDaily$start<-StatesDaily$states_daily.date
StatesDaily$end<-StatesDaily$states_daily.date
StatesDaily$radius<-(StatesDaily$Confirmed_Cumsum)/1000
States_Daily_Geo <- geojsonio::geojson_json(StatesDaily,lat="Lat",lon="Long")


#library(sf)
#StatesDaily$Count<-1:nrow(StatesDaily)
#TempOut<-data.frame()
#for(i in 1:nrow(StatesDaily)){
#Temp<-st_sf(data.frame(a=i,geom=st_sfc(st_point(c(StatesDaily$Long[i],StatesDaily$Lat[i])))))
#Temp<-as.data.frame(Temp)
#TempOut<-rbind(TempOut,Temp)
#}

#StatesDaily<-merge(StatesDaily,TempOut,by.x="Count",by.y="a",all.x=TRUE)

#StatesDaily_Today<-subset(StatesDaily,StatesDaily$states_daily.date==Sys.Date()-1)
#StatesDaily_MH<-subset(StatesDaily,StatesDaily$statecode=="MH")
#TodayData_Cases<-merge(TodayData_Cases,LatLongMapper[,c(1,3,4)],by.x="state",by.y="state",all.x=TRUE)
#TodayData_Cases[,c(2,3,4,5,6,7,9)] <- sapply(TodayData_Cases[,c(2,3,4,5,6,7,9)],as.numeric)
#TodayData_Cases$content<-paste(paste("Confirmed: ",TodayData_Cases$confirmed,sep=""),
#paste("Deaths: ",TodayData_Cases$deaths,sep=""),
#paste("Active: ",TodayData_Cases$active,sep=""),sep="\n")

library(leaflet)
library(rgdal)
states <- readOGR("../IND_adm1.shp")
popup <- paste0("<strong>Name: </strong>", 
                states$NAME_1)

m<-leaflet(States_Daily_Geo, elementId = "leaflet-wide-timeline") %>%
  addTiles() %>%
  setView(78.435977, 23.204480, zoom = 5) %>%
  addTimeline(
    width = "96%"
  ) %>%  addPolygons(data=states, weight = 2, fillColor = "yellow")


leaflet(States_Daily_Geo) %>%
  addTiles() %>%
  setView(78.435977, 23.204480, zoom = 5) %>%
  addTimeline(width = "96%",
    timelineOpts = timelineOptions(
      styleOptions = NULL, # make sure default style does not override
      pointToLayer = htmlwidgets::JS(
        "
function(data, latlng) {
  return L.circleMarker(
    latlng,
    {
      radius: +data.properties.radius,
      color: data.properties.color,
      fillColor: data.properties.color,
      fillOpacity: 1
    }
  );
}
"
      )
    )
  ) %>%  addPolygons(data=states, weight = 2, fillColor = "yellow")


