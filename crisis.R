crisis<-read.csv("gtds_2001.to.may.2014.csv")
crisis <- read.csv("~/Documents/Data Mining/final project/gtds_2001.to.may.2014.csv")
getMidEastCountry<- function()
{
  event <- read.delim("~/Documents/Data Mining/final project/events.2005.tab")
  eventx<-event[event$Latitude<40,]
  eventx<-eventx[eventx$Latitude>12,]
  eventx<-eventx[eventx$Longitude>29,]
  eventx<-eventx[eventx$Longitude<63,]
  country<-unique(event$Country[duplicated(event$Country)]) 
  country<-data.frame(country=country)
  MidECountr<-unique(eventx$Country[duplicated(eventx$Country)]) 
  MidECountr<-data.frame(country=MidECountr)
  MidECountr<-MidECountr[-c(8,10,12,17,21,22,23,24,25,26,27),]
  MidECountr<-data.frame(country=MidECountr)
  MidECountr
}
 MidECountr<-getMidEastCountry()

crisis$country<- factor(crisis$country, levels=levels(MidECountr$country))
eventx<-crisis[crisis$country==MidECountr$country[1],]
for(i in 1:nrow(MidECountr))
{
  tem<-crisis[crisis$country==MidECountr$country[i],]
  eventx<-rbind.data.frame(tem,eventx)
}

tem<-eventx[eventx$year==c(2005:2015),]
eventx2<-crisis[crisis$country==MidECountr$country[1],]
for(i in 2005:2015)
{
  tem<-eventx[eventx$year==i,]
  eventx2<-rbind.data.frame(tem,eventx2)
}
write.csv(eventx2,"crisis.csv")
