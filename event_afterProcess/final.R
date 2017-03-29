library(splitstackshape)

# events <- read.delim("~/Documents/Spring Semester/Data Mining/final project/events.2005.tab")
#  event<-events

getMidEastCountry<- function()
{
  event <- read.delim("~/Documents/Spring Semester/Data Mining/final project/events.2005.tab")
  eventx<-event[event$Latitude<40,]
  eventx<-eventx[eventx$Latitude>12,]
  eventx<-eventx[eventx$Longitude>29,]
  eventx<-eventx[eventx$Longitude<63,]
  country<-unique(event$Country[duplicated(event$Country)]) 
  country<-data.frame(country=country)
  country2<-unique(eventx$Country[duplicated(eventx$Country)]) 
  country2<-data.frame(country=country2)
  country2<-country2[-c(8,10,11,12,17,21,22,23,24,25,26,27),]
  country2<-data.frame(country=country2)
  country2
}
SplitDate<- function(event,country2)
{
  event$Source.Country<- factor(event$Source.Country, levels=levels(country2$country))
  event$Target.Country<- factor(event$Target.Country, levels=levels(country2$country))
  event$Country<- factor(event$Target.Country, levels=levels(country2$country))
  # eventx<-event[event$Country==country2$country||event$Source.Country==country2$country||event$Target.Country==country2$country,]
  eventx<-event[event$Source.Country==country2$country,]
  eventx2<-event[event$Target.Country==country2$country,]
  eventx3<-event[event$Country==country2$country,]
  eventx4<-rbind.data.frame(eventx,eventx2,eventx3)
  eventx4<-eventx4[!duplicated(eventx4$Event.ID),]
  eventx4<-concat.split.multiple(eventx4, "Event.Date", "-")
  eventx4
}
 country2<-getMidEastCountry()




for(i in 2005:2015)
{  
  data.url = '~/Documents/Spring Semester/Data Mining/final project/events.'
  event <- read.delim(paste(data.url,i,".tab",sep = ""))
  eventx<-SplitDate(event,country2)
  for(j in 1:12)
  {
    tem<-eventx[eventx$Event.Date_2==j,]
    write.csv(tem,paste("eventdata_",i,"_",j,".csv",sep = ""))
  }
}
  
  
  
  