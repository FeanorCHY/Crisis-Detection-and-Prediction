library(splitstackshape)

# events <- read.delim("~/Documents/Data Mining/final project/events.2005.tab")
#  event<-events

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
SplitDate<- function(event,MidECountr)
{
  event$Source.Country<- factor(event$Source.Country, levels=levels(MidECountr$country))
  event$Target.Country<- factor(event$Target.Country, levels=levels(MidECountr$country))
  event$Country<- factor(event$Target.Country, levels=levels(MidECountr$country))
  # eventx<-event[event$Country==MidECountr$country||event$Source.Country==MidECountr$country||event$Target.Country==MidECountr$country,]
  eventx<-event[event$Source.Country==MidECountr$country,]
  eventx2<-event[event$Target.Country==MidECountr$country,]
  eventx3<-event[event$Country==MidECountr$country,]
  eventx4<-rbind.data.frame(eventx,eventx2,eventx3)
  eventx4<-eventx4[!duplicated(eventx4$Event.ID),]
  eventx4<-concat.split.multiple(eventx4, "Event.Date", "-")
  eventx4
}
 MidECountr<-getMidEastCountry()



 # write.csv("file",fileEncoding = "UTF-8")
 
for(i in 2005:2015)
{  
  data.url = '~/Documents/Data Mining/final project/events.'
  event <- read.delim(paste(data.url,i,".tab",sep = ""))
  eventx<-SplitDate(event,MidECountr)
  for(j in 1:12)
  {
    tem<-eventx[eventx$Event.Date_2==j,]
    if(i==2005 && j==1){
      allevent<-tem;
      }
    else{
      allevent<-rbind.data.frame(allevent,tem);
    }
    # write.csv(tem,paste("eventdata_",i,"_",j,".csv",sep = ""),fileEncoding = "UTF-8")
  }
}
 
 