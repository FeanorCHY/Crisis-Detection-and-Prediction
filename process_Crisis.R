library(dplyr)
crisis <- read.csv("~/Documents/Data Mining/final project/crisis.csv")
y=matrix(0,112,8)
# for(i in 2005:2014)
# {
#   for(j in 1:12)
#   {
#     if(!(i==2014&&j>5)){
#       subs<-filter(crisis, year == i, month == j)
#       if(((i-2005)*12+j-1)!=0)
#         y[(i-2005)*12+j-1,1]=sum(subs$ins,subs$reb,subs$dpc,subs$erv,subs$ic)
#     }
#   }
# }
k=1
predx<-matrix(0,113,8)
for(i in 2005:2014)
{
  for(j in 1:12)
  {
    if(!(i==2014&&j>5)){
      subs<-filter(crisis, year == i, month == j)
      if(((i-2005)*12+j-k)!=0){
        y[(i-2005)*12+j-k,1]=sum(subs$ins,subs$reb,subs$dpc,subs$erv,subs$ic)
        y[(i-2005)*12+j-k,2]=sum(subs$ins)
        y[(i-2005)*12+j-k,3]=sum(subs$reb)
        y[(i-2005)*12+j-k,4]=sum(subs$dpc)
        y[(i-2005)*12+j-k,5]=sum(subs$erv)
        y[(i-2005)*12+j-k,6]=sum(subs$ic)
        y[(i-2005)*12+j-k,7]=i
        y[(i-2005)*12+j-k,8]=j
      }
      predx[(i-2005)*12+j,1]=sum(subs$ins,subs$reb,subs$dpc,subs$erv,subs$ic)
      predx[(i-2005)*12+j,2]=sum(subs$ins)
      predx[(i-2005)*12+j,3]=sum(subs$reb)
      predx[(i-2005)*12+j,4]=sum(subs$dpc)
      predx[(i-2005)*12+j,5]=sum(subs$erv)
      predx[(i-2005)*12+j,6]=sum(subs$ic)
      predx[(i-2005)*12+j,7]=i
      predx[(i-2005)*12+j,8]=j
    }
  }
}
for(i in 1:nrow(y))
{
  if(y[i,1]<10)
    y[i,1]<-0
  else
    y[i,1]<-1
}
sum(y[,1])
for(i in 1:nrow(y))
{
  if(y[i,2]<3)
    y[i,2]<-0
  else
    y[i,2]<-1
}
for(i in 1:nrow(y))
{
  if(y[i,3]<2)
    y[i,3]<-0
  else
    y[i,3]<-1
}
for(i in 1:nrow(y))
{
  if(y[i,4]<2)
    y[i,4]<-0
  else
    y[i,4]<-1
}
for(i in 1:nrow(y))
{
  if(y[i,5]<2)
    y[i,5]<-0
  else
    y[i,5]<-1
}
sum(y[,5])
for(i in 1:nrow(y))
{
  if(y[i,6]<3)
    y[i,6]<-0
  else
    y[i,6]<-1
}
sum(y[,6])
# y=matrix(0,112,1)
# y<-crisis$ins[2:nrow(crisis)]
