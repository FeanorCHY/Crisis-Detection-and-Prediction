grahSet <- read.csv("~/Documents/Data Mining/final project/grahSet.csv")
grahSet2<-grahSet
for(i in nrow(grahSet))
{
  grahSet2[i+1,]<-grahSet[i,]
}
test = scale(grahSet)
del=c()
for(i in 1:ncol(test))
{
  if(sum(test[,i])==0||is.nan(sum(test[,i])))
    del<-c(del,i)
}
x<-as.matrix(test[,del])
test<-test[,-del]
# sum(test[,3909])
data.pca = prcomp(test, scale=TRUE) 
data.pc = predict(data.pca)
plot(data.pca, main='screeplot for PCA on financial indicators')  #screeplot
# plot(data.pc[,1:2], type="n")
# text(x=data.pc[,1], y=data.pc[,2], labels=labels)    #scatterplot of two largest priciple component
length(data.pca$sdev^2 / sum(data.pca$sdev^2))

grahSet<-data.pc[,1:100]


# library(igraph)
# library(Cairo)
# 
# g9<- graph(c(0,1,0,2,0,3,1,4,1,2,3,4,3,5,4,5,5,2),n=6,dir=FALSE)
# V(g9)$name<-c(1:6)
# V(g9)$label<-V(g9)$name
# coords <- c(0, 0, 1.00000000000000, 0,0.500000000000000, 0.866025403784439, 0.300000000000000, 0.200000000000000, 0.441421356237309, 0.341421356237310,0.248236190979496,0.393185165257814)
# coords <- matrix(coords, 6,2,byrow=T)
# plot(g9,layout=coords)