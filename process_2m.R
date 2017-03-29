library(igraph)
library(dplyr)
require(sna)
# library("Cairo")
load("~/Documents/Data Mining/final project/ReplaceOutside.RData")
# allevent$Source.Name<-factor(allevent$Source.Name)
# allevent[allevent$Source.Country!=MidECountr$country]$Source.Name<-"outside"
# allevent <- data.frame(lapply(allevent, as.character), stringsAsFactors=FALSE)
# for(i in 1:nrow(allevent))
# {
#   if(sum(allevent$Source.Country[i]==MidECountr$country)!=1){
#     allevent$Source.Name[i]<-"outside"
#     allevent$Source.Country[i]<-"outside"
#   }
#   if(sum(allevent$Target.Country[i]==MidECountr$country)!=1){
#     allevent$Target.Name[i]<-"outside"
#     allevent$Target.Country[i]<-"outside"
#   }
# }

# a<-graph.data.frame(allevent, directed = T)
# table(allevent$Source.Name)
# allevent$Event.Date_3<-NULL
# allevent$Latitude<-NULL
# allevent$Longitude<-NULL


# d1<-allevent[Event.Date_1==2005]
# d1<-d1[Event.Date_2==1]

# d1[d1$Target.Country=='',]$Target.Country<-d1[d1$Target.Country=='',]$Target.Name
# d1[d1$Source.Country=='',]$Source.Country<-d1[d1$Source.Country=='',]$Source.Name

# group<-group_by(allevent, Source.Country,Target.Country)
group<-group_by(allevent, Source.Name,Target.Name)
xall<-summarise(group,length(Event.Date_2))
m_all<-as.matrix(xall)
labels <- unique( c(m_all[,1], m_all[,2]) )




d1 <- subset(allevent, Event.Date_1==2005)
d1 <- subset(d1, Event.Date_2==1)
# group<-group_by(d1, Source.Country,Target.Country)
group<-group_by(d1, Source.Name,Target.Name)
x<-summarise(group,length(Event.Date_2))
m<-as.matrix(x)

A <- matrix(0, length(labels), length(labels))
grahSet=matrix(0,132,length(labels)*6+2)
row=c();
lasrow<-NULL;
for(i in 2005:2015)
{
  for(j in 1:12)
  {
    d1 <- subset(allevent, Event.Date_1==i)
    d1 <- subset(d1, Event.Date_2==j)
    # group<-group_by(d1, Source.Country,Target.Country)
    group<-group_by(d1, Source.Name,Target.Name)
    x<-summarise(group,length(Event.Date_2))
    m<-as.matrix(x)
    A <- matrix(0, length(labels), length(labels))
    rownames(A) <- colnames(A) <- labels
    A[ m[,1:2] ] <- as.numeric( m[,3] )
    g = graph.adjacency(A,mode="undirected") 
    dia<-diameter(g)
    # clo<-closeness(A)
    bet<-betweenness(A)
    pr<-page.rank(g)$vector
    aut<-authority.score(g)$vector
    deg = degree(A)
    hub<-hub.score(g)$vector
    row=c(lasrow,dia,as.numeric(hub),bet,deg)
    if((i-2005)*12+j!=1)
      grahSet[(i-2005)*12+j,1:length(row)]<-row
    lasrow=c(dia,as.numeric(hub),bet,deg)
    print((i-2005)*12+j)
  }
}
# x<-table(d1$Source.Name)

# m = matrix(nrow=3,ncol=3)

# el[,1]=as.character(m[,3])

# labels <- unique( c(m[,1], m[,2]) )

# gplot(A, label=rownames(A))
# V(g)$size = 5
# E(g)$size = 5
# plot(g,vertex.label=rownames(A),edge.color='black')
# adj = get.adjacency(g)

# order(authority.score(g)$vector,decreasing=T)
# hub=hub.score(g)$vector 
# hub = hub.score(g)$vector 
# top = order(hub,decreasing=T)[1:5]
# plot(g, layout=layout.circle, main = 'circle')

# wc = walktrap.community(g)
# modularity(wc)
# membership(wc)
# gplot(wc, g, main='random-walk community')
# dend = as.dendrogram(wc, use.modularity=TRUE)
# plot(dend, nodePar=list(pch=c(NA, 20)))
# dendPlot(wc)
# sg = spinglass.community(g)
# modularity(sg)
# plot(sg, g, main='spin-glass community')
# 
# 
# 
# 
# 
# 
# 
# 
# net.bg <- g
# V(net.bg)$frame.color <- "white"
# V(net.bg)$color <- "orange"
# V(net.bg)$label <- "" 
# V(net.bg)$size <- 10
# E(net.bg)$arrow.mode <- 0
# plot(net.bg,vertex.label=rownames(A))
# l <- layout.fruchterman.reingold(net.bg, repulserad=vcount(net.bg)^3, 
#                                  area=vcount(net.bg)^2.4)
#  par(mfrow=c(1,1),  mar=c(0,0,0,0)) # plot two figures - 1 row, 2 columns
# plot(net.bg, layout=layout.fruchterman.reingold)
# plot(net.bg, layout=l,vertex.label=rownames(A))
# l <- layout.circle(net.bg)
# plot(net.bg,vertex.label=rownames(A), layout=l)

write.csv(grahSet,'graphSet_2m.csv')

l <-layout.reingold.tilford(g) 
