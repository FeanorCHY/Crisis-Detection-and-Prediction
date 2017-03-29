library(igraph)
library(dplyr)
require(sna)
# library("Cairo")
require(graphics)
# library(gplots)
library(grDevices)
library(png)
library(caTools)
library(ggplot2)
# library("netbiov")
library(splitstackshape)
# library("RCytoscape")
load("~/Documents/Data Mining/final project/ReplaceOutside.RData")
TargetCountry<-'Iraq'
allevent<-concat.split.multiple(allevent, "Source.Name", " ")
allevent<-concat.split.multiple(allevent, "Target.Name", " ")
names(allevent)[names(allevent)=='Source.Name_01']<-'Source.Name'
names(allevent)[names(allevent)=='Target.Name_01']<-'Target.Name'
# allevent$Source.Name<-factor(allevent$Source.Name)
# allevent[allevent$Source.Country!=TargetCountry,]$Source.Name<-"outside"
# allevent[allevent$Target.Country!=TargetCountry,]$Target.Name<-"outside"
allevent<-allevent[!allevent$Source.Country!=TargetCountry,]
allevent<-allevent[!allevent$Target.Country!=TargetCountry,]
# allevent <- data.frame(lapply(allevent, as.character), stringsAsFactors=FALSE)
# for(i in 1:nrow(allevent))
# {
#   if(sum(allevent$Source.Country[i]==TargetCountry)!=1){
#     allevent$Source.Name[i]<-"outside"
#     allevent$Source.Country[i]<-"outside"
#   }
#   if(sum(allevent$Target.Country[i]==TargetCountry)!=1){
#     allevent$Target.Name[i]<-"outside"
#     allevent$Target.Country[i]<-"outside"
#   }
# }

# a<-graph.data.frame(allevent, directed = T)
# table(allevent$Source.Name)
# allevent$Event.Date_3<-NULL
# allevent$Latitude<-NULL
# allevent$Longitude<-NULL


#coordinate
index_x<-c(51,30,53,43,35,35,47,35.6,34.0,56.7,62,51,44,38,34.7,54,47)
index_y<-c(25,25,33,32,32,30,29,33.6,32.8,20.2,17,25,25,35,39.7,24,15)
l=matrix(0,17,2)
l[,1]=index_x
l[,2]=index_y


# d1<-allevent[Event.Date_1==2005]
# d1<-d1[Event.Date_2==1]

eventx<-allevent[allevent$Source.Country==TargetCountry,]
eventx2<-allevent[allevent$Target.Country==TargetCountry,]
eventx4<-rbind.data.frame(eventx,eventx2)
allevent<-eventx4[!duplicated(eventx4$Event.ID),]


# d1[d1$Target.Country=='',]$Target.Country<-d1[d1$Target.Country=='',]$Target.Name
# d1[d1$Source.Country=='',]$Source.Country<-d1[d1$Source.Country=='',]$Source.Name
# group<-group_by(allevent, Source.Country,Target.Country)
group<-group_by(allevent, Source.Name,Target.Name)
xall<-summarise(group,length(Event.Date_2))
m_all<-as.matrix(xall)
# labels <- unique( c(m_all[,1], m_all[,2]) )



# ima <- readPNG("./map.png")
d1 <- subset(allevent, Event.Date_1==2005)
d1 <- subset(d1, Event.Date_2==1)
# group<-group_by(d1, Source.Country,Target.Country)
group<-group_by(d1, Source.Name,Target.Name)
x<-summarise(group,length(Event.Date_2))
m<-as.matrix(x)
labels <- unique( c(m[,1], m[,2]) )

A <- matrix(0, length(labels), length(labels))
grahSet=matrix(0,132,length(labels)*3+1)
row=c();
img <- readPNG("./iraq.png")
for(i in 2005:2015)
{
  for(j in 1:12)
  {
    d1 <- subset(allevent, Event.Date_1==i)
    d1 <- subset(d1, Event.Date_2==j)
    # group<-group_by(d1, Source.Country,Target.Country)
    group<-group_by(d1, Source.Name,Target.Name)
    x<-summarise(group,weight=length(Event.Date_2))
    # x2<-aggregate(as.numeric(d1$Intensity), by=list(source=d1$Source.Country,target=d1$Target.Country), FUN=sum)
    x2<-aggregate(as.numeric(d1$Intensity), by=list(source=d1$Source.Name,target=d1$Target.Name), FUN=sum)
    names(x)[3]<-"weight"
    # x$weight<-x$weight/length(x$weight)
    m<-as.matrix(x)
    labels <- unique( c(m[,1], m[,2]) )
    A <- matrix(0, length(labels), length(labels))
    rownames(A) <- colnames(A) <- labels
    A[ m[,1:2] ] <- as.numeric( m[,3] )
    diag(A)<-0
    # A <- ifelse(abs(A)>=1,1,A)
    g = graph.adjacency(A,mode="undirected",weighted=TRUE) 
    dia<-diameter(g)
    # clo<-closeness(A)
    bet<-betweenness(A)
    pr<-page.rank(g)$vector
    aut<-authority.score(g)$vector
    deg = degree(A)
    hub<-hub.score(g)$vector
    row=c(dia,as.numeric(hub),bet,deg)
    # grahSet[(i-2005)*12+j,1:length(row)]<-row
    
    m<-as.matrix(x2)
    A2 <- matrix(0, length(labels), length(labels))
    rownames(A2) <- colnames(A2) <- labels
    A2[ m[,1:2] ] <- as.numeric( m[,3] )
    g2 = graph.adjacency(A2,mode="undirected",weighted=TRUE) 
    
    deg = degree(A)
    top = order(bet,decreasing=T)[1:5] ## the top-5 nodes with highest degrees
#     E(g2)$weight
#     E(g)
    V(g)$label.color = "blue"
    V(g)$label.cex=1.5
    E(g)$color = "black"
    V(g)[ top ]$label.color = "red" ## highlight the top-5 nodes
    set.seed(1)
    plot.new()
    # img <- readPNG("./map.png")
    r = as.raster(img[,,1:3])
    # rasterImage(img, -0.5, -1, 1.5, 1.8)
    r[img[,,4] == 0] = "white"
    # rasterImage(img, -0.2, -0.2, 1.2, 1.2)
    # rasterImage(img, 0, 0, 1, 1,bg="blue")
    par(new=T)
    # plot(g,layout=l,edge.width=E(g)$weight/10)
    V(g)$size = abs(bet)*1.6
    plot(g,layout=layout.circle,edge.width=E(g)$weight)
    title(paste("betweenness centrality(",i,"-",j,")"))
    if(i==2010){
      print(1)
    }
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





# m <- outer(5:2,1:2,function(x,y) sin(sqrt(x*y)/30))
# rgb.palette <- colorRampPalette(c( "orange","red"), space = "rgb")
# t<-filled.contour(m,col = rgb.palette(20))
# par(bg = )
# plot(g,layout=l,edge.width=E(g)$weight/10)
# 
# colors=heat.colors(50,alpha = 0.1)
# count <- length(colors)
# m <- matrix(1:count, count, 1)
# image(m, col=colors, ylab="", axes=FALSE)
# 
# plot(img)
# image(ima)
# g <- rasterGrob(img, interpolate=TRUE) 
# 
# 
# 
# plot(1:2,type="n")
# rasterImage(r,1,1,1,1)
# 
# Gif <- read.gif("http://www.openbsd.org/art/puffy/ppuf600X544.gif")
# n <- dim(Gif$image)
# image(t(Gif$image)[n[2]:1,n[1]:1],col=Gif$col,axes=F)
# op <- par(new=T)
# plot(1:100,new=T)
# plot(g,layout=l,edge.width=E(g)$weight/10)
# par(op)
# 
# 
# plot.new()
# # img <- readPNG("./map.png")
# img <- readPNG("./iraq.png")
# r = as.raster(img[,,1:3])
# # rasterImage(img, -0.5, -1, 1.5, 1.8)
# r[img[,,4] == 0] = "white"
# # rasterImage(img, -0.2, -0.2, 1.2, 1.2)
# rasterImage(img, 0, 0, 1, 1)
# par(new=T)
# plot(g,layout=l,edge.width=E(g)$weight/10)
# par(bg="yellow")
# background<-rgb(255,100,0,max=255,alpha = 0)
# par(bg=background)
# plot.new()
# 
# 
# gD <- simplify(graph.data.frame(A, directed=FALSE))
# degAll <- degree(A)
# gD.cyt <- igraph.to.graphNEL(gD)
# gD.cyt <- initNodeAttribute(gD.cyt, 'degree', 'numeric', 0) 
# gD.cyt <- initNodeAttribute(gD.cyt, 'betweenness', 'numeric', 0) 
# gD.cyt <- initEdgeAttribute (gD.cyt, "weight", 'integer', 0)
# gD.cyt <- initEdgeAttribute (gD.cyt, "similarity", 'numeric', 0)
# gDCW <- new.CytoscapeWindow("Les Miserables", graph = gD.cyt, overwriteWindow = TRUE)
# 
# 
# 
# x2<-aggregate(as.numeric(crisis$ins+crisis$reb+crisis$dpc+crisis$erv+crisis$ic), by=list(source=crisis$country), FUN=sum)

