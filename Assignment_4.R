# hw4sample.R - sample code for hw4
#
# @author: Yu-Ru Lin
# @refined by: Muheng Yan
# @date: 2015-02-18

library('foreign') 
library('ggplot2')
library(plyr) 

load.data.dji <- function() {
  dataset <- test
  print(head(dataset))
  print(dim(dataset))
  n = nrow(dataset)
  #dataset = t(dataset)
  return(dataset)
}
do.pca <- function(dataset,labels,
                   do.screeplot=F,do.scatter=F,do.biplot=F,do.loadingplot=F) {
  data.pca = prcomp(dataset, scale=TRUE) 
  data.pc = predict(data.pca)
  if (do.screeplot) {plot(data.pca, main='screeplot for PCA on financial indicators');}
  if (do.scatter) {
    plot(data.pc[,1:2], type="n")
    text(x=data.pc[,1], y=data.pc[,2],labels=labels)    
  }
  if (do.biplot) biplot(data.pca)
  if (do.loadingplot) {
    plot(data.pca$rotation[,1],type='l')
    #     plot(data.pc[,1],type='l')
  }
  data.pc
}
do.mds <- function(dataset,labels,do.scatter=T) {
  data.dist = dist(dataset)
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'p')
    text(data.mds,labels=labels)       
  }
  data.mds
}
do.kmeans <- function(dataset,labels,k=3,do.scatter=F) {
  set.seed(666)
  data.clu = kmeans(dataset, centers=k, nstart=10)
  if (do.scatter) {
    mds = do.mds(dataset,do.scatter = F)
    plot(mds,type='n')
    text(mds,labels=labels,col=rainbow(k)[data.clu$cluster])    
  }
  data.clu
}
do.hclust <- function(dataset,labels,method = 'complete',k=3,do.dendrogram=T,do.scatter=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method=method) ## change method to be single, complete, average, etc.
  if (do.dendrogram) plot(hc)
  hc1 = cutree(hc,k)
  #print(hc1)
  if (do.scatter) {
    mds = do.mds(dataset,do.scatter = F)
    plot(mds,type='n')
    text(mds,labels=labels,col=rainbow(k)[hc1])    
  }
  hc1
}
cluster.purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}
cluster.entropy <- function(clusters,classes) {
  en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {if (p) -p*log2(p) else 0} ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}
rollcall.simplified <- function(df) {
  no.pres <- subset(df, state < 99)
  ## to group all Yea and Nay types together
  for(i in 10:ncol(no.pres)) {
    no.pres[,i] = ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 1, -1, no.pres[,i])
  }
  
  return(as.matrix(no.pres[,10:ncol(no.pres)]))
}
load.roll.call <- function(congr) { ## extract the 10th congress data by default
  data.url = 'http://www.yurulin.com/class/spring2015_datamining/data/roll_call'
  data.files = c("sen101kh.dta", "sen102kh.dta",
                 "sen103kh.dta", "sen104kh.dta",
                 "sen105kh.dta", "sen106kh.dta",
                 "sen107kh.dta", "sen108kh_7.dta",
                 "sen109kh.dta", "sen110kh_2008.dta",
                 "sen111kh.dta")
  dataset = read.dta(file.path(data.url, data.files[congr]), convert.factors = FALSE)
  dataset = subset(dataset, state < 99)
  print(dim(dataset))
  print(head(dataset[,1:12]))
  dataset
}
roll.call.mds <- function(dataset,clu,do.scatter=F,do.scatter.ggplot=F) {
  get.dist <- function(m) {
    dist(m %*% t(m))
  }
  data1 = rollcall.simplified(dataset)
  #print(dim(data1))
  #print(head(data1[,1:12]))  
  data.dist = get.dist(data1)
  lbls = dataset$name
  party = mapvalues(dataset$party,from=c(100, 200, 328),to=c("Dem", "Rep", "Ind") )
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'n')
    text(data.mds,labels=lbls,col=rainbow(3)[clu])       
  }
  data2 = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,party=party,clu=factor(clu))
  if (do.scatter.ggplot) {
    p = ggplot(aes(x=x,y=y,shape=party,color=clu), data=data2) +
      geom_point(size=4,alpha=0.5) +
      geom_text(aes(x=x,y=y,shape=party,color=clu,label=name), size=3)
    print(p)
  }
  print("purity")
  print(cluster.purity(clu,party))
  print("entropy")
  print(cluster.entropy(clu,party))
  data.mds  
}


#--------task 1---------
  df = test
  df = t(scale(df))
  labels = rownames(df)
  #job 1
  temp = do.pca(df,labels = labels,do.screeplot = T,do.loadingplot = T)
  #job 2
  temp = do.pca(df,labels = labels,do.scatter = T)
  #job 3
  temp = do.mds(df,labels = labels)
  #job 4
    #k = 3
    temp = do.kmeans(df,labels = labels, do.scatter = T)
    temp = do.hclust(df,labels = labels, do.scatter = T)
    temp = do.hclust(df,labels = labels, method = 'single', do.scatter = T)
    temp = do.hclust(df,labels = labels, method = 'average', do.scatter = T)
    #k = 6
    temp = do.kmeans(df,labels = labels, k = 6, do.scatter = T)
    temp = do.hclust(df,labels = labels, k = 6,do.scatter = T)
    temp = do.hclust(df,labels = labels, k = 6,method = 'single', do.scatter = T)
    temp = do.hclust(df,labels = labels, k = 6,method = 'average', do.scatter = T)

#----------task 2---------

  df = load.roll.call(congr = 11)
  #job 1
  default.cluster = mapvalues(df$party,from=c(100, 200, 328),to=c(1,2,3))
  roll.call.mds(df,clu = default.cluster,do.scatter=T)
  #job 2
    #cluster
    df1 = rollcall.simplified(df)
    kmeans.cluster = do.kmeans(df1,labels = labels,k = 2)$cluster
    h.comlete.cluster = do.hclust(df1,labels = labels,k = 2,do.dendrogram=F)
    h.single.cluster = do.hclust(df1,labels = labels,k = 2,method = 'single', do.dendrogram=F)
    h.average.cluster = do.hclust(df1,labels = labels,k = 2,method = 'average', do.dendrogram=F)
    #mds
    temp = roll.call.mds(df,clu = kmeans.cluster,do.scatter=T)
    temp = roll.call.mds(df,clu = h.comlete.cluster,do.scatter=T)
    temp = roll.call.mds(df,clu = h.single.cluster,do.scatter=T)
    temp = roll.call.mds(df,clu = h.average.cluster,do.scatter=T)
  #job 3
  cluster.eval = data.frame(default.cluster,
                            kmeans.cluster,
                            h.comlete.cluster=(3-h.comlete.cluster),
                            h.single.cluster=(3-h.single.cluster),
                            h.average.cluster=(3-h.average.cluster))
  cluster.eval$name = df$name
  
  
  print("Dem clustered as Rep")
  print("k-mean")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 1 & cluster.eval$kmeans.cluster[i] == 2){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical complete")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 1 & cluster.eval$h.comlete.cluster == 2){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical single")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 1 & cluster.eval$h.single.cluster[i] == 2){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical average")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 1 & cluster.eval$h.average.cluster[i] == 2){
      print(cluster.eval$name[i])
    }
  }
  
  print("Rep clustered as Dem")
  print("k-mean")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 2 & cluster.eval$kmeans.cluster[i] == 1){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical complete")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 2 & cluster.eval$h.comlete.cluster == 1){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical single")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 2 & cluster.eval$h.single.cluster[i] == 1){
      print(cluster.eval$name[i])
    }
  }
  
  print("hierachical average")
  for(i in 1:nrow(cluster.eval)){
    if(cluster.eval$default.cluster[i] == 2 & cluster.eval$h.average.cluster[i] == 1){
      print(cluster.eval$name[i])
    }
  }
  
  #job4
  temp = roll.call.mds(df,clu = kmeans.cluster)
  temp = roll.call.mds(df,clu = h.comlete.cluster)
  temp = roll.call.mds(df,clu = h.single.cluster)
  temp = roll.call.mds(df,clu = h.average.cluster)

