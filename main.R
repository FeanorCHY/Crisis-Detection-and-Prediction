library(car)
library(ggplot2)
library("reshape2")
library(locfit)
library(lars)
library(utils)
library(kernlab)
library(e1071)
library(MASS)
library(tree)
library(class)
library(rpart)
library(lattice)
library(pROC)
library(ROCR)
library(neuralnet)
library(dplyr)


# dataset<-data.frame(y=y[,2],grahSet)
suppressWarnings( library(ROCR))
# d = dataset
# d$y[d$y<10]<-0
# d$y[d$y>=10]<-1

globalauc=c()
globalF=c()
globalAC=c()
globalPROB=c()
globalacc=c()
globalfscore=c()

do.classification <- function(train.set, test.set, cl.name, verbose=F) 
{
  switch(cl.name, 
         nn = { # here we test k=3; you should evaluate different k's
           set.seed(666)
           f <- as.formula(paste("y ~", paste(names(d)[2:ncol(train.set)], collapse = " + ")))
           model<-neuralnet(f,d,hidden=20,rep=100)
           prob<-neuralnet::compute(model,test.set[,2:ncol(test.set)])$net.result
           prob<- ifelse(prob>1,1,prob)
           prob<-ifelse(prob<0,0,prob)
           #print(cbind(prob,as.character(test.set$y)))
           print("run nn")
           prob
         },
         knn1 = { # here we test k=3; you should evaluate different k's
           prob = knn(train.set[,-11], test.set[,-11], cl=train.set[,11], k = 1, prob=T)
           prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         knn3 = { # here we test k=3; you should evaluate different k's
           prob = knn(train.set[,-11], test.set[,-11], cl=train.set[,11], k = 3, prob=T)
           prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         knn5 = { # here we test k=3; you should evaluate different k's
           prob = knn(train.set[,-11], test.set[,-11], cl=train.set[,11], k = 5, prob=T)
           prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         lr = { # logistic regression
           model = glm(y~., family=binomial, data=train.set)
           if (verbose) {
             print(summary(model))             
           }
           prob = predict(model, newdata=test.set, type="response") 
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         nb = {
           model = naiveBayes(y~., data=train.set)
           prob = predict(model, newdata=test.set, type="raw") 
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtree = {
           model = tree(y~.,data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           
           if (0) { # here we use the default tree, 
             ## you should evaluate different size of tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           #print(cbind(prob,as.character(test.set$y)))
           #prob = prob[,1]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtreeP17 = {
           model = tree(y~.,data=train.set)
           prune.tree(model,k=1.7)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           
           if (0) { # here we use the default tree, 
             ## you should evaluate different size of tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           #print(cbind(prob,as.character(test.set$y)))
           #prob = prob[,1]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtreeP3 = {
           model = tree(y~.,data=train.set)
           prune.tree(model,k=3)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           
           if (0) { # here we use the default tree, 
             ## you should evaluate different size of tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           #print(cbind(prob,as.character(test.set$y)))
           #prob = prob[,1]/rowSums(prob) # renormalize the prob.
           prob
         },
         svmRA = {
           model = svm(y~., data=train.set,kernel='radial', probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           
           prob = predict(model, newdata=test.set, probability=T)
           #####prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmPOLY = {
           model = svm(y~., data=train.set,kernel='polynomial', probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           
           prob = predict(model, newdata=test.set, probability=T)
           #####prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmSIG = {
           model = svm(y~., data=train.set,kernel='sigmoid', probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           
           prob = predict(model, newdata=test.set, probability=T)
           #####prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         ada = {
           model = ada(y~., data = train.set)
           prob = predict(model, newdata=test.set, type='probs')
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob)
           prob
         }
  )
}

k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5) {
  ## default: 10-fold CV, cut-off 0.5 
  n.obs <- nrow(dataset) # no. of observations 
  s = sample(n.obs)
  errors = dim(k.fold)
  probs = NULL
  actuals = NULL
  for (k in 1:k.fold) {
    test.idx = which(s %% k.fold == (k-1) ) # use modular operator
    train.set = dataset[-test.idx,]
    test.set = dataset[test.idx,]
    cat(k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    prob = do.classification(train.set, test.set, cl.name)
    predicted = as.numeric(prob > prob.cutoff)
    actual = test.set$y
    confusion.matrix = table(actual,predicted)
    if(length(predicted)-sum(predicted)==0)
    {
      predicted[1]<-1-predicted[1]
      confusion.matrix = table(actual,predicted)
      confusion.matrix[1,2]=0
      confusion.matrix[2,1]=confusion.matrix[2,1]+1
    }
    else if(sum(predicted)==0)
    {
      predicted[1]<-1-predicted[1]
      confusion.matrix = table(actual,predicted)
      confusion.matrix[2,2]=0
      confusion.matrix[1,2]=confusion.matrix[1,2]+1
    }
    confusion.matrix
    error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
    errors[k] = error
    cat('\t\terror=',error,'\n')
    probs = c(probs,prob)
    actuals = c(actuals,actual)
    ## you may compute other measures and store them in arrays
  }
  avg.error = mean(errors)
  cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
  
  ## plot ROC
  result = data.frame(probs,actuals)
  pred =  ROCR::prediction(result$probs,result$actuals) 
  perf = performance(pred, "tpr","fpr")
  plot(perf)  
  globalAC<<-c(globalAC,actuals)
  globalPROB<<-c(globalPROB,probs)
  #roc<<- c(roc,plot.roc(actual,prob, main="ROCcomparison", percent=TRUE, col="1"))
  #rocx<<- plot.roc(actual,prob, main="ROCcomparison", percent=TRUE, col="1")
  #roc<<-c(roc,rocx)
  
  
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name='auc') {
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    #     print(slot(perf, "x.values"))
    #     print(slot(perf, "y.values"))
    m
  }
  err = mean(get.measure(pred, 'err'))
  
  precision = mean(get.measure(pred, 'prec'),na.rm=T)
  recall = mean(get.measure(pred, 'rec'),na.rm=T)
  fscore = mean(get.measure(pred, 'f'),na.rm=T)
  accuracy = mean(get.measure(pred, 'acc'),na.rm=T)
  cat('error=',err,'precision=',precision,'recall=',recall,'f-score',fscore,'accuracy',accuracy,'\n')
  auc = get.measure(pred, 'auc')
  cat('auc=',auc,'\n')
  globalauc <<- c(globalauc,auc)
  globalacc <<- c(globalacc,accuracy)
  globalF <<- c(globalF,fscore)
}
pre.test <- function(dataset, cl.name, r=0.6, prob.cutoff=0.5) {
  ## Let's use 60% random sample as training and remaining as testing
  ## by default use 0.5 as cut-off
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.train = floor(n.obs*r)
  train.idx = sample(1:n.obs,n.train)
  train.idx
  train.set = dataset[train.idx,]
  test.set = dataset[-train.idx,]
  cat('pre-test',cl.name,':',
      '#training:',nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name)
  # prob is an array of probabilities for cases being positive
  
  ## get confusion matrix
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$y
  confusion.matrix = table(actual,predicted)
  if(length(predicted)-sum(predicted)==0)
  {
    predicted[1]<-1-predicted[1]
    confusion.matrix = table(actual,predicted)
    confusion.matrix[1,2]=0
    confusion.matrix[2,1]=confusion.matrix[2,1]+1
  }
  else if(sum(predicted)==0)
  {
    predicted[1]<-1-predicted[1]
    confusion.matrix = table(actual,predicted)
    confusion.matrix[2,2]=0
    confusion.matrix[1,2]=confusion.matrix[1,2]+1
  }
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  cat('error rate:',error,'\n')
  # you may compute other measures based on confusion.matrix
  # @see handout03 p.32-36
  
  
}
my.classifier <- function(dataset, cl.name='knn', do.cv=F) {
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.cols <- ncol(dataset) # no. of predictors
  cat('my dataset:',
      n.obs,'observations',
      n.cols-1,'predictors','\n')
  print(dataset[1:3,])
  cat('label (y) distribution:')
  print(table(dataset$y))
  
  pre.test(dataset, cl.name)
  if (do.cv) k.fold.cv(dataset, cl.name)
}

final_fixed <- read.csv("~/Downloads/final_fixed.csv")
y<-ifelse(final_fixed$total>=9,1,0)
y<-y[2:113]
grahSet<-grahSet[1:112,]
dataset<-data.frame(y=y,grahSet)
# dataset<-data.frame(y=y[,1],grahSet)#,predx=predx[1:112,1:5])
suppressWarnings( library(ROCR))
d = dataset
my.classifier(d, cl.name='lr',do.cv=T)
my.classifier(d, cl.name='knn1',do.cv=T)
my.classifier(d, cl.name='knn3',do.cv=T)
my.classifier(d, cl.name='knn5',do.cv=T)
my.classifier(d, cl.name='nb',do.cv=T)
my.classifier(d, cl.name='dtree',do.cv=T)
my.classifier(d, cl.name='dtreeP17',do.cv=T)
my.classifier(d, cl.name='dtreeP3',do.cv=T)
my.classifier(d, cl.name='svmRA',do.cv=T)
my.classifier(d, cl.name='svmPOLY',do.cv=T)
my.classifier(d, cl.name='svmSIG',do.cv=T)
my.classifier(d, cl.name='nn',do.cv=T)
# neuralnet(y ~ . ,data=d,hidden=2,rep=5,linear.output = T)

#glm(y ~ ., family = binomial, data = d) 
aucF=data.frame(Fscore=globalF,AUC=globalauc)
names <- c("LR","KNN1","KNN3","KNN5","NB","dtree","dtreeP17","dtreeP3","SVMRA","SVMPOLY","SVMSIG","NN")
barplot(globalauc[c(1,5,7,11,12)],names.arg = names[c(1,5,7,11,12)])
title("AUC")
barplot(globalF,names.arg = names)
barplot(globalacc[c(1,5,7,11,12)],names.arg = names[c(1,5,7,11,12)])
title("Accuracy")

names <- c("LR","KNN","NB","dtree","SVM")
barplot(globalauc[c(1,3,5,6,9)],names.arg = names)
barplot(globalF[c(1,3,5,6,9)],names.arg = names)

#ROC
#roc<<- c(roc,plot.roc(actual,prob, main="ROCcomparison", percent=TRUE, col="1"))
k=1
roc1<- plot.roc(globalAC[(1000*(k-1)+1):(1000*k)],globalPROB[(1000*(k-1)+1):(1000*k)], main="ROCcomparison", percent=TRUE, col="1")
par(new=TRUE)
k=2
roc2<- plot.roc(globalAC[(1000*(k-1)+1):(1000*k)],globalPROB[(1000*(k-1)+1):(1000*k)], percent=TRUE, col="2")
par(new=TRUE)
k=5
roc3<- plot.roc(globalAC[(1000*(k-1)+1):(1000*k)],globalPROB[(1000*(k-1)+1):(1000*k)], percent=TRUE, col="3")
par(new=TRUE)
k=8
roc4<- plot.roc(globalAC[(1000*(k-1)+1):(1000*k)],globalPROB[(1000*(k-1)+1):(1000*k)], percent=TRUE, col="4")
par(new=TRUE)
k=9
roc5<- plot.roc(globalAC[(1000*(k-1)+1):(1000*k)],globalPROB[(1000*(k-1)+1):(1000*k)], percent=TRUE, col="5")
#testobj<- roc.test(roc1,roc2,roc3,roc4,roc5)
#text(50, 50, labels=paste("p-value =",format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright",inset=c(0.15,0), legend=c("LR","KNN-1","NB","DtreeP3","SVMSIG"),text.width=30, col=c("1", "2", "3", "4", "5"), lwd=20,cex=c(0.5,0.5,0.5,0.5,0.5))

#roc1 <- plot.roc(aSAH$outcome, aSAH$s100, main="Statisticalcomparison", percent=TRUE, col="1")
#roc2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE,col="2")
Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1)) 
print(net.sqrt <- neuralnet(Sqrt~Var1,  sqrt.data, hidden=10, threshold=0.01))
pr<-neuralnet::compute(net.sqrt, (1:10)^2)$net.result
