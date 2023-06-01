
rm(list = ls(all = TRUE))
library(impute); library(Amelia)

library(robustHD)
library(praznik)
library(ROCR)
library(Hmisc)
library(MXM)
library(mlbench)

datatest=read.csv(file.choose())


#mean imputation for column 5 and 7 
meanrm<- mean(datatest$rm, na.rm= TRUE)
datatest[is.na(datatest[,5]),5]<- meanrm

meandis<- mean(datatest$dis, na.rm= TRUE)
datatest[is.na(datatest[,7]),7]<- meandis

#standardize like assignment 1
for (s in 2:13)
  datatest[,s] <- standardize(datatest[,s])

# split data similar to my assignment 3
set.seed(16084253)
split <- sample(2,nrow(datatest), prob=c(0.5, 0.5),replace=TRUE)

testhouse<- datatest[split==1,]
trainhouse<-datatest[split==2,]


TSCR = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X1<- X[which(Y==0),i]
    X2<- X[which(Y==1),i]
    mu1<- mean(X1); mu2<- mean(X2)
    var1<- var(X1); var2<- var(X2)
    n1<- length(X1); n2<- length(X2)
    J[i]<- (mu1-mu2)/sqrt(var1/n1+var2/n2)
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}

# select top 4 feaures
datay<- datatest[,1]
datax<- datatest[,-c(1)]
k<- 4

TSCR(datax,datay,k)
JMI(datax, datay, k)
MRMR(datax, datay, k)

#logistic regression

logit <- glm(crim~dis+zn+b+medv+nox+indus+ptratio+tax+age+rad, data=trainhouse,
               family="binomial")
logittest <- glm(crim~dis+zn+b+medv+nox+indus+ptratio+tax+age+rad, data=testhouse,
             family="binomial")

#loading coefficient
summary(logit)

#apply to test data
perglm <- predict(logittest, newdata=testhouse, type="response")
perglmtrain <- predict(logittest, newdata=trainhouse, type="response")

#training set
predictobj<- prediction(predictions = perglmtrain, labels=trainhouse[,1])
perfo<- performance(predictobj, measure="tpr", x.measure="fpr")
plot(perfo, col="magenta", lwd=3, main="training ")

#test
predictobj2<- prediction(predictions = perglm, labels=testhouse[,1])
perfo2<- performance(predictobj2, measure="tpr", x.measure="fpr")
plot(perfo2, col="black", lwd=3, main="test ")

#auc value
somers2(perglm, testhouse[,1])
somers2(perglmtrain, trainhouse[,1])
