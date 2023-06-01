rm(list = ls(all = TRUE))
library(impute); library(Amelia)
 
#load data matrix
dataCO=read.table(file.choose(), header = TRUE)
dataCO$id <- NULL


# transform to matrix 

dataCO <- data.matrix(dataCO)
head(dataCO)
tail(dataCO)

# 15 percent at NA for column uptake question 2
impdata<- dataCO; missinglvl<- 0.15



set.seed(500)
mis15<- sample(1:length(dataCO[,6]), round(length(dataCO[,6])*missinglvl), replace=F)

impdata[mis15,6 ] <- NA

#imputation question 3



#k nearest neighbor

knear<- impute.knn(impdata,k=3)
knres=sqrt(sum((dataCO[mis15,6]-knear$data[mis15,6])^2) /length(mis15))  
impdata


#amelia 

amy=amelia(impdata, m=5) 
impamelia=(amy$imputations$imp1+amy$imputations$imp2+amy$imputations$imp3+amy$imputations$imp4+amy$imputations$imp5)/5
ameliares=sqrt(sum((dataCO[mis15,6]-impamelia[mis15,6])^2) /length(mis15)) 
dataCO[mis15,6]
impamelia[mis15,6]

rm(amy,impamelia,missinglvl,ameliares,knres,mis15,missinglvl,knear,impdata)

#MAR
impdata <- dataCO
range(dataCO[,6])
rangedata = impdata[,6] > 37
index = which(rangedata == TRUE)

set.seed(500)
x2 = sample(index ,size = round(length(index)*0.3))
impdata[,6][x2] <- NA

#mean impute

meansub <- mean(impdata[,6],na.rm=TRUE)

impdata[is.na(impdata[,6])] <- meansub

meanres= sqrt(sum((dataCO[x2,6]-impdata[x2])^2) /length(x2))  

dataCO[x2,6]
impdata[x2]


#knn

rm(impdata,meanres,meansub)
impdata <- dataCO
range(dataCO[,6])
rangedata = impdata[,6] > 37
index = which(rangedata == TRUE)

set.seed(500)
x2 = sample(index ,size = round(length(index)*0.3))
impdata[,6][x2] <- NA

knear<- impute.knn(impdata,k=3)
knear
knres=sqrt(sum((dataCO[x2,6]-knear$data[x2,6])^2) /length(x2))  
dataCO[x2,6]
knear$data[x2,6]

#amelia 
rm(impdata,knres,knear)
impdata <- dataCO
range(dataCO[,6])
rangedata = impdata[,6] > 37
index = which(rangedata == TRUE)

set.seed(500)
x2 = sample(index ,size = round(length(index)*0.3))

impdata[,6][x2] <- NA

amy=amelia(impdata, m=5) 
impamelia=(amy$imputations$imp1+amy$imputations$imp2+amy$imputations$imp3+amy$imputations$imp4+amy$imputations$imp5)/5
ameliares=sqrt(sum((dataCO[x2,6]-impamelia[x2,6])^2) /length(x2)) 

dataCO[x2,6]
impamelia[x2,6]  
