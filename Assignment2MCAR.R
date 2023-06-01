#Question 2 

rm(list = ls(all = TRUE))
library(impute); library(Amelia)

#load data matrix question 1
dataCO=read.table(file.choose(), header = TRUE)
dataCO$id <- NULL


# transform to matrix 

dataCO <- data.matrix(dataCO)


# 15 percent at NA for column uptake question 2
impdata<- dataCO; missinglvl<- 0.15


set.seed(500)
mis15<- sample(1:length(dataCO[,6]), round(length(dataCO[,6])*missinglvl), replace=F)

impdata[mis15,6 ] <- NA

#imputation question 3


knear<- impute.knn(impdata,k=3)
knres=sqrt(sum((dataCO[mis15,6]-knear$data[mis15,6])^2) /length(mis15))  



#amelia 

amy=amelia(impdata, m=5) 
impamelia=(amy$imputations$imp1+amy$imputations$imp2+amy$imputations$imp3+amy$imputations$imp4+amy$imputations$imp5)/5
ameliares=sqrt(sum((dataCO[mis15,6]-impamelia[mis15,6])^2) /length(mis15)) 

# Mean substitution

meansub <- mean(impdata[,6],na.rm=TRUE)

impdata[is.na(impdata[,6])] <- meansub

meanres= sqrt((sum((dataCO[mis15,6]-impdata[mis15])^2)) /length(mis15))  


