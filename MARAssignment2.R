
rm(list = ls(all = TRUE))
library(impute); library(Amelia) # Libraries we will be using

# Loading the data
dataCO=read.table(file.choose(), header = TRUE)
dataCO$id <- NULL

# Reformatting as matrix

dataCO <- data.matrix(dataCO)


impdata<- dataCO


#setting the MAR mechnaism
rangedata = impdata[,6] > 37
index = which(rangedata == TRUE)


range(dataCO[,5])
set.seed(500)
x1 = sample(index ,size = round(length(index)*0.3))

impdata[,5][x1] <- NA


#mean impute

meansub <- mean(impdata[,5],na.rm=TRUE)

impdata[is.na(impdata[,5])] <- meansub

meanres= sqrt(sum((dataCO[x1,5]-impdata[x1])^2) /length(x1))  



#knn neighbor
#new dataset for each impute

impdata2 <- dataCO
rangedata = impdata2[,6] > 37
index = which(rangedata == TRUE)

set.seed(500)
x1 = sample(index ,size = round(length(index)*0.3))
impdata2[,5][x1] <- NA

knear<- impute.knn(impdata2,k=3)

knres=sqrt(sum((dataCO[x1,5]-knear$data[x1,5])^2) /length(x1))  

#amelia 

impdata3 <- dataCO

rangedata = impdata3[,6] > 37
index = which(rangedata == TRUE)

set.seed(500)
x1 = sample(index ,size = round(length(index)*0.3))

impdata3[,5][x1] <- NA

amy=amelia(impdata3, m=5) 
impamelia=(amy$imputations$imp1+amy$imputations$imp2+amy$imputations$imp3+amy$imputations$imp4+amy$imputations$imp5)/5
ameliares=sqrt(sum((dataCO[x1,5]-impamelia[x1,5])^2) /length(x1)) 
