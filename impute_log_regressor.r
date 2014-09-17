
=========================================================== R - CODE==================================================
ds<- read.table("ds_final_test.txt", Header= TRUE, sep"\t")

## STEP 1:  Handling Missing DATA {AMELIA}: Uses imputation methods to fill in the missing data
ds_am=amelia(ds[,-10],m=1)
hadoop<-ds_am$imputations[[1]]

had_na_rem=hadoop[!is.na(ds[,10]),]
had_na=hadoop[is.na(ds[,10]),-1] # no need of row numbers



##STEP 2:  Data cleaning and setting up the data for model input
data<-had_na_rem
rows=nrow(data)
data<-cbind(data,ds[!is.na(ds[,10]),10])
colnames(data)[31]<- "hicov"
data[,31]<-data[,31]-1
data2 <- data[,-1]  # Remove first column.. it is just a row number
data<- data2

head(data)

##  Separating the test and training data
set.seed(1)
index=sample(c(1:rows),floor(.8*rows),replace=FALSE)

train=data[index,]


## STEP 3 : LEARNING >> Using log regression algorithm to predict the probabilities
model.fit<-glm(data[,30]~.,data=data[,-30], family=binomial)

test=data[-index,]

# Predict it on test set 
glm.probs =predict (model.fit ,test[,-30], type="response")

## Convert the prediction to labels  for accuracy check
glm.probs[glm.probs>=0.5] <-1
glm.probs[glm.probs< 0.5] <-0

## Accuracy
accuracy <- sum(glm.probs == test[,30])/ nrow(test)
accuracy ## 88.67 %

## STEP 4 : Predicting the new probabilities on missing data
probs.prediction <- predict (model.fit ,had_na, type="response")

#probs.prediction gives the probabilities