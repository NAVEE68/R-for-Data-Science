#Naive Bayes

#Importing the dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[2:5]

class(dataset$Purchased)

#Encoding the target feature as factor
dataset$Purchased=factor(dataset$Purchased,levels = c(0,1))

colSums(is.na(dataset))

#Feature Scaling
dataset[,c(2,3)]=scale(dataset[,c(2,3)])

#Splitting the dataset into test and train
library(caTools)
set.seed(24)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

attach(dataset)
#Fitting a model
library(e1071)
classifier1=naiveBayes(Purchased~.,data = training_set)

#Predicting the test set results
y_pred=predict(classifier1,newdata = test_set[-4])

#Confusion Matrix
cm=table(y_pred,test_set[,4])

getwd()
source("accuracy.R")
acc(cm)
