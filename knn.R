#knn

#Importing the dataset
dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[3:5]

class(dataset$Purchased)

#Encoding the target feature as factor
dataset$Purchased=factor(dataset$Purchased,levels = c(0,1))

colSums(is.na(dataset))

#Feature Scaling
dataset[,c(1,2)]=scale(dataset[,c(1,2)])


#Splitting the dataset into test and train
library(caTools)
set.seed(24)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Fitting a model
library(class)


#Predicting the test set results
y_pred=knn(train = training_set[,-3],test = test_set[,-3],cl=training_set$Purchased,k=20,prob = TRUE)


#Confusion Matrix
cm=table(y_pred,test_set[,3])

getwd()
source("accuracy.R")
acc(cm)
