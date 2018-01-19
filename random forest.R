#Random Forest Classification

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[2:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-c(1,4)] = scale(training_set[-c(1,4)])
test_set[-c(1,4)] = scale(test_set[-c(1,4)])


training_set$Gender=as.factor(training_set$Gender)
test_set$Gender=as.factor(test_set$Gender)
levels(training_set$Gender)=1:2
levels(test_set$Gender)=1:2


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-4],
                          y = training_set$Purchased,
                          ntree = 10)

y_pred=predict(classifier,newdata = test_set[-4])


# Making the Confusion Matrix
cm = table( y_pred,test_set[,4])

source("accuracy.R")
acc(cm)


library(ROCR)
#plotting auc curve 
y_pred=as.numeric(y_pred)
pr<-prediction(y_pred,test_set[,4])
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)

