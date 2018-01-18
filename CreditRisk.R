getwd()

#read and view the data
cr=read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv",na.strings = c(""," ","NA"))#see this line
View(cr)

#EDA
summary(cr)
colSums(is.na(cr))

#Remove the unique variables
cr$Loan_ID=NULL

source("mode.R")


#Imputation
mode(cr$Credit_History)
cr$LoanAmount[is.na(cr$LoanAmount)]=mean(cr$LoanAmount,na.rm = TRUE)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]=median(cr$Loan_Amount_Term,na.rm = TRUE)
cr$Credit_History[is.na(cr$Credit_History)]=mode(cr$Credit_History)
cr$Self_Employed[is.na(cr$Self_Employed)]=mode(cr$Self_Employed)
cr$Gender[is.na(cr$Gender)]=mode(cr$Gender)
cr$Dependents[is.na(cr$Dependents)]=mode(cr$Dependents)
cr$Married[is.na(cr$Married)]=mode(cr$Married)


#Convert all the categorical variables to factor
cr$Credit_History=as.factor(cr$Credit_History)
class(cr$Credit_History)



#1.LOGISTIC REGRESSION

#model 1
#Its already splitted,so no need of splitting
model1=glm(Loan_Status~.,family = binomial(link = 'logit'),data=cr)
summary(model1)

#model 2()
model2=glm(Loan_Status~Married+Credit_History+Property_Area,
           family = binomial(link = 'logit'),data=cr)
summary(model2)


#2.DECISION TREE
# Fitting Decision Tree Classification to the Training set
library(rpart)
classifierdecision=rpart(Loan_Status~.,data=cr)
summary(classifierdecision)


#3.SVM 
#building SVM model
library(e1071)
classifierL<-svm(formula=Loan_Status~.,data=cr,type='C-classification',kernel='linear')

classifierR<-svm(formula=Loan_Status~.,data=cr,type='C-classification',kernel='radial')

classifierS<-svm(formula=Loan_Status~.,data=cr,type='C-classification',kernel='sigmoid')

classifierP<-svm(formula=Loan_Status~.,data=cr,type='C-classification',kernel='polynomial')


#4.naiveBayes
library(class)
classifierN=naiveBayes(formula=Loan_Status~.,data=cr)

#5.Credit Risk
# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500)


#read the validate dataset
cr_v=read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings = c(""," ","NA"))




#EDA
#summary(cr_v)
colSums(is.na(cr_v))

#cr_v$Loan_Status<-ifelse(cr_v$Loan_Status=="Y",1,0)
cr_v$LoanAmount[is.na(cr_v$LoanAmount)] <- mean(cr_v$LoanAmount,na.rm=T)
cr_v$Loan_Amount_Term[is.na(cr_v$Loan_Amount_Term)] <- mean(cr_v$Loan_Amount_Term,na.rm=T)
cr_v$Credit_History[is.na(cr_v$Credit_History)]<-mode(cr_v$Credit_History)
cr_v$Self_Employed[is.na(cr_v$Self_Employed)]<-mode(cr_v$Self_Employed)
cr_v$Dependents[is.na(cr_v$Dependents)]<-mode(cr_v$Dependents)
cr_v$Gender[is.na(cr_v$Gender)]<-mode(cr_v$Gender)
cr_v$Married[is.na(cr_v$Married)]=mode(cr_v$Married)

#similarly impute for gender, married, dependents, self employed
cr_v<-cr_v[,-1]
cr_v$Credit_History<-as.factor(cr_v$Credit_History)

View(cr_v)
# Validating using the validation set for logistic regression
pred_log=predict(model2,newdata = cr_v,type = 'response')
pred_log=ifelse(pred_log>=0.5,1,0)


source("accuracy.R")
#confusion matrix
cf1<-table(pred_log,cr_v$Loan_Status)
acc(cf1)


library(ROCR)

#plotting auc curve
pr<-prediction(pred_log,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)



#decision tree
predD = predict(classifierdecision, newdata = cr_v, type = 'class')
predD<-ifelse(predD=="Y",1,0)

#confusion matrix and accuracy for decision tree
CFD= table(predD,cr_v$Loan_Status)
acc(CFD)

#plot of decision tree
plot(classifierdecision,uniform=TRUE,cex=0.8)
text(classifierdecision, use.n=TRUE, all=TRUE)

#plotting auc curve for decision tree
pr<-prediction(predD,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for decision tree
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)


#SVM
#confusion matrix and accuracy for SVM
#LINEAR
predL = predict(classifierL, newdata = cr_v)
predL<-ifelse(predL=="Y",1,0)
cr_v$Loan_Status=ifelse(cr_v$Loan_Status=="Y",1,0)


CFL<-table(predL,cr_v$Loan_Status)
acc(CFL)

#plotting auc curve for linear
pr<-prediction(predL,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1


#RADIAL
predR = predict(classifierR, newdata = cr_v)
predR<-ifelse(predR=="Y",1,0)
#cr_v$Loan_Status=ifelse(cr_v$Loan_Status=="Y",1,0)


CFR<-table(predR,cr_v$Loan_Status)
acc(CFR)

#plotting auc curve for linear
pr<-prediction(predR,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)



#SIGMOID
predS = predict(classifierS, newdata = cr_v)
predS<-ifelse(predS=="Y",1,0)
#cr_v$Loan_Status=ifelse(cr_v$Loan_Status=="Y",1,0)


CFS<-table(predS,cr_v$Loan_Status)
acc(CFS)

#plotting auc curve for linear
pr<-prediction(predS,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)


#POLYNOMIAL
predP = predict(classifierP, newdata = cr_v)
predP<-ifelse(predP=="Y",1,0)
#cr_v$Loan_Status=ifelse(cr_v$Loan_Status=="Y",1,0)


CFP<-table(predP,cr_v$Loan_Status)
acc(CFP)

#plotting auc curve for linear
pr<-prediction(predP,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)
# 
#  #KNN
# library(class)
# cr$Loan_Status=ifelse(cr$Loan_Status=="Y",1,0)
#  predK=knn(train=cr[,-12],test=cr_v[,-12],cl=cr$Loan_Status,k=20,prob = TRUE)
# 

#Validation for Naive bayes
#Predicting the test set results
y_pred=predict(classifierN,newdata = cr_v[,-12])
y_pred=ifelse(y_pred=="Y",1,0)


#Confusion Matrix
cm=table(y_pred,cr_v$Loan_Status)
acc(cm)

#plotting auc curve for linear
pr<-prediction(y_pred,cr_v$Loan_Status)
pr
prf<-performance(pr,measure = "tpr",x.measure = "fpr")

#obtaining area under ROC curve for linear
auc<-performance(pr,measure = "auc")
auc
auc1<-auc@y.values[[1]]
auc1

plot(prf)


#For Random Forest
pred_random=randomForest(x=cr[-12],
                         y=cr$Loan_Status,
                         ntree = 50)



#So after comparing all the algorithms behaviour, we can choose decision tree classifier as the optimal classifier.
cr_test=read.csv("R_Module_Day_8.1_Credit_Risk_Test_data.csv",na.strings = c(""," ","NA"))
View(cr_test)


#EDA
summary(cr_test)
colSums(is.na(cr_test))

#Remove the unique variables
cr_test$Loan_ID=NULL

source("mode.R")


#Imputation
mode(cr$Credit_History)
cr_test$LoanAmount[is.na(cr_test$LoanAmount)]=mean(cr_test$LoanAmount,na.rm = TRUE)
cr_test$Loan_Amount_Term[is.na(cr_test$Loan_Amount_Term)]=median(cr_test$Loan_Amount_Term,na.rm = TRUE)
cr_test$Credit_History[is.na(cr_test$Credit_History)]=mode(cr_test$Credit_History)
cr_test$Self_Employed[is.na(cr_test$Self_Employed)]=mode(cr_test$Self_Employed)
cr_test$Gender[is.na(cr_test$Gender)]=mode(cr_test$Gender)
cr_test$Dependents[is.na(cr_test$Dependents)]=mode(cr_test$Dependents)
cr_test$Married[is.na(cr_test$Married)]=mode(cr_test$Married)


#Convert all the categorical variables to factor
cr_test$Credit_History=as.factor(cr_test$Credit_History)
class(cr_test$Credit_History)

#Predicting the test set results.
y_predtest=predict(classifierdecision,newdata = cr_test,type = 'class')

#Adding the predicted results to the data frame.
cr_test=data.frame(cr_test,y_predtest)
