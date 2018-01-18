#Decision tree classification

#Read data
CR <- read.csv('D:/IMARTICUS/R/dataset/R_Module_Day_7.2_Credit_Risk_Train_data.csv',na.strings = c(""," ","NA"))
View(CR)
attach(CR)

#EDA
#EDA:Remove irrelevant variables
CR$Loan_ID=NULL

#Identify missing values using colSums 
summary(CR)
sum(is.na(CR))
colSums(is.na(CR))

#imputation
#Replace it with mean/median/mode

source('am.R')


CR$Gender[is.na(CR$Gender)]<- mode(CR$Gender)


CR$Married[is.na(CR$Married)]<-mode( CR$Married)
CR$Dependents[is.na(CR$Dependents)]<- mode(CR$Dependents)
CR$Self_Employed[is.na(CR$Self_Employed)]<- mode(CR$Self_Employed)
CR$LoanAmount[is.na(CR$LoanAmount)]<- median(CR$LoanAmount,na.rm = T)
CR$Loan_Amount_Term[is.na(CR$Loan_Amount_Term)]<- median(CR$Loan_Amount_Term,na.rm = T)
CR$Credit_History[is.na(CR$Credit_History)]<- mode(CR$Credit_History)

#covert CR$Credit_History to factor
CR$Credit_History<-as.factor(CR$Credit_History)
class(CR$Credit_History)

library(rpart)
classifier = rpart(Loan_Status~.,data=CR)

cr_v <- read.csv('D:/IMARTICUS/R/dataset/R_Module_Day_8.2_Credit_Risk_Validate_data.csv',na.strings = c(""," ","NA"))
#check for data issues with testa nd clean it for the same
colSums(is.na(cr_v))
#cr_v$Loan_Status<-ifelse(cr_v$Loan_Status=="Y",1,0)
cr_v$LoanAmount[is.na(cr_v$LoanAmount)] <- mean(cr_v$LoanAmount,na.rm=T)
cr_v$Loan_Amount_Term[is.na(cr_v$Loan_Amount_Term)] <- mean(cr_v$Loan_Amount_Term,na.rm=T)
cr_v$Credit_History[is.na(cr_v$Credit_History)]<-mode(cr_v$Credit_History)
cr_v$Self_Employed[is.na(cr_v$Self_Employed)]<-mode(cr_v$Self_Employed)
cr_v$Dependents[is.na(cr_v$Dependents)]<-mode(cr_v$Dependents)
cr_v$Gender[is.na(cr_v$Gender)]<-mode(cr_v$Gender)

cr_v$Loan_ID=NULL
cr_v$Credit_History<-as.factor(cr_v$Credit_History)

                   
help(rpart)
# Validating using the validation set 
fitted.results1 <- predict(classifier,newdata=cr_v,type='class')
#fitted.results1 <- ifelse(fitted.results1 >=0.5,1,0)


#Confusion matrix
cf1<-table(fitted.results1,cr_v$Loan_Status)
cf1
#function for accuracy
acc<-function(cf1){
  Totp<-cf1[2,1]+cf1[2,2]
  TP<-cf1[2,2]
  c<-TP/Totp
  c
}
acc(cf1)

library(ROCR)
cr_v$Loan_Status<-ifelse(cr_v$Loan_Status=="Y",1,0)
fitted.results1 <- ifelse(fitted.results1=="Y",1,0)

pr1<- prediction(fitted.results1,cr_v$Loan_Status)
length(cr_v$Loan_Status)
prf1<- performance(pr1,measure = "tpr",x.measure = "fpr")
plot(prf1)

#obtaining area under ROC curve
auc<-performance(pr1,measure = "auc")
auc1<- auc@y.values[[1]]
auc1

#prediction for the prediction set
plot(classifier,uniform=TRUE,cex=0.8)
text(classifier, use.n=TRUE, all=TRUE)

cr_test1 <- read.csv('D:/IMARTICUS/R/dataset/R_Module_Day_8.1_Credit_Risk_Test_data.csv',na.strings = c(""," ","NA"))

cr_test1$Credit_History<-as.factor(cr_test1$Credit_History)

fr_p1<- predict(classifier,newdata = cr_test1,type ='class')
#fr_p <- ifelse(fitted.results1 >=0.5,1,0)


cr_test1<-data.frame(cr_test1,fr_p1)

View(cr_test1)


