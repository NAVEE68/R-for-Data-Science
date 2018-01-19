library(psych) #describe

library(hydroGOF) #rmsc test

LGD <- read.csv('LoanDefault.csv')

View(LGD)
class(LGD)
# 1.a data processing/data cleaning
class(LGD$Gender)
class(LGD$Married)

LGD$Ac_No=NULL
summary(LGD)

sum(is.na(LGD))

boxplot(LGD$Losses.in.Thousands)
logLit=log2(LGD$Losses.in.Thousands)
boxplot(logLit)

LGD <- data.frame(LGD,logLit)

#remove $Losses.in.Thousands from dataset has we have included logLit

LGD$Losses.in.Thousands=NULL


#spliting the data to train and test

library(caTools) #splitting
set.seed(123)
split=sample.split(LGD$logLit,0.70)
Train1=subset(LGD,split==T)
dim(Train1)
View(Train1)

Test1=subset(LGD,split==F)        
dim(Test1)
View(Test1)

#Building model

cor.test(Train1$Age,Train1$logLit) #significant
cor.test(Train1$logLit,Train1$Years.of.Experience)  #significant
cor.test(Train1$logLit,Train1$Number.of.Vehicles)  #not significant
t.test(Train1$logLit~Train1$Gender,var.equal=T)    #significant
t.test(Train1$logLit~Train1$Married,var.equal=T)   #significant

# remove Number.of.Vehicles

Train1$Number.of.Vehicles=NULL


#multicollinarity


cor.test(Train1$Age,Train1$Years.of.Experience) # significant
t.test(Train1$Age~Train1$Gender,var.equal=T)    # not significant
t.test(Train1$Age~Train1$Married,var.equal=T)    # not significant


t.test(Train1$Years.of.Experience~Train1$Gender,var.equal=T) # not significant

t.test(Train1$Years.of.Experience~Train1$Married,var.equal=T)# not significant

chisq.test(Train1$Gender,Train1$Married)      # not significant

#Remove either of age or Years.of.Experience
# remove either of gender or married

#multiple liner regreesion
#model 1
model1 <- lm(logLit~Age+Years.of.Experience+Gender+Married,Train1)
summary(model1)

#PredictiOn
predloss1 <-predict(model1,newdata=Test1)

RMSETest1 <- rmse(Test1$logLit,predloss1)
RMSETest1

#model 2
model2 <- lm(logLit~Years.of.Experience+Gender+Married,Train1)

summary(model2)

#PredictiOn
predloss2 <-predict(model2,newdata=Test1)

RMSETest2 <- rmse(Test1$logLit,predloss2)
RMSETest2

#model 3
model3 <- lm(logLit~Age+Gender+Married,Train1)
summary(model3)

#PredictiOn
predloss3 <-predict(model3,newdata=Test1)

RMSETest3 <- rmse(Test1$logLit,predloss3)
RMSETest3

#model 4
model4 <- lm(logLit~Years.of.Experience+Married,Train1)
summary(model4)

#PredictiOn
predloss4 <-predict(model4,newdata=Test1)

RMSETest4 <- rmse(Test1$logLit,predloss4)
RMSETest4

#model 5
model5 <- lm(logLit~Years.of.Experience+Gender,Train1)
summary(model5)

#PredictiOn
predloss5 <-predict(model5,newdata=Test1)

RMSETest5 <- rmse(Test1$logLit,predloss5)
RMSETest5

#model 6
model6 <- lm(logLit~Age+Married,Train1)
summary(model6)

#PredictiOn
predloss6 <-predict(model6,newdata=Test1)

RMSETest6 <- rmse(Test1$logLit,predloss6)
RMSETest6

#model 7
model7 <- lm(logLit~Age+Gender,Train1)
summary(model7)

#PredictiOn
predloss7 <-predict(model7,newdata=Test1)
predloss7
RMSETest7 <- rmse(Test1$logLit,predloss7)
RMSETest7



#RANDOM FOREST
library(randomForest)
model_LD_RF<- randomForest(x=Train1[,-5],
                           y=Train1[,5],
                           ntree = 200)
predloss_RF<-predict(model_LD_RF,newdata = Test1)

RMSE_RF<-rmse(Test1$logLit,predloss_RF)
RMSE_RF

#Decision tree
library(rpart)
model_LD_DT = rpart(formula = logLit ~ .,data = Train1,method = "anova")

predloss_DT<-predict(model_LD_DT,newdata = Test1)

RMSE_DT<-rmse(Test1$logLit,predloss_DT)
RMSE_DT

#svm for linear
library(e1071)
model_LD_SVM<- svm(formula=logLit ~ .,
                   data=Train1,
                   type='eps-regression',
                   kernel='linear')
predloss_SVM<-predict(model_LD_SVM,newdata = Test1)

RMSE_SVM<-rmse(Test1$logLit,predloss_SVM)
RMSE_SVM


#svm for sigmoid
model_LD_SVMS<- svm(formula=logLit ~ .,
                    data=Train1,
                    type='eps-regression',
                    kernel='sigmoid')
predloss_SVMS<-predict(model_LD_SVMS,newdata = Test1)

RMSE_SVMS<-rmse(Test1$logLit,predloss_SVMS)
RMSE_SVMS

#SVM for Polynomial
model_LD_SVMP<- svm(formula=logLit ~ .,
                    data=Train1,
                    type='eps-regression',
                    kernel='polynomial')
predloss_SVMP<-predict(model_LD_SVMP,newdata = Test1)

RMSE_SVMP<-rmse(Test1$logLit,predloss_SVMP)
RMSE_SVMP

#SVM for radial
model_LD_SVMR<- svm(formula=logLit ~ .,
                    data=Train1,
                    type='eps-regression',
                    kernel='linear')
predloss_SVMR<-predict(model_LD_SVMR,newdata = Test1)

RMSE_SVMR<-rmse(Test1$logLit,predloss_SVMR)
RMSE_SVMR

# sd