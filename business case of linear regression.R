library(car)
library(psych) #(for describing)
library(caTools) #(for splitting)
library(hydroGOF) #(rmse)
library(MASS)  #(stepAIC)

#Withot splitting
#get the working directory.
getwd()

#Read the data into a object.
data2<-read.csv("C:/Users/Naveen/Desktop/Rbasics/LoanDefault.csv")

#View the data
View(data2)
summary(data2)


#Remove the unique column.
data2$Ac_No<-NULL

#To get the no of rows and columns.
dim(data2)

#To find the sums of missing values in each column.
colSums(is.na(data2))


#To check if the dependent variable is normally distributed.
hist(data2$Losses.in.Thousands)
#Since the distribution is positively skewed, we need to transform the data.
boxplot(data2$Losses.in.Thousands)
LGD<-log2(data2$Losses.in.Thousands)
LGD
boxplot(LGD)

#Checking the relationship between dependent and independent variables.
    #correlation between age and LGD
cor(LGD,data2$Age)
cor.test(LGD,data2$Age)
#correlation is significant for above pair(i.e,p-value is < 0.05)

#correlation between years of exp and LGD
cor(LGD,data2$Years.of.Experience)
cor.test(LGD,data2$Years.of.Experience)
#correlation is significant for above pair(i.e,p-value is < 0.05)


#correlation between no of vehicles and LGD
cor(LGD,data2$Number.of.Vehicles)
cor.test(LGD,data2$Number.of.Vehicles)
#correlation is not significant for above pair(i.e,p-value is > 0.05)


#LGD vs Gender(t-test)
t.test(LGD~data2$Gender)
#test is significant for above pair(i.e,p-value is < 0.05)

#LGD vs Marital status(t-test)
t.test(LGD~data2$Married)
#test is significant for above pair(i.e,p-value is < 0.05)


#Checking for multicollinearity.
  #For age and yrs of exp.
cor(data2$Age,data2$Years.of.Experience)
cor.test(data2$Age,data2$Years.of.Experience)
#Age and yrs of exp are correlated and it is  significant.

#For age and gender.
t.test(data2$Age~data2$Gender)
#test is not significant

#For age and marital status.
t.test(data2$Age~data2$Married)
#test is not significant


#For years of exp and gender.
t.test(data2$Years.of.Experience~data2$Gender)
#test is not significant


#For years of exp and marital status.
t.test(data2$Years.of.Experience~data2$Married)
#test is not significant


#For gender and marital status.
chisq.test(data2$Gender,data2$Married)
#test is not significant


#Final model.
loan<-lm(LGD~data2$Age+data2$Gender+data2$Married)
summary(loan)

data2$Losses.in.Thousands<-NULL
View(data2)
data2<-data.frame(data2,LGD)
View(data2)



#Validation of the Model.
predloss<-predict(loan,newdata = data2)
predloss
rmsetest<- rmse(data2$LGD,predloss)
rmsetest



#2nd model
loan1<-lm(LGD~data2$Age+data2$Years.of.Experience+data2$Gender+data2$Married)
summary(loan1)


predloss1<-predict(loan1,newdata = data2)
predloss
rmsetest<- rmse(data2$LGD,predloss1)
rmsetest


#3rd model

loan2<-lm(LGD~.,data = data2)
summary(loan1)


predloss2<-predict(loan2,newdata = data2)
predloss
rmsetest<- rmse(data2$LGD,predloss2)
rmsetest




















