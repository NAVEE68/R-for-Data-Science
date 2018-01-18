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


# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(logLit~Age+Years.of.Experience+Gender+Married,Train1,method="anova")
summary(regressor)
# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor,Test1)

# Visualising the Decision Tree Regression results 

# Plotting the tree
plot(regressor,uniform=TRUE,margin=0.2)
text(regressor,use.n=TRUE,all=TRUE,cex=0.8)

RMSETest1 <- rmse(Test1$logLit,y_pred)
RMSETest1


regressor2 = rpart(logLit~Years.of.Experience+Gender+Married,Train1,method="anova")

# Predicting a new result with Decision Tree Regression
y_pred2 = predict(regressor2,Test1)

# Visualising the Decision Tree Regression results 

# Plotting the tree
plot(regressor2,uniform=TRUE,margin=0.2)
text(regressor2,use.n=TRUE,all=TRUE,cex=0.8)

RMSETest2 <- rmse(Test1$logLit,y_pred2)
RMSETest2

