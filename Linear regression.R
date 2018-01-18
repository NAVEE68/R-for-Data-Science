getwd()
install.packages('psych')
library(psych)
"hi"
getwd()
data1<-read.csv('risk.csv')
getOption("max.print")
#if the file is not in working directory
#read.csv(C:/Users/Naveen/Downloads/risk.csv)
setwd("C:/Users/Naveen/Desktop/Rbasics")
getwd()
View(data1)
summary(data1)
class(data1)
type(data1)
is.data.frame(data1)



#1.Convert the gender and wvcat into categorical variables(data processing/data cleaning)
data1$wvcat<-factor(data1$wvcat)
class(data1$subid)
data1$gender<-factor(data1$gender)
class(data1$gender)
summary(data1)
data1$active<-NULL
View(data1)
summary(data1)

class(data1[ ,1])
data1<-data1[,-1]
View(data1)
#Give no of rows and columns
dim(data1)
data.frame(ifelse(data1$risk>=40,"H","L"))
num<-1:1012
newrisk<-factor('h','l')
data1<-data.frame(num,data1)
#Adding a column based on risk
data1<-data.frame(newrisk,data1)
for (newrisk in data1$risk) {
  ifelse(data1$risk>=43.44,data1$newrisk='h',data1$newrisk='l'){}
  
}

table(data1$age)
summary(data1)


View(data1)
fac<-factor('m','f')
#1.a)missing values
summary(data1)
sum(is.na(data1))
colSums(is.na(data1))
data1$fac<-NULL
View(data1)
#Imputation(changing the na's with the mean of risk column)
data1$risk[is.na(data1$risk)]<-mean(data1$risk,na.rm = T)


#Imputation(changing the na's with the mean of age column)
data1$age[is.na(data1$age)]<-mean(data1$age,na.rm = T)
summary(data1)

#Univariate analysis
meanage<-mean(data1$age)
meanage
median(data1$age)
mode(data1$age)
#for categorizing and finding frequencies
table(data1$gender)
sd(data1$age)
max(table(data1$wvcat))
var(data1$age)
describe(data1)

calculatedMode<-modecalc(temp){
  
}
which.max(table(data1$gender))

#Bivariate
var(age)
help(t.test)
#Qualitative vs Quantitative



a<-c(2,3,2,5,2,8)
unique(a)
t.test(data1$age~data1$gender)


#2 Categories:Anova
ftest<-aov(data1$age~data1$wvcat)
summary(ftest)



#Correlation:-Quantitative vs Quantitative
cor(data1$age,data1$risk)
cor.test(data1$age,data1$risk)



#Qualitative vs Qualitative (Chi Square)
chisq.test(data1$gender,data1$wvcat)



#Graphs

#Histogram
hist(data1$age)

#Scatter plot
plot(data1$risk,data1$age)
View(data1)


#Boxplot
boxplot(data1[,c(2,4)])



#Prediction:Linear Regression
#Simple linear regression

#Risk vs Age

slr<-lm(data1$risk~data1$age)
slr
summary(slr)



#Multiple Linear regression
LR<-lm(risk~gender+age+wvcat,data=data1)
summary(LR)


























