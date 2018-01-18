#ANN
data=read.csv("Churn_Modelling.csv")
colSums(is.na(data))
data=data[4:14]


#Convert into numeric as every factor variable should be numeric in ANN
data$Geography=as.numeric(factor(data$Geography))
data$Gender=as.numeric(factor(data$Gender))


#Feature Scaling
data[,1:10]=scale(data[,1:10])



#install.packages("neuralnet)
library(neuralnet)
n=names(data)

#For formula
f=as.formula(paste("Exited~",paste(n[!n%in% "Exited"],collapse = "+")))

#model
nn=neuralnet(f,data = data,linear.output = FALSE,hidden = 2)
plot(nn)
