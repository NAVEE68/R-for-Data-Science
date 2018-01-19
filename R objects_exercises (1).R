#create a vector using sequencee
x<-5:9
a<-c(1,2,3,4)
#sequence
c<-seq(1,10,2)
b<-1:2
#vector multiplication
a*b
#Access a particular element of a vector
a[1]
#excluding an element in a vector
a[-1]
#logical operators
a!=2
sum(a>3)
#repeated sequence
x<-rep(6,4)
#character vector
apple<-c("red","green")
#Matrices
y<-matrix(c(1,2,3,4))
y
#class of an R object
class(y)
#dimension
dim(y)
#naming the rows and columns of matrix
te<-c("T1","T2","t3","t4")
rownames(y)<-te
#colnames
suns<-c("maths")
colnames(y)<-suns
y
#Mean, sum in a matrix
X<-matrix(c(50,70,40,90,60, 80,50, 90,100, 50,30, 70),nrow=3)
rowSums(X)
rowMeans(X)
#Adding a row or column in a matrix
X<-rbind(X,apply(X,2,mean))
X<-cbind(X,apply(X,1,var))
colnames(X)<-c(1:4,"variance")
rownames(X)<-c(1:3,"mean")
#Arrays
#list
x <- list(u=2, v="abc")
x
j<-list(name="Joe", sal=7000, union=T)
#Adding to List
z <- list(a="abcd",b=100)
z$c <- " r-project" # add a c component
#delete a list component by setting it to NULL.
z$b<-NULL
#dataframe
kids <- c("John", "Joe", "Cloe")
ages <- c(12,10,8)
d <- data.frame(kids,ages,stringsAsFactors=FALSE)
d # matrix-like view
#accessing data frames
d[[2]]
d$kids
d[,1]
#other fucntion
str(d)
names(d)
#accession subdata
d[2:3,] 
d[2:3, 2] 
d[d$ages >= 10,] 
#factors
x <- c(1:3)
 xfac <- factor(x)
 xfac
#using tapply()
ages <- c(25,26,55,37,21,42)
 location <- c("Rural","Urban","Urban","Rural","Urban","Rural")
                 tapply(ages,location,sum)
                
