#1. Generalization
getwd()
setwd("C:/Users/aeoluseros/R-programming")
dir()
ls()  # browsing variables
#source("file.R") is used to access a file file.R

#2. R systems contains two parts:
#(1)Base part:base, utils, stats, datasets, graphics, grDevices, grid, methods, tools, parallel, compiler, splines, tcltk, stats4
#(2)Recommened part: boot, class, cluster, codetools, foreign, KernSmooth, lattice, mgcv, nlme, rpart, survival, MASS, spatial, nnet, Matrix

#3. R has 5 basic or "atomic" classes of objects: Characer, numeric(real numbers),integer, complex, logical(True/False) 
#The most basic object is a vector: can only contain objects of the same class
#"List" is an extension of vector that could contain objects of difference classes
vector()  # create empter vector, class is logical, length is 0.
#Numeric is doulbe percision real number
#If you want an integer, you need to specify the L suffix.
x<-4L
class(x)

x <- c(4, TRUE)
class(x)

x <- c(1,3, 5)
y <- c(3, 2, 10)
z <- rbind(x, y)
dim(z)
attributes(z)  #accessible attributes of an object
#special number: (1) Inf: infinity, e.g. 1/0. and 1/Inf = 0 (2) NaN: Not a number, e.g.0/0

#4.Creating Vectors and lists: c() function and vector() function, list() function
x<-c(0.5,0.6)
x<-c(0.5+1i,0.6+0i)
x<-c(0.5+1i,FALSE)  #Automatically convert FALSE to complex 0+0i
x<-vector("numeric",length=10)  # elements are 0's
x<-vector("list",length=10)  # and could be used to produce list.
x<-list(1,"a",TRUE,1+4i)
#set all elements of this vector that are less than 6 to be equal to zero:
x <- c(3, 5, 1, 10, 12, 6)
x[x < 6] <- 0

#5.Creating matrices:matrix() function
m<-matrix(nrow=2,ncol=3)   # elements are NA
dim(m)
attributes(m)
m<-matrix(1:6,nrow=2, ncol=3)  #fill the matrix vertically
#created from vectors by adding a dimension attribute
m<-1:10
dim(m)<-c(2,5)
m
#created by column-binding or row-binding with cbind() and rbind() --> most common
x<-1:3
y<-10:12
cbind(x,y) #3*2
rbind(x,y) #2*3
  
#6. factors: treat specially in modelling functions like lm() and glm()
x<-factor(c("yes","yes","no","yes","no"))
x  # first level is "no"
table(x)  #contingency table of the counts
unclass(x)  #unclass returns (a copy of) its argument with its class attribute removed
#unclass() is contrary to class()
#the order of the levels can be set using the "levels" arguement to a factor(). 
#This is important in linear modeling because the first level is used as the baseline level
x<-factor(c("yes","yes","no","yes","no"),levels=c("yes","no"))
x  # first level is "yes"



# read in data & subsetting
x <- read.csv("C:/Users/aeoluseros/DataScience/R_Prog_data/hw1_data.csv")
dim(x)
nrow(x)
head(x,n=2)
tail(x,n=2)
miss <- is.na(x[, "Ozone"])  ## A vector of TRUE/FALSE
sum(miss)
mean(x[,"Ozone"],na.rm=TRUE)  #mean(x, trim = 0, na.rm = FALSE, ...) na.rm=TRUE means getting rid of NA's
x_sub<-subset(x, Ozone > 31 & Temp > 90)  #subset(x,logical expression): Return subsets of vectors, matrices or data frames which meet conditions.
mean(x_sub[,"Solar.R"],na.rm=TRUE)
mean(subset(x,Month==6)[,"Temp"],na.rm=TRUE)
max(subset(x,Month==5)[,"Ozone"],na.rm=TRUE)



