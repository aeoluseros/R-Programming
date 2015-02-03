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

#4.




