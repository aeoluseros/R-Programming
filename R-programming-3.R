setwd("D:/study/DataScience")
#1.loop functions: 
#lapply, sapply, tapply, mapply
#lapply(X, FUN, ...), Loop over a list and evaluate a function on each element  
#sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE), Same as lapply but try to simplify the result 
        #if the result is a list where every element is length 1, then a vector is returned.
        #if the result is a list where every element is of the same length, then a matrix is returned
        #if it can't figure things out, a list is returned.
#apply(X, MARGIN, FUN, ...), Apply a function over the margins of an array or matrix
        #apply is not faster than loop, but less typing
str(apply)

x<-list(a=1:5,b=rnorm(10),c=rnorm(20,1),d=rnorm(100,5)) #change the means of c,d to 1 and 5
lapply(x,mean) 
sapply(x,mean)
class(sapply(x,mean))

x<-1:4
lapply(x,runif)
lapply(x,runif,min=0,max=10)  #this is the way how we specify args of function

#these loop functions make heavy use of anonymous functions
x<-list(a=matrix(1:4,2,2),b=matrix(1:6,3,2))
x
lapply(x,function(elt) elt[,1])  #extract the first column

x<-matrix(rnorm(200),20,10) #20 rows and 10 columns
apply(x,2,mean)  #apply the mean function on the second dimension-column, will return 10 values
apply(x,1,sum)

#the following four functions are much faster than apply function. we could directly use them
#rowSums: apply(x,1,sum)
#rowMeans: apply(x,1,mean)
#colSums: apply(x,2,sum)
#colMeans: apply(x,2,mean)

x<-matrix(rnorm(200),20,10)
apply(x,1,quantile,probs=c(0.25,0.75))  #apply quantile function on the 1st dimension -- rows, so 20 elements
#quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE,names = TRUE, type = 7, ...)

a<-array(rnorm(2*2*10),c(2,2,10))  #10 2*2 matrices
a[,2,] #returns the second column of every small matrices
a[2,2,] #returns the fourth element of each samll matrices
class(a)  #array
apply(a,c(1,2),mean) #the mean of the first column and the second column  
                     #keep the 1st and 2nd dimensions but collapse the 3rd dimension
mean(a[,,1]) == apply(a,3,mean)[1]   #TRUE
rowMeans(a,dims=2) #dims=2, and we take "row mean", then mean is over the 3rd dimension. 
rowMeans(a,dims=1) #dims=1, and we take "row mean", then mean is over the 2nd and 3rd dimension. 
apply(a,1,mean) == rowMeans(a,dims=1)  #we take mean to the 1st margin of a.
apply(a,2,mean) #we take mean to the 2nd margin of a.
apply(a,3,mean) #we take mean to the 3rd margin of a.


#mapply(multivariate version of lapply): it applied a function in parallel over a set of arguments
str(mapply)
#function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE) 
#FUN is the function to apply; ... contains arguments/objects to apply over, MoreArgs is a list of other arguments to FUN
#lapply, sapply, tapply only apply a function over a single object. for example, the input to lapply is a single list
#if we have two lists, we could use mapply.
list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))  #tedious, use the below one
mapply(rep,1:4,4:1) 

vrep <- Vectorize(rep.int)  #equivalent
vrep(1:4, 4:1)

#vectorizing a function
noise<-function(n,mean,sd){
        rnorm(n,mean,sd)
}
noise(5,1,2)
noise(1:5,1:5,2)  
mapply(noise,1:5,1:5,2) #Vectorization: one random number with mean 1, two random number with mean 2, three random number with mean 3, etc.
list(noise(1,1,2),noise(2,2,2),noise(3,3,2),noise(4,4,2),noise(5,5,2))

#notes: the following example could demonstrate the effect of using vector in rnorm
a = c(0, 10, 100)
y = rnorm(a, mean=a, sd=1) #Swe generate length(a) random numbers with mean a[i].
y #-0.7325153 10.4808952 99.0105497, therefore, the mean is separately (0,10,100)


#tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE): Apply a function over subsets of a vector 
        #X is a vector, INDEX is a factor or a list of factors
        #Apply a function to each cell of a ragged array, that is to each (non-empty) group of 
        #values given by a unique combination of the levels of certain factors.
x<-c(rnorm(10),runif(10),rnorm(10,1))
f<-gl(3,10) #gl: Generate factors by specifying the pattern of their levels.
str(gl) #function (n, k, length = n * k, labels = seq_len(n), ordered = FALSE) 
# n: an integer giving the number of levels.; k: an integer giving the number of replications.
f
tapply(x,f,mean)
tapply(x,f,range)

#a auxiliary function "split" is also useful, particularly in conjunction with lapply
str(split) #function (x, f, drop = FALSE, ...) 
#"str" takes a vector/list/data frame and splits it into groups determined by a factor or list of factors
x<-c(rnorm(10),runif(10),rnorm(10,1))
f<-gl(3,10)
split(x,f)

#a common idiom is split followed by an lapply:
lapply(split(x,f),mean)

#split data.frames
library(datasets)
head(airquality)
#split the data frame into monthly pieces
s<-split(airquality,airquality$Month)  #elements in the list is a "list".
lapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
sapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
lapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm=TRUE))

#split on more than one level
x<-rnorm(10)
f1<-gl(2,5)
f2<-gl(5,2)
f1
f2
#interaction computes a factor which represents the interaction of the given factors. 
interaction(f1,f2) #get total 10 levels
split(x,list(f1,f2)) #some of levels have nothing in them.
str(split(x,list(f1,f2)))  #str: Compactly display the internal structure of an R object
#remove empty levels before using lapply/sapply
str(split(x,list(f1,f2),drop=TRUE))

#2.debugging - basic
options(warn=2) #If warn is two or larger all warnings are turned into errors.
options(warn=1) #If warn is one, warnings are printed as they occur.
options(warn=0) #default

printmessage<-function(x){
        if(x>0) print("x is greater than zero")
        else print("x is less than or equal to zero")
        invisible(x) #invisible is a function that prevents auto priting.   
                     #if I at command line and type a function, the function will return
                     #the last element of the function body
}
printmessage(1)
#printmessage(NA)   #if condition would get error for NA.
#fix:
printmessage2<-function(x){
        if(is.na(x)) print("x is a missing value")
        else if(x>0) print("x is greater than zero")
        else print("x is less than or equal to zero")
        invisible(x) #invisible is a function that prevents auto priting.   
        #if I at command line and type a function, the function will return
        #the last element of the function body
}
printmessage2(NA) 
x<-log(-1)
printmessage2(x)  #NaN is an NA.


#3.debugging - tools
#traceback: prints out the function call stack after an error occurs; does nothing is no error 
#debug: flags a function for "debug" mode: step through execution of a function one line at a time
#browser: suspends the execution of a function wherever it is called and puts the function in debug mode
        #you could call browser function anywhere
#trace: allows you to insert debugging code into a function without editing the function itself
        #this is handy if you are debugging someone else's code. For example, there's code in the package, which you cannot easily edit
#recover: allows you to modify the error behavior so that you can browse the function call stack

rm(list=ls())
mean(x)
traceback() #will show the error occured at mean(x)
#many times error will occur at the top level function(functions that call a lot of lower-level functions). In that case, traceback is not very handy.
#you have to call traceback immediate after error occurs
lm(y~x)
traceback() #7: eval(expr, envir, enclos) --> error happens in "expr"
debug(lm)
y<-airquality[,1]
x<-airquality[,2]
yxlm<-lm(y~x) #will enter to lm's function environment, use "Enter" in console to step through the function
undebug(lm)
#for lower-level functions, we could bind them with debug() first

#for "recover", you set error = recover (that will set a global option) so as to modify the error behavior
options(error=recover)
read.csv("nosuchfile")
options("show.error.messages")

#browser:
SS <- function(mu, x) {
        d <- x - mu
        d2 <- d^2
        browser()
        ss <- sum(d2)
        ss
}
SS(2, x)


#3. str function -- most important function
#similar to summary. It's compactly display "summary".
str(str)
str(ls)
x<-rnorm(100,2,4)
summary(x)
str(x)  #gives the first five numbers
f<-gl(40,10)
str(f) #we could see the first couple factors are all have the label, one.
summary(f)

head(airquality)
str(airquality)
summary(airquality)

m<-matrix(rnorm(100),10,10)
str(m)
summary(m)

s<-split(airquality,airquality$Month)
str(s)
summary(s)


#4.simulation
#rnorm: random normal variable
#dnorm: evaluate the Normal probability density (with a given mean/SD) at a point (or vector of points)
#pnorm: evaluate the cumulative distribution function for a Normal distribution
#rpois: random poisson variable
#rbinom: random binomial variable
?distributions
#d for density, p for cumulative
set.seed(1)
rnorm(5)

#The Poisson distribution can also be used for the number of events in other specified i
#ntervals such as distance, area or volume.
rpois(10,2) #with rate of 1
rpois(10,7) #with rate of 2
ppois(2,2)  #cumulative distribution, Pr(x<=2)
ppois(4,2)  #Pr(x<=4)

#simulate random number form a linear model
set.seed(20)
x<-rnorm(100)
e<-rnorm(100,0,2)
y<-0.5+2*x+e
summary(y)
plot(x,y)

set.seed(10)
x<-rbinom(100,1,0.5)
e<-rnorm(100,0,2)
y<-0.5+2*x+e
summary(y)
plot(x,y)

#generate random numbers from a generalized linear model
set.seed(1)
x<-rnorm(100)
log.mu<-0.5+0.3*x
y<-rpois(100,exp(log.mu))  #rpois(n, lambda), log(E[Y|x])=alpha * beta'x ==> E[Y|x]=exp(alpha * beta'x), Y~poisson(exp(alpha * beta'x))
summary(y)
plot(x,y)

#random sampling 
set.seed(1) #setting sampling order
sample(1:10,4) #without replacement
sample(1:10,4,replace=TRUE) #with replacement
sample(1:10,replace=TRUE) 
sample(letters,5)  #Built-in Constants: letters/LETTERS/month.abb/month.name/pi
sample(1:10)  #permutation = sampling the whole set without replacement

#5.R Profiler @ performance analysis
#using system.time(expr) or system.time({expr}), returns an object of class proc_time    #distinguish from Sys.time() / Sys.Date()
#user time: CPU time. elapsed time: "wall clock" time
#elaspsed time is smaller than the user time if your machine has multiple cores/processors.
        #-multithreaded BLAS(Basic Linear Algebra, subroutines) libraries (MAC-vecLib/Accelerate, general-ATLAS, AMD machine-ACML, Intel Machine-MKL)
        #-Parallel processing via the parallel package
system.time(readLines("http://jhsph.edu"))
hilbert<-function(n){
        i<-1:n
        outer(i-1,i,"+")
        #1/outer(i-1,i,"+")  #The outer operaion(default:product) of the arrays X and Y is the 
                            #array A with dimension c(dim(X), dim(Y)) where element 
                            #A[c(arrayindex.x, arrayindex.y)] = FUN(X[arrayindex.x], Y[arrayindex.y], ...).
}
x<-hilbert(1000) # result is 1000*1000
hilbert(2) #dim(x) = dim(c(0,1)),dim(y) = dim(c(1,2)), so result is 2*2.
system.time(svd(x))

system.time({
        n<-1000
        r<-numeric(n)
        for(i in 1:n){
                x<-rnorm(n)
                r[i]<-mean(x)
        }        
})

###what if we don't know where to start to measure the performance
#Rprof(), must be compiled with profiler support. It keeps track of functions, default sampling interval is 0.02s.
        #if your code runs very quickly, the profiler is not very useful.
#summaryRprof() makes the output from Rprof() readable (tabulate)
        #two methods for normalizing the data: "by.total" "by.self"
#do not use system.time() and Rprof together or you will be sad

#comparison of four functions:
#Consider the problem of removing rows from a data frame where any value in the row has an NA, i.e. there is any incomplete data.
#first function
funAgg = function(x) {
        # initialize res 
        res = NULL
        n = nrow(x)        
        for (i in 1:n) {
                if (!any(is.na(x[i,]))) res = rbind(res, x[i,])
        }
        res
}
#second function
funLoop = function(x) {
        # Initialize res with x
        res = x
        n = nrow(x)
        k = 1        
        for (i in 1:n) {
                if (!any(is.na(x[i,]))) {
                        res[k, ] = x[i,]
                        k = k + 1
                }
        }
        res[1:(k-1), ]
}
#third function
funApply = function(x) {
        drop = apply(is.na(x), 1, any)
        x[!drop, ]
}
#fourth function
funOmit = function(x) {
        # The or operation is very fast, it is replacing the any function
        # Also note that it doesn't require having another data frame as big as x        
        drop = F
        n = ncol(x)
        for (i in 1:n)
                drop = drop | is.na(x[, i])
        x[!drop, ]
}

#R profiler
xx = matrix(rnorm(20000),1000,20)
xx[xx>2] = NA
x = as.data.frame(xx)

# Call the R code profiler and give it an output file to hold results
Rprof("exampleAgg.out")
# Call the function to be profiled
y <- funAgg(xx)
Rprof(NULL)
summaryRprof("exampleAgg.out")

Rprof("exampleLoop.out")
y <- funLoop(xx)
Rprof(NULL)
summaryRprof("exampleLoop.out")

Rprof("exampleApply.out")
y <- funApply(xx)
Rprof(NULL)
summaryRprof("exampleApply.out")

Rprof("exampleOmit.out")
y <- funOmit(xx)
Rprof(NULL)
summaryRprof("exampleOmit.out")

#Good to break code into functions so that the profiler can give useful info about where time is being spent
#C or Fortran code is not profiled












