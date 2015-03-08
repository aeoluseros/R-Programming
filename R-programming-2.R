#1. control structures
#if condition:
x<-10
if(x==5){
        y<-10
}else if(x>6){
        y<-0
}else{
        y<-5
}
# we don't have to use 'else'.
if(x==10){
        y<-11
}
if(x>10){
        y<-9
}

#for,while,repeat -- three kinds of loops
#control structures mentioned here are primarily useful for writing programs; 
#for command-line interactive work, the *apply functions are more useful;
#for loop
for(i in 1:10){
        print(i)
}

#three different ways to use for loop
x<-c("a","b","c","d") # same as: x<-c('a','b','c','d')
for(i in 1:4){
        print(x[i])
}

for(i in seq_along(x)){
        print(x[i])
}

for(letter in x){
        print(letter)
}

for(i in 1:4) print(x[i])  #if for loop only has single expression, we could remove the curly braces.

#while loop
count<-0
while(count<10){
        print(count)
        count<-count+1
}
#could have more than one condition with while loop
z<-5
while(z>=3&&z<=10){   #conditions are alwys evaluated from left to right
        print(z)
        coin<-rbinom(1,1,0.5)
        if(coin==1){
                z<-z+1
        }else{
                z<-z-1
        }    
}

#repeat infinite loop + break(the only way to exit a repeat)
x0<-1
tol<-1e-8
repeat{
        x1<-rbinom(1,1,0.5)
        print(x1)
        if(abs(x1-x0)<tol){
                break
        } else{
                x0<-x1
        }
}

#next is used to skip an iteration of a loop
for(i in 1:100){
        if(i<=20)
                ##skip the first 20 iterations
                next
        print(i)
}

#"return(value)" signals that a function/loop should exit and return a given value

2. #####writing functions#####
add2<-function(x,y){
        x+y
}
add2(3,5)

above10<-function(x){
        use <- x>10
        x[use]  #subset x
}

above<-function(x,c=3){
        use<-x>c
        x[use]
}
x<-1:12
above(x,10)
above(x)  #default critical value is 3

columnmean<-function(y,removeNA=TRUE){
        nc<-ncol(y)  #number of columns
        means<-numeric(nc)  #empty vector with all zeros
        for(i in 1:nc){
                means[i]<-mean(y[,i],na.rm=removeNA)
        }
        means
}
columnmeans <- function(y) sapply(y[complete.cases(y),],mean)  #same function
columnmean(airquality)  #loop over column
columnmeans(airquality)


#3.function arguments
#functions could be passed as arguments to other functions
#The return value of a function is the last expression in the function body to be evaluated.
#so there is no special expression for returning something for a function, although there is a function called return.
formals(file)  #formals() function returns a list of all the formal arguments of a function
args(lm)
#argument matching can also be partially matched.

#4.arguments are valuated lazily
f<-function(a,b){
        a^2
}
f(2)  #the function doesn't use b, so we don't have to specify b

f<-function(a,b){
        print(a)
        print(b)
}
#f(45)  # the value of a could still be printed, but the second line would commit error

#5. the "..." argument
#... is used when extending another function and you don't want to copy the entire argument list of the original function
myplot<-function(x,y,type="l",...){
        plot(x,y,type=type,...)
}
#... argument is also necessary when the number of arguments passed to the function cannot be known in advance
args(paste)  #paste function is used to concatenate strings together and returns a character variable
args(cat) #cat will not return anything, it will just output to the console or another connection.
c<-paste("a","b",sep=":")
#any arguments that appear after ... on the argument list must be named explicitly and cannot be partially matched
paste("a","b",se=":")  #partial matching cannot be partially matched
d<-cat("a","b",sep=":")  # d couldn't be assigned a value because cat() is just used to print out.
print(paste("a","b",sep=":"))

#6. Symbol binding -- how does R know which value to assign to which symble?
lm<-function(x) {x*x}
lm  #it won't give the value of lm that is in the "stats" package
#R uses lexical scoping or static scoping (equivalent concepts)
search()  # the search list when R tries to find a value
#lm is deined in Global Environment, so when I  that object would be found first
rm(lm)
lm
stats::lm
#when a package is loaded, it would be put in position 2 of the search list.
#R has separate namespaces for functions and non-functions so it's possible to have an object named c and a function named c
#free variables:
#free variables are not formal arguments and are not local variables.
f<-function(x,y){
        x^2+y/z
}
rm(z)
#f(2,3)
z<-2
f(2,3)  #scoping rules of a language determine how values are assigned to free variables.

#define a function inside another function (not allowed in some languages such as C):
make.power<-function(n){
        pow<-function(x){
                x^n
        }
        pow
}
cube<-make.power(3)
square<-make.power(2)
cube(3)
square(5)
ls(environment(cube)) #"ls" and "objects" return a vector of character strings giving the names of the objects in the specified environment.
objects(environment(cube))
get("n",environment(cube)) #search an object in an environment
get("n",environment(square))  #cube and square both functions have different environments

y<-10
f<-function(x){ #y and g are both free variables
        y<-2
        y^2+g(x)
}
g<-function(x){ 
        x*y
}
f(3) #with lexical scoping, the value of Y and the function g is loked up in the environment 
#in which the function is defined, which in this case is the global environment. 
#So the value of y in function g is 10. so 2^2 +3*10. 
#when you looking for a free variable in funtion g, you will look up global environment first.
#other languages also support lexical scoping: Scheme, Python, Perl, Common Lisp
#in SPLUS, free variables are always looked up in the global workspace, so everything can be 
#stored on the disk because the "defining environment" of all functions is the same.

#7. Application: Optimization
#optim, nlm, optimize -- used in MLE(minimize, maximize)
make.NegLogLik<-function(data,fixed=c(FALSE,FALSE)){
        params<-fixed    #parameters
        function(p){
                params[!fixed]<-p  #the unfixed parameter would be assigned to be p. p should be a two-element vector when fixed=c(FALSE,FALSE)
                mu<-params[1]
                sigma<-params[2]
                a<--0.5*length(data)*log(2*pi*sigma^2)
                b<--0.5*sum((data-mu)^2)/(sigma^2)
                -(a+b)
        }
}
set.seed(1);
normals<-rnorm(100,1,2)
nLL<-make.NegLogLik(normals)
ls(environment(nLL))  #return the objects in the environment of the nLL function.
args(optim)
optim(c(mu=0,sigma=1),nLL)$par   #initial guess of params: p=c(mu=0,sigma=1)
formals(optim)
nLL<-make.NegLogLik(normals,c(FALSE,2)) #fixing sigma = 2
optimize(nLL,c(-1,3))$minimum   #optimize is used for single variable only.
nLL<-make.NegLogLik(normals,c(1,FALSE))
optimize(nLL,c(1e-6,10))$minimum  #c(1e-6,10) is an interval
#plot likelihood
nLL<-make.NegLogLik(normals,c(1,FALSE))
x<-seq(1.9,2.1,len=100)
y<-sapply(x,nLL)
plot(x,exp(-(y-min(y))),type="l")  #if normals have more value, the plot would be sharper.

nLL<-make.NegLogLik(normals,c(FALSE,2))
x<-seq(0.5,1.5,len=100)
y<-sapply(x,nLL)
plot(x,exp(-(y-min(y))),type="l")


#suggestion: limit the  size of a function. each function only does one thing. 
#one function is no more than one page.


#8. date and times in R
#Class of date: Date (store as the number of days since 1970-01-01)
#class of Time: POSIXct or POSIXlt (store as the number of seconds since 1970-01-01)
#ct means concise time format. lt means long time format.
#in POSIXct class, times are represented at just as very large integers. It's a useful 
#                  type of class if you want to store times in a data frame or something 
#                  like because it's basically a big integer vector.
#in POSIXlt class stores a time as a list, so there is a bunch of other useful information
#                  about a given time, for example what's the day of the week of that time,
#                  what's the day of the years, the day of the week, the day of the month,
#                  or the month itself
#three functions: weekdays(give the day of the week), months(give the month name), 
#                 quarters(give the quarter number: "Q1","Q2","Q3","Q4)
Sys.time()
x<-as.Date("1970-1-1")
x
class(x)
unclass(x) #returns 0
class(unclass(x)) #numeric
unclass(as.Date("1970-01-02"))
x<-as.Date("1970/1/1")
x<-as.Date("1/1/1970") #wrong format
p<-as.POSIXlt(Sys.time(), "GMT")
unclass(p)
names(unclass(p))
p$sec
p$yday
p$isdst #Daylight Saving Time flag. Positive if in force, zero if not, negative if unknown.
q<-as.POSIXct(Sys.time(),"EST")
unclass(q) #a large integer number
names(unclass(q))  # NULL

#strptime function
datestring<-c("January 10,2012 10:40","December 9, 2011 9:10")
x<-strptime(datestring,"%B %d, %Y %H:%M")  #%B is full month Name,  %b is abbr. month name, but they are interchangable
x
datestring<-c("Jan 10,2012 10:40","Dec 9, 2011 9:10")
x<-strptime(datestring,"%b %d, %Y %H:%M")
x
class(x)

#as.Date, as.POSIXct, as.POSIXlt
x<-as.Date("2012-01-01")
class(x)
y<-strptime("9 Jan 2011 11:34:21","%d %b %Y %H:%M:%S")
#x-y  #error will show up
x<-as.POSIXlt(x)
class(x)
x-y

x<-as.Date("2012-03-01");y<-as.Date("2012-02-28")
x-y
x<-as.POSIXlt("2012-10-25 01:00:00")
y<-as.POSIXlt("2012-10-25 6:00:00",tz="GMT")
#R will automatically take care of time zone
x-y














