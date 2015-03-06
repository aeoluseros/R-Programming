1. #control structures
#if condition:
x<-10
if(x=5){
  y<-10
}else if(x>6){
  y<-0
}else(x=6){
  y<-5
}
# we don't have to use 'else'.
if(x=10){
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

for(i in seq_len(x)){
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

#return signals that a function/loop should exit and return a given value
for(i in 1:100){
  if(i=20)
    ##skip the first 20 iterations
    return 10
  print(i)
}


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
columnmean(airquality)
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
f(45)  # the value of a could still be printed, but the second line would commit error

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
f(2,3)
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









































