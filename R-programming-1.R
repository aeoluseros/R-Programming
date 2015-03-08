setwd("D:/study/DataScience")
if (!require("swirl")) {
  install.packages("swirl")
}
library("swirl")
swirl()   #use Esc to exit
bye()

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
#matrices are arrays with only 2 dimensions. #Arrays are matrices with more than 2 dimensions.
a<-array(rnorm(2*2*10),c(2,2,10))
class(a)  #array
#a<-array(rnorm(2*2*10),2,2,10)  #error
a<-matrix(rnorm(2*10),c(2,10))
class(a)  #matrix
a<-matrix(rnorm(2*10),2,10)
class(a)  #matrix

#matrix could contain "atomic" variables  
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
class(x)
unclass(x)  #unclass returns (a copy of) its argument with its class attribute removed
#unclass() is contrary to class()
#the order of the levels can be set using the "levels" arguement to a factor(). 
#This is important in linear modeling because the first level is used as the baseline level
x<-factor(c("yes","yes","no","yes","no"),levels=c("yes","no"))
x  # first level is "yes"

#7. missing value
x<-c(1,2,NA,10,3)
is.na(x)  # NA is for any classes. NA has a class too, so there are integer NA, character NA, etc.
is.nan(x)  # NaN is used for undefined mathematical operations. A NaN is also NA but the converse is not true.
#all FALSE is due to none of x is a undefined mathematical operations. 
x<-c(1,2,NaN,NA,4)
is.na(x)
is.nan(x)
#NA is undefined, so by definition, any comparisons to NA will be as well(undefined).
a <- sample(c(NA, 1:5), 20, replace = TRUE)
a
a!=NA  #all NA's
a=NA   #all NA's
NA == NA #NA
#three ways to remove NA's:
a[!is.na(a)]  # fastest
a[complete.cases(a)]
na.exclude(a) #same as na.omit(a)
b<-na.omit(a)
as.vector(b) # remove the attr(,"na.action") and attr(,"class") sections

#8. the last data type -- data frame
#data frame is a special type of list. # every element of the list has to have the same length
#every element refers to every column. the length of each element is the number of rows.
#unlike matrix, data frame could store different classes of objects in each column.
#data frames also have a special attribute called "row.names".
#read.table() and read.csv() create data frame.
#convert to matrix by data.matrx(), but you couldn't convert if variables inside are of different classes
x<-data.frame(foo=1:4,bar=c(T,T,F,F))

#9. Name attributes
x<-"foo"
names(x)
x<-1:3
names(x)
names(x)<-c("foo","bar","norf")  # give the name to the vector x
x
names(x)
x<-list(a=1,b=2,c=3)   # list could also have a name
x
m<-matrix(1:4,nrow=2,ncol=2)
dimnames(m) <- list(c("a","b"),c("c","d"))
m

#10.Reading/writing Table
#read.table (text file, compressed files, URL,etc...), read.csv #read.table("../data.txt",header=T,sep="\t")
#write.table(), write.csv()
#read.table is too important:
# read.table(file, header = FALSE, sep = "", quote = "\"'",
#            dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
#            row.names, col.names, as.is = !stringsAsFactors,
#            na.strings = "NA", colClasses = NA, nrows = -1,
#            skip = 0, check.names = TRUE, fill = !blank.lines.skip,
#            strip.white = FALSE, blank.lines.skip = TRUE,
#            comment.char = "#",
#            allowEscapes = FALSE, flush = FALSE,
#            stringsAsFactors = default.stringsAsFactors(),
#            fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)
#file: the name of file, or a connection
#sep: default is " "(space)
#colClasses: a character vector indicating the class of each column in the dataset
#comment.char: a character string indicating the comment character (default is #, anything including and right to the symbol would be ignored)
#skip: the number of lines to skip from the beginning
#stringsAsFactors: should character variables   #default.stringsAsFactors() = TRUE
#read.csv() is identical except that the default sep is a comma and default of header is TRUE

#the general rule of RAM memory should be twice the memory requirement.
set.seed(2)
datatable <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
write.table(datatable,file="datatable.txt")
initial<-read.table("datatable.txt",nrows=100)
classes<-sapply(initial, class) # loop over each column, calling the "class" function
tabAll <- read.table("datatable.txt",colClasses = classes) # specifying colClasses would make R run faster. so tabAll is read into faster than initial
write.csv(tabAll, file = "tabAll.csv")
write.table(tabAll, file = "tabAll.tsv", sep = "\t")
unlink("tabAll.csv")
unlink("tabAll.tsv")
###
#readLines: read lines from files or webpage
#con<-gzfile("words.gz")
#x<-readLines(con,10)
#x
con<-url("http://www.jhsph.edu","r")
x<-readLines(con)
head(x)

#source("R folder/code.R") (inverse of dump)

#dget(file) (inverse of dput(x,file=", control = c("keepNA", "keepInteger", "showAttributes")))
#dput(x,file=", control = c("keepNA", "keepInteger", "showAttributes"))
#the dump function writes the dput output to a file. They write the exact same representation we saw above on the console
###ex.1####
set.seed(1)
mydf <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#mydf<-data.frame(x="a",y="b")
save(mydf, file = "saveddf.RData")
load("saveddf.RData")
unlink("saveddf.RData")  # remove the file saveddf.RData
load("saveddf.RData")
save.image('myworkspace.RData')
ls()
rm(m)
rm(list = ls())
load('myworkspace.RData')
dput(mydf)
dput(mydf, "saveddf.txt")
#unlink("saveddf.txt")   #unlink could be used to delete any type of file
mydf2 <- dget("saveddf.txt")
head(mydf)
head(mydf2)
mydf == mydf2   #these two seem equal but actually not. They differ due the rules of floating point values (a basic element of computer programming that is unimportant to really understand):
#therefore, if elements in mydf are characters, mydf and mydf2 would be equal
unlink("saveddf.text")
dump("mydf")  #the default dumped file name is dumpdata.R
mydf3<-source("dumpdata.R", echo = TRUE)
class(mydf3) #class is list
class(mydf)
unlink("dumpdata.R")

###ex.2####
y<-data.frame(a=1,b="a") #class(y$b) is factor
dput(y)
dput(y,file="y.R")
new.y<-dget("y.R")
new.y
unlink("y.R")
x<-"foo"
dump(c("x","y"),file="data.R")  #use double quotes when refering to x,y in dump() function
                                #we don't have to use double quotes if just using c(x,y) to create a list
rm(x,y)
source("data.R")
x

#load() --> for reading in saved workspaces  *.Rhistroy
#save(list = ls(all = TRUE), file = ".RData")
x <- stats::runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")
save(list=ls(),file = "xy.RData")
rm(list=ls()) 
load("xy.RData")

#unserialize(connection, refhook = NULL) --> for reading single R objects in binary form
#serialize --> convert to binary form
x <- serialize(list(1,2,3), NULL)
unserialize(x)

#11. Data are read in using "connetion" interface
#file: connection to a file
str(file) #str function: Compactly display the internal structure of an R object
#function (description = "", open = "", blocking = TRUE, encoding = getOption("encoding"), raw = FALSE)  
#open's options:: "r": read-only, "w": writing(and initializing a new file), "a":appending, "rb","wb","ab":reading, writing, or appending in binary mode(Windows)
set.seed(3)
foo <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
write.table(foo,"foo.txt",row.names = TRUE)
rm(list=ls())
con<-file("foo.txt","r")
data<-read.table(con)
close(con)
unlink("foo.txt")
data
#same as: data<-read.table("foo.txt")

#gzfile: opens a connection to a file compressed with gzip
#con<-gzfile("words.gz")
#x<-readLines(con,10)
#x

#bzfile: opens a connection to a file compressed with bzip2
#url: a connection to a webpage


#12.subsetting
#[]:return an object of the same class as the original; can be used to select more than one element
#[[]]: used to extract a single element(column) of a list or a data frame; the class of the returned object will not necessarily be a list or data frame
#$: is used to extract elements of a list or data frame by column name;
x<-c("a","b","c","c","d","a")
x[1]  # numerical index
class(x[1]) = class(x)

x[1:4]
x[c(1,2,3)]  #x[1,2,3] is incorrect
x[x>"a"]

u<-x>"a"
u
x[u]  #logical index  #same as x[x>"a"]

x<-list(foo=1:4,bar=0.6)
x[1]  # we got a list
x[[1]]  # we just got a sequence of integer
class(x[[1]]) #integer
x$bar
x[["bar"]]
x["bar"]
class(x["bar"])  #got a list

x<-list(foo=1:4,bar=0.6,baz="hello")
x[c(1,3)]  #x[1,3] is an error
name<-"foo"
x[[name]]
x$name   #object 'name" not found, because name includes the double quote and foo.
x$foo  

x<-list(a=list(10,12,14),b=c(3.14,2.81),c=2.5)
x[1,3]  # incorrect number of dimensions
x[c(1,3)]
x[[c(1,3)]]
x[[c(2,1)]]
x[[c(1,3)]] = x[[1]][[3]]

x<-matrix(1:12,3,4)
x[1,2]
x[1,] # the first row
x[,2] # the second column
x[1,2]
x[1,2,drop=FALSE]  #when a single element(number or vector) of a matrix is retrieved, it returns a vector. This behavior can be turned off by setting drop=FALSE
x[1,,drop=FALSE]
x[1:2,2:3]  # we don't have to set drop=FALSE here.

#partial matching
x<-list(aardvark=1:5)
x$a  #$sign do partial matching
x[["a"]]
x[["a",exact=FALSE]]
x<-list(aardvark=1:5,aa=list(10,12,14),b=c(3.14,2.81))
x$aa  #this will return NULL because R couldn't determine which element to return

#removing missing values
x<-c(1,2,NA,4,NA,6)
bad<-is.na(x)  #logical index method
x[!bad]
y <- c("a","b",NA,"d","f",NA)
good<-complete.cases(x,y)  #indexes that both x and y are TRUE
good
x[good]
y[good]

airquality[1:6,]
good<-complete.cases(airquality)
airquality[good,][1:6]

#13. vectorized operations
x<-1:4; y<-6:9
x+y;x/y;x>=2;x*y;y==8  #other languages need a loop to do this

x<-matrix(1:4,2,2); y<-matrix(rep(10,4),2,2)
x*y   # multiply element by element
x%*%y  # matrix multiplication   #check: ?`%/%`
x/y
z<-y%/%x  # matrix division
x*z
z%*%x  #I don't know what is this!

###########practice#############
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








