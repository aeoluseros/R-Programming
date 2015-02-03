#Toolbox
#1.Getting helps
?rnorm   #directly access help file
help.serach("rnorm")  #search help files  #same with ??rnorm
??rnorm   #same with help.search("rnorm")
args("rnorm")   #get arguments

#2.readLines
con<-url("http://www.jhsph.edu","r")
#file, pipe, fifo, url, gzfile, bzfile, xzfile, unz and socketConnection
#file: the description is a path to the file to be opened or a complete URL or "" (the default) or "clipboard" 
#url: the description is a complete URL, including scheme (such as http://, ftp:// or file://)
#gzfile: the description is the path to a file compressed by gzip: it can also open for reading uncompressed files and those compressed by bzip2, xz or lzma.
#"r" or "rt": Open for reading in text mode. URLs can only be opened for reading.
# "w" or "wt": Open for writing in text mode.
#"a" or "at": Open for appending in text mode.
#"r+", "r+b": Open for reading and writing.
#"w+", "w+b": Open for reading and writing, truncating file initially.
#"a+", "a+b": Open for reading and appending.
x<-readLines(con)
head(x)

#3. getting and cleaing Data Content
# Excel, XML, JSON, MySQL, HDF5, Web
if (!require("RMySQL")) {
  install.packages("RMySQL")
}
library("RMySQL")
uscDb<-dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery()
dbDisconnect(ucscDb)
result

#4. Merging data
mergeData2<-merge(reviews, solutions, by.x="solution_id",by.y="id",all=TRUE)
head(mergedData2[,1:6],3)
reviews[1,1:6]

#5. EDA
#Fina relationships you didn't know about.EDA are usually not the final say. 
#EDA alone should not be used for generalizating/predicting
# Exploratory graphs, Plotting Systems in R(base, lattice, ggplot2), Hierachical clustering, K-means Clustering, Dimension Reduction
# ggplot2:
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library("ggplot2")
gplot(displ, hwy, data=mpg, geom=c("point","smooth"))
# k-means clutering:
set.seed(1234)
par(mar=c(0,0,0,0))  #mar: margins(bottom, left, top, right)
x<-rnorm(12, mean=rep(1:3, each = 4),sd=0.2)
y<-rnorm(12, mean=rep(c(1,2,1),each = 4),sd=0.2)
plot(x,y,col="blue",pch=19,cex = 2)
text(x+0.05,y+0.05,label=as.character(1:12))

#6.Statistical Inference
#sample->population. sampling scheme is important.(Monte Carlo)
#Asymptotics, Bootstrapping, Non-parametric tests, Bayesian statistics
#bootstrapping:
B<-1000
n<-length(gmVol)
resamples<-matrix(sample(gmVol,n*B,replace=TRUE),B,n)  #extract n*B samples from gmVol
median<-apply(resamples,1,median) #apply(X, MARGIN, FUN, ...)
#MARGIN: a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns.
sd(medians)
quantile(medians,c(0.025,0.975))

#7. Regression Models
#Casual Analysis
#If X predicts Y, it doesn't mean that X causes Y. --> Confounding means that there may be other variables cause the seeming correlation between two interested variables.
#Splines, Machine Learning Via regression, permutation tests, Weighted regression, Mixed Models(random intercepts)
#Three ways to deal with confounding: (1)fix a variable  (2)if you don't fix, you can stratify samples. (3)If can't fix a variable, randomize it

#simple random sampling: 
#stratified sampling: (1)group(stratiy) samples on certain characteristics
#systematic sampling
#cluster sampling:
#convenience sampling:
#non-probability sampling:
#multi-stage sampling:

#8.Practical Maching Learning
#caret package, Correlated predictor, prediction with regression, prediction with trees, boosting, bagging, model blending, forecasting

#Correlated predictor
if (!require("caret")) {
  install.packages("caret")
}
if (!require("kernlab")) {   # package for Kernel Methods
  install.packages("kernlab")
}
library("lattice")
library("caret") # required package: lattice & ggplot2
library("kernlab")
data(spam) # dim(spam): 4601 * 58
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE) #class(inTrain) = matrix
#spam$type:Levels: nonspam spam
#createDataPartition(y, times = 1,p = 0.5,list = TRUE,groups = min(5, length(y))): A series of test/training partitions are created
#times: the number of partitions to create; p:the percentage of data that goes to training
#list: should the results be in a list (TRUE) or a matrix with the number of rows equal to floor(p * length(y)) and "times" columns.

#createResample(y, times = 10, list = TRUE): creates one or more bootstrap samples
#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE) splits the data into k groups
#createTimeSlices(y, initialWindow, horizon = 1, fixedWindow = TRUE) creates cross-validation sample information to be used with time series data.
training <- spam[inTrain,]
testing<-spam[-inTrain,]
M<-abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8,arr.ind=T)
#arr.ind: logical; should array indices be returned when x is an array

#9. Building Data Products
#R packages: devtools, roxygen, testhat
#Marketing report: rCharts
#slidify
#shiny: interactive web app

##################################################3
#Git: version control tools
#set up Git Bash:
#git config --global user.name "aeoluseros"
#git config --global user.email "yklouk@gmail.com"
#git config --list
#mkdir ~/R-programming
#cd ~/R-programming
#git init
#git remote add origin https://github.com/aeoluseros/R-programming.git
# make a local copy:
#git clone https://github.com/aeoluseros/R-programming.git    

#update local repo:
#git add .   #adds all new files
#git add -u  #update all changed or deleted files
#git add -A  #above two
#git commit -m "message"

#put things to the github
#git push
#sometimes we need to "git pull origin master" first.

#sometimes we don't want to edit a version. we could create a branch:
#git checkout -b brachname
#see what branch you are on type:
#git branch
#switch back to the master branch type








