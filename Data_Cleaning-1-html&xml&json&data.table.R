setwd("D:/study/DataScience")
setwd("D:\\study\\DataScience")
#Relative: setwd("./data),setwd("../")
#absolute: setwd("Users/jtleek/data/")
#file.exists("directoryName")  #check whether a directory exists
#dir.create("GetAndCleanData") #create a directory
if(!file.exists("GetAndCleanData")){
        dir.create("GetAndCleanData")
}

#1. getting data from the internet -- download.file(fileUrl,destfile)
str(download.file)
#function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))  
#useful for downloading tab-delimited, csv, Excel files, etc.
fileUrl<-"http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/camera.csv") #if in MAC, should add option, method="curl" because of https instead of http
     #in Windows, we don't have to care about method, but we need to set mode = "wb" if a binary file is downloaded.
list.files("./GetAndCleanData")
dateDownloaded<-date()  # format(Sys.time(), "%a %b %d %H:%M:%S %Y") # POSIX's ctime
dateDownloaded  #be sure to record when you downloaded.

#2.Reading local flat files -- text, csv.
cameraData<-read.table("./GetAndCleanData/camera.csv",sep=",",header=TRUE)
cameraData<-read.csv("./GetAndCleanData/camera.csv") #header=TRUE(default), spe=","(default)
head(cameraData)
#options:: quote - tell R whether there are any quoted values. quote = "" means no quote. only considered for columns read as character.
        #  na.strings - set the character that represents a missing value
        #  nrow, skip

#3.Reading from Excel File
fileUrl<-"http://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/camera.xlsx",mode="wb") 
    #Since Windows (unlike Unix-alikes) does distinguish between text and binary files, 
    #care is needed that other binary file types are transferred with mode = "wb".
list.files("./GetAndCleanData")
dateDownloaded<-date()
if(!require("xlsx")){
        install.packages("xlsx")
}
library("xlsx")
cameraData<-read.xlsx("./GetAndCleanData/camera.xlsx",sheetIndex=1,header=TRUE)
#read.xlsx2 is much faster than read.xlsx but for reading subsets of rows may be slightly 
#unstable because internally read.xlsx2 uses readColumns which is tailored for tabular data.
head(cameraData)
#Reading Specific rows and columns
colIndex<-2:3
rowIndex<-1:4
cameraDataSubset<-read.xlsx("./GetAndCleanData/camera.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex)
cameraDataSubset
#write.xlsx
write.xlsx(cameraDataSubset, "./GetAndCleanData/cameraDataSubset.xlsx") 

#package XLConnect
if(!require("XLConnect")){
        install.packages("XLConnect")
}
library("XLConnect")
vignette ("XLConnect")  #user manual
wb = loadWorkbook("./GetAndCleanData/camera.xlsx", create = TRUE)
createSheet(wb, name = "womenData")
createName(wb, name = "womenName", formula = "womenData!$A$1", overwrite = TRUE)
writeNamedRegion(wb, women, name = "womenName") #women is a small embedded dataset
saveWorkbook(wb)
writeWorksheetToFile("./GetAndCleanData/ChickWeight.xlsx", data = ChickWeight, 
                     sheet = "chickSheet", startRow = 1, startCol = 1)


#4. Reading XML(Extensible Markup Language) -- Web Scraping from "source" page
#two components in XML file: Markup(give the text structure) and Content
library(XML)
fileUrl<-"http://www.w3schools.com/xml/simple.xml"
doc<-xmlTreeParse(fileUrl,useInternal=TRUE) #xmlTreeParse loads the XML document into R memory.
        #set useInternal=TRUE then we could just get the XML Content
class(doc) #"XMLInternalDocument" "XMLAbstractDocument" --> it's a special structure type, so we need special functions to read it
rootNode<-xmlRoot(doc)  #remove the first line of doc: <?xml version="1.0" encoding="UTF-8"?>
xmlName(rootNode)
names(rootNode)  #five elements in rootNode. like five columns' names in a data.frame
rootNode[[1]]
rootNode[[1]][[1]]
rootNode[[1]][[2]]

#Programatically extract parts of the file
xmlSApply(rootNode, xmlValue) #xmlSApply is a XML-version Sapply  #xmlValue

#Use Xpath to get a specific component
#/node:Top level node. //node: Node at any level.  node[@attr-name]Node with an attr name
#node[@attr-name="bob"]Node with attr name attr-name = 'bob'
xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)

#Another Example:
fileUrl<-"http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc<-htmlTreeParse(fileUrl, useInternal=TRUE)
scores<-xpathSApply(doc,"//div[@class='results']",xmlValue)
teams<-xpathSApply(doc,"//div[@class='city']",xmlValue)
teams
scores #L means lose

#5. Reading JSON data
#In JSON, data stored as: Numbers(double), Strings(double quoted), Boolean(true or false),
                        #Array(ordered, comma separated enclosed in []), 
                        #Object(unordered, comma separated collection of key:value pairs in {})
#R has 3 packages for working with JSON data: "RJSONIO","rjson","jsonlite"
#All packages provide 2 main functions -toJSON() and fromJSON()
library(jsonlite)
jsonData<-fromJSON("https://api.github.com/users/jtleek/repos")#save JSON data to "data.frame"
class(jsonData) #data.frame
names(jsonData)
names(jsonData$owner)
names(jsonData$owner$login)
#convert to JSON
myjson<-toJSON(iris,pretty=TRUE)
cat(myjson) #Outputs the objects, concatenating the representations.
                #an example about cat:
                iter <- stats::rpois(1, lambda = 10)
                ## print an informative message
                cat("iteration = ", iter <- iter + 1, "\n")
iris2<-fromJSON(myjson)
head(iris2) == head(iris) #TRUE

baseurl <- "http://projects.propublica.org/nonprofits/api/v1/search.json?order=revenue&sort_order=desc"
mydata0 <- fromJSON(paste0(baseurl, "&page=0"), flatten = TRUE) #flatten=TRUE: automatically flatten nested data frames into a single non-nested data frame
class(mydata0)  #list
mydata1 <- fromJSON(paste0(baseurl, "&page=1"), flatten = TRUE)
class(mydata1)
mydata2 <- fromJSON(paste0(baseurl, "&page=2"), flatten = TRUE)
class(mydata2)
mydata0$filings[1:10, c("organization.sub_name", "organization.city", "totrevenue")]
nrow(mydata0$filings)
filings <- rbind.pages(list(mydata0$filings, mydata1$filings, mydata2$filings))
#rbind.pages {jsonlite}: rbind.pages(list of data.frames)
head(filings)
colnames(filings)
dateDownloaded<-date() 

#Automatically combining many pages
baseurl <- "http://projects.propublica.org/nonprofits/api/v1/search.json?order=revenue&sort_order=desc"
pages <- list() #declare an empty list
for(i in 0:30){
        if(i==18|i==20|i==27) next
        mydata <- fromJSON(paste0(baseurl, "&page=", i)) #mydata is a list #paste0 is the no-sep version of paste
        message("Retrieving page ", i)
        pages[[i+1]] <- mydata$filings
}
#combine all into one
filings <- rbind.pages(pages)
#check output
nrow(filings)


#6. data.table package -- faster(written in C) and more memory efficient version of the data frames
#inherits from data.frame, all functions that accept data.frame work on data.table
#have some new syntax
if(!require("data.table"){
        install.packages("data.table")
}
library("data.table")
vignette("datatable-intro")
DF<-data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)
#DF<-data.frame(DF,w=DF$z^2)

DT<-data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)
tables()  #we could see tables in memory with this command.
sapply(DT,class) #see the column types

DT[2,]
DT[DT$y=="a",]
DT$z
DT[,z]  #same as DT$z
DT[c(2,3)]
DT[,c(2,3)]  #incorrecct   #this will not return the 2nd and 3rd columns. 
             #subsetting columns of data table is different from that of data frame.
#we could use:
DT[,c(2,3),with=FALSE]

#DIFFERENCE BETWEEN DATA.TABLE AND DATA.FRAME
DT[3]  #refers to the 3rd row , DT[3] == DT[3,]
DF[3]  #refers to the 3rd column,  DF[3] == DF[,3]
#DT[[3]]==DF[[3]]==DF[3] all refers to the 3rd columns
class(DT[,3,with=FALSE])  #data.table  # DT[,3] returns 3
class(DF[,3]) #VECTOR
DT[,"x",with=FALSE][[1]]  # DF[,"x"]
DT[NA]  #returns a 1 row data table
#DF[NA]  #Error in `[.data.frame`(DF, NA) : undefined columns selected
#DT[c(TRUE,NA,FALSE)] treats the NA as FALSE, but DF[c(TRUE,NA,FALSE)] returns NA rows
#DT[ColA==ColB] is simpler than DF[!is.na(ColA) & !is.na(ColB) & ColA==ColB,]
data.frame(list(1:2,"k",1:4)) #create 3 columns (transform list to data frame)
data.table(list(1:2,"k",1:4)) #create one column data table (accomodate the whole list)

#subsetting columns:
#in R, any expression is sum of a set of statements that are between curly brackets
{x=1
 y=2}
k={print(10);5}   #first assign k to 10, then assign k to 5. So k is 5 finally.
k=print(10)
print(k)
#the above property will come into play when we try to use expressions to summarize data sets
#instead of putting an index, you can pass a list of functions that you want to perform
DT[,list(mean(x),sum(z))]
DT[,table(y)]
DT[,list(y,z)]  #equivalent to DT[,c(2,3),with=FALSE]  #replace the incorrect DT[,C(2,3)]

###adding new column
DT[,w:=z^2]   # := -->new function in data.table. In data frame, we need to use DF<-data.frame(DF,w=DF$z^2)
              #the nice thing is when we use data frame, a new data frame would be generated. 
              #but for data.table, we don't have to so that we could save memory
DT2<-DT
DT[,y:=2]   #replace a column
head(DT,n=3)
head(DT2,n=3)  #oh shit, data.table is using pass by reference.
DT3<-copy(DT) #copy is a function specialized in data.table, pass by value
DT[,y:=3]   #replace a column
head(DT,n=3)
head(DT2,n=3)

#Multi-step Operations
DT[,m:={tmp<-(x+z);log2(tmp+5)}]  #m would be finally defined as log2(x+z+5)
DT
#plyr-like operations (plyr: Tools for splitting, applying and combining data)
DT[,a:=x>0]
DT[,b:=mean(x+w)]
DT[,b:=mean(x+w),by=a]  #by=a means group by a --> b=mean(x+w) where a is true. and b- mean(x+w) where a is false

#Special Variables in data.table
#.N --> the number of times a particular group appears
set.seed(123)
DT<-data.table(x=sample(letters[1:3],1E5,TRUE))
dim(DT) #10000*1
DT[,.N, by=x]

#Keys
DT<-data.table(x=rep(c("a","b","c"),each=100),y=rnorm(300))
setkey(DT,x)
DT['a']  #because x is a key, we could directly use x's value to subset.

#Use keys to faciliate joins between data tables
DT4<-data.table(x=c("a","a","b","dt1"),y=1:4)
DT5<-data.table(x=c("a","b","dt2"),z=5:7)
setkey(DT4, x);setkey(DT5,x)
merge(DT4,DT5) #merge items/tuples with the same keys

#Read things fast from the disc
big_df<-data.frame(x=rnorm(1E6),y=rnorm(1E6))
#big_dt<-data.table(x=rnorm(1E6),y=rnorm(1E6))
file<-tempfile()
###tempfile() returns a vector of character strings which can be used as names for temporary files.
system.time(write.table(big_df,file=file,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE))
#system.time(write.table(big_dt,file=file,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE))
system.time(fread(file)) #fread{data.table}: Similar to read.table but faster and more convenient.
system.time(read.table(file,header=TRUE,sep="\t"))

#new function: melt --> reshape
set.seed(45)
require(reshape2)
DT <- data.table(
        i1 = c(1:5, NA), 
        i2 = c(NA,6,7,8,9,10), 
        f1 = factor(sample(c(letters[1:3], NA), 6, TRUE)), 
        c1 = sample(c(letters[1:3], NA), 6, TRUE), 
        d1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"), 
        d2 = as.Date(6:1, origin="2012-01-01"))
DT[, l1 := DT[, list(c=list(rep(i1, sample(5,1)))), by = i1]$c] # list cols
DT[, l2 := DT[, list(c=list(rep(c1, sample(5,1)))), by = i1]$c]
# basic examples
melt(DT, id=1:2, measure=3)   #measure is column 3
melt(DT, id=c("i1", "i2"), measure="f1", value.factor=TRUE) # same as above, but value is factor
# on Date
melt(DT, id=c("i1", "f1"), measure=c("d1", "d2")) # date class attribute lost
melt(DT, id=c("i1", "f1"), measure=c("c1", "d1")) # value is char, date attribute lost
# on list
melt(DT, id=1, measure=c("l1", "l2")) # value is a list
melt(DT, id=1, measure=c("c1", "l1")) # c1 coerced to list
# on character
melt(DT, id=1, measure=c("c1", "f1")) # value is char
melt(DT, id=1, measure=c("c1", "i2")) # i2 coerced to char
# on na.rm=TRUE
melt(DT, id=1, measure=c("c1", "i2"), na.rm=TRUE) # remove NA

#new function: dcast
#A dcast.data.table is a much faster version of reshape2::dcast, but for data.table.
names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table
# no S3 method yet, have to use "dcast.data.table"
dcast.data.table(DT, time ~ variable, fun=mean)
dcast.data.table(DT, diet ~ variable, fun=mean)
dcast.data.table(DT, diet+chick ~ time, drop=FALSE)
#drop = FALSE will cast by including all missing combinations.
dcast.data.table(DT, diet+chick ~ time, drop=FALSE, fill=0)
# using subset
dcast.data.table(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))

