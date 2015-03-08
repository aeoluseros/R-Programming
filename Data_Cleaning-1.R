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

#5. Reading JSON
#In JSON, data stored as: Numbers(double), Strings(double quoted), Boolean(true or false),
                        #Array(ordered, comma separated enclosed in []), 
                        #Object(unordered, comma separated collection of key:value pairs in {})
library(jsonlite)
jsonData<-fromJSON("https://api.github.com/users/jtleek/repos")
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
vignette("jsonlite")

#6. data.table package -- faster(written in C) and more memory efficient version of the data frames
#inherits from data.frame, all functions that accept data.frame work on data.table
#have some new syntax
if(!require("data.table"){
        install.packages("data.table")
}
library("data.table")
DF<-data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))









