setwd("D:/study/DataScience")
setwd("D:\\study\\DataScience")
#Relative: setwd("./data),setwd("../")
#absolute: setwd("Users/jtleek/data/")
#file.exists("directoryName")  #check whether a directory exists
#dir.create("GetAndCleanData") #create a directory
if(!file.exists("GetAndCleanData")){
        dir.create("GetAndCleanData")
}

#1. getting data from the internet -- download.file()
str(download.file)
#function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))  
#useful for downloading tab-delimited, csv, Excel files, etc.
fileUrl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/camera.csv") #if in MAC, should add option, method="curl" because of https instead of http
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
fileUrl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/camera.xlsx") 
list.files("./GetAndCleanData")
dateDownloaded<-date()
if(!require("xlsx")){
        install.packages("xlsx")
}
library(xlsx)






