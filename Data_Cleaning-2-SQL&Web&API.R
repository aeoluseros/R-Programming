setwd("D:\\study\\DataScience")

#1. MySQL
if(!require(RMySQL)){
        install.packages(RMySQL)
}
library("RMySQL")

#connect http://genome.ucsc.edu/goldenPath/help/mysql.html
#mysql --user=genome --host=genome-mysql.cse.ucsc.edu -A
#syntax:
        #con <- dbConnect(MySQL(), user="network_portal", password="monkey2us", dbname=db.name, host="localhost")
        #rs <- dbSendQuery(con, "select name from genes limit 10;")
        #data <- fetch(rs, n=10)
        #huh <- dbHasCompleted(rs)
        #dbClearResult(rs)
        #dbDisconnect(con)
ucscDb<-dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery(ucscDb,"show databases;");  #SHOW DATABASES is an SQL expression
#result<-dbGetQuery(ucscDb,"I love y;");
dbDisconnect(ucscDb);   #should get TRUE response to confirm disconnection
#focus database "hg19" (human genome 19)
hg19<-dbConnect(MySQL(),user="genome",db="hg19",host="genome-mysql.cse.ucsc.edu")
allTables<-dbListTables(hg19)
length(allTables)
allTables[1:5]  #fisrt five tables, each table corresponds to its own table
head(allTables, n=20)
#suppose we are interested in table "affyU133Plus2"
dbListFields(hg19,"affyU133Plus2") #dbListFields(conn, name of the remote table, ...),List field names of a remote table.
sql<-sprintf("select count(*) from affyU133Plus2")
dbGetQuery(hg19,sql)
#get one table out
affyData<-dbReadTable(hg19,"affyU133Plus2")
class(affyData)  #"data.frame"
head(affyData)
#any particular table might be gigantic and might be too big to read into R
#therefore we might want to select a specific subset.
query<-dbSendQuery(hg19,"select * from affyU133Plus2 where misMatches between 1 and 3")
affMis<-fetch(query) #fetch(res, n = -1, ...)
        #Fetch the next n elements (rows) from the result set and return them as a data.frame
quantile(affMis$misMatches)
affMisSmall<-fetch(query,n=10) #make sure you don't suck down a gigantic table
dim(affMisSmall)
names(affMisSmall)
#we need to stop the query after we fetch the data back.
dbClearResult(query) #dbClearResult(res, ...) clear the query from the remote server
#Don't forget to close the connection!
on.exit(dbDisconnect(hg19));

#Rule: Don't delete, add or join things from ensembl. Only select.


#2. HDF5(Hierarchical Data Format): used for storing large data sets.
#"groups" containing zero or more data sets and metadata.
   #Have a group header with group name and list of attributes
   #Have a group symbol table with a list of objects in group
#"datasets" multidimensional array of data elements with metadata
   #Have a header with name, datatype, dataspace, and storage layout
   #Have a data array with the data

#R HDF5 package is installed through the bioconductors
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library("rhdf5")
#create a HDF5 data set - H5createFile
created=h5createFile("example.h5") #create an empty file
created
#create a group with in the file  - h5createGroup
created=h5createGroup("example.h5","foo")  
created=h5createGroup("example.h5","baa")
created=h5createGroup("example.h5","foo/foobaa")
h5ls("example.h5")
#write objects to groups - h5write
A=matrix(1:10,nr=5,nc=2)
h5write(A,"example.h5","foo/A")
B=array(seq(0.1,2.0,by=0.1),dim=c(5,2,2)) #20 elements
attr(B,"scale")<-"liter"
h5write(B,"example.h5","foo/foobaa/B")
h5ls("example.h5")
#write a data set
df=data.frame(1L:5L,seq(0,1,length.out=5),c("ab","cde","fghi","a","s"),stringAsFactors=FALSE)
h5write(df,"example.h5","df")
h5ls("example.h5")
#Read data - h5read
readA<-h5read("example.h5","foo/A")
readB<-h5read("example.h5","foo/foobaa/B")
readdf<-h5read("example.h5","df")
readA
#write and read chunks
h5write(c(12,13,14),"example.h5","foo/A",index=list(2:4,1)) #change the first three rows in the first column
readAA<-h5read("example.h5","foo/A")
h5dump("example.h5",load=FALSE)

#3. Reading data from Web - Webscraping
#attempting to read too many pages too quickly can get you IP address blocked
con=url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ")
htmlCode=readLines(con)
close(con)
htmlCode #HTML
library(XML)
parsedHtmlCode=htmlParse(htmlCode,asText=TRUE) 
xpathSApply(parsedHtmlCode,"//title",xmlValue)

#the above result is a mess, we could use XML or httr package
#HTML parsing (parsing with XML)
library(XML)
url<-"http://scholar.google.com/citations?user=HI-I6C0AAAAJ"
html<-htmlTreeParse(url,useInternalNodes=TRUE) #equivalent to htmlParse(url)
xpathSApply(html,"//title",xmlValue)
xpathSApply(html,"//td[@id='col-citedby']",xmlValue)
#GET from the httr package
library(httr);
html2=GET(url)  #class type: response #could give us connection information
names(html2)
html2$content
html2$cookies
content2=content(html2,as="text")    #same as htmlCode generated from readLines(con).
        #as a raw object (as = "raw"), as a character vector, (as = "text"), 
        #and as parsed into an R object where possible, (as = "parsed"). If 
        #as is not specified, content does its best to guess which output is 
        #most appropriate.
parsedHtml=htmlParse(content2,asText=TRUE) #if the first argument is URL, then asText=FALSE, otherwise asText=TRUE
        #xmlParse and htmlParse are equivalent to the xmlTreeParse and 
        #htmlTreeParse respectively, except they both use a default value for 
        #the useInternalNodes parameter of TRUE
xpathSApply(parsedHtml,"//title",xmlValue)

#Accessing websites with passwords
pg1=GET("http://httpbin.org/basic-auth/user/passwd")
#response is 401: access denied
#with httr package we could log in with user and password
pg2=GET("http://httpbin.org/basic-auth/user/passwd",authenticate("user","passwd"))
names(pg2)  #same as names(pg1)
names(pg1)
pg2$content

#use handles to save authorization across multiple access to websites
google=handle("http://google.com") #assign "google" the website and cookies, next time we could directly use "google" to enter the google webpage
pg1=GET(handle=google,path="/") #if I authenticate one time, then the cookies will stay with that handle and you'll be authenticated in the following
pg2=GET(handle=google,path="search")
hamQuery<-GET(handle=google, path = "search", query = list(q = "ham"))
hamContent<-content(hamQuery,as="text")
parsedHam<-htmlParse(hamContent,asText=TRUE)
xpathSApply(parsedHtml,"//title",xmlValue)

#4. getting data from APIs
#google, facebook have these APIs
#use httr package to be able to get data from these websites
detach("package:httr",unload=TRUE)
library(httr)
#in this case, we need to create a developer's account on thse websites (sometimes same with the usual account)
#we're going to create an application using the Twitter handle from blog

#online help:  https://dev.twitter.com/rest/reference/get/statuses/mentions_timeline
#myapp=oauth_app("twitter",key="yourConsumerKeyHere",secret="yourConsumerSecretHere")
#sig=sig_oauth1.0(myapp,token="yourTokenHere",token_secret="yourTokenSecrectHere")
#homeTL=GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
#json1=content(homeTL)
#json2=jsonlite::fromJSON(toJSON(json1))
#json2[1,1:4]

##Linkedin Example - don't know how to 
oauth_endpoints("linkedin")
linkedin_app <- oauth_app("linkedin", key = "758bif2f68i7yp",secret="1xEsrQvlo4r1ynm2")
TokenLinkedIn <- R6::R6Class("ef031b1e-6b9f-4e6a-ac14-f12733d8136a", inherit = Token2.0, list(
        sign = function(method, url) {
                url <- parse_url(url)
                url$query$oauth2_access_token <- self$credentials$access_token
                list(url = build_url(url), config = config())
        },
        can_refresh = function() {
                TRUE
        },
        refresh = function() {
                self$credentials <- init_oauth2.0(self$endpoint, self$app,
                                                  scope = self$params$scope, type = self$params$type,
                                                  use_oob = self$params$use_oob)
        }
))
token <- TokenLinkedIn$new(
        endpoint = oauth_endpoints("linkedin"),
        app = linkedin_app,
        params = list(use_oob = FALSE, scope = NULL, type = NULL)
)

# Use API
req <- GET("https://api.linkedin.com/v1/people/~", config(token = token))
stop_for_status(req)
content(req)

#5. getting data from other resources
#google "data storage mechanism R package": for example "MYSQL R Package"

#interacting more directly with files
#file-open a connection to a text file
#url-open a connection to a url
#gzfile-open a connection to a .gz file
#bzfile-open a connection to a .bz file
?connections
#remember to close connections

#foreign packages - load data from Minitab, S, SAS, SPSS, Stata, Systat
library("foreign")
#Basic functions:
#read.arff(Weka)
#read.dta(Stata)
#read.mtp(Minitab)
#read.octave(Octave)
#read.spss(SPSS)
#read.xport(SAS)

#other database
install.packages("RPostgreSQL")
install.packages("RODBC") #including PostgreSQL,MySQL,Access,SQLite
install.packages("RMongo")
library("RPostgreSQL")
library("RODBC")
library("RODBC")

#reading images: jpeg,readbitmap,png,EBImage
#reading GIS data: rdgal, rgeos, raster
#reading music data: tuneR, seewave






























