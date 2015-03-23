setwd("D:\\study\\DataScience")

#1. subsetting - quick review
set.seed(13435)
X<-data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X<-X[sample(1:5),];
X$var2[c(1,3)]=NA
X
X[,1]
X[,"var1"]
X[1:2,"var2"]
X[(X$var1<=3 & X$var3>11),]  #subset using logical statements
X[(X$var1<=3|X$var3>15),]
#dealing with NA. (NA's would not be omitted in above four expressions)
X[which(X$var2>8),]  # which: Give the TRUE indices of a logical object, allowing for array indices. NAs are omitted
#sorting
sort(X$var1)
sort(X$var1,decreasing=TRUE)
sort(X$var2,na.last=TRUE)
sort(X$var2,na.last=FALSE)
#Ordering
X[order(X$var1),]
X[order(X$var1,X$var3),]
####ordering with plyr
library(plyr)
arrange(X,var1)      #for fast ordering without using ordering() commands
arrange(X,desc(var1))
#add new column
X$var4<-rnorm(5)
X
Y<-cbind(X,rnorm(5))  #another way to add column
Y
Z<-rbind(Y,rnorm(5))
Z


#2.Summarizing the data
if(!file.exists("./GetAndCleanData")){dir.create("./GetAndCleanData")}
fileUrl<-"http://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/restaurants.csv")
restData<-read.csv("./GetAndCleanData/restaurants.csv")
head(restData,n=3)
tail(restData,n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict,na.rm=TRUE)
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))
table(restData$zipCode,useNA="ifany") #useNA="ifany" -->if there is any missing values, they'll be
                                    #an added column to this table, which will be NA and NA's count. 
                                #By default, NA's nubmer would not be reported.
table(restData$councilDistrict,restData$zipCode)  #2-dim table
class(table(restData$councilDistrict,restData$zipCode))  #table
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)
colSums(is.na(restData))
all(colSums(is.na(restData))==0)
#Values with specific characteristics
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))
restData[restData$zipCode %in% c("21212","21213"),] #pass to row subsetting.
#Cross tabs
data(UCBAdmissions)
DF=as.data.frame(UCBAdmissions) #data.frame() calls as.data.frame().therefore as.data.frame is faster:
xt<-xtabs(Freq~Gender+Admit,data=DF) #Create a contingency table (optionally a sparse matrix) 
                                     #from cross-classifying factors
#Flat Tables
warpbreaks$replicate<-rep(1:9,len=54)
xt<-xtabs(breaks~.,data=warpbreaks)
ftable(xt)
#size of a data set
fakeData<-rnorm(1e5)
object.size(fakeData)
class(fakeData)
print(object.size(fakeData),units="Mb")


#3. Create New Variables
#often the raw data won't have a value you are looking for
#Common Variables to create: (1)missingness indicators (2)"cutting up" quantitative variables (3)Applying transforms
if(!file.exists("./GetAndCleanData")){dir.create("./GetAndCleanData")}
fileUrl<-"http://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/restaurants.csv")
restData<-read.csv("./GetAndCleanData/restaurants.csv")
#Creating Sequences
s1<-seq(1,10,by=2);s1
s2<-seq(1,10,length=3);s2
x<-c(1,3,8,25,100); 
seq(along=x)
seq_along(x) #seq_along and seq_len are very fast primitives for two common cases.
seq_len(9)
#add one column by subsetting
names(restData)
restData$nearMe<-restData$neighborhood %in% c("Rolland Park","Homeland") #nearMe column is appended
table(restData$nearMe)
#create binary variable
restData$zipWrong=ifelse(restData$zipCode<0,TRUE,FALSE) #same as if in excel
table(restData$zipWrong,restData$zipCode<0)
#create catogorical/factor variable with cut-- break a quantitative variable up into a categorical variable
restData$zipGroup=cut(restData$zipCode,breaks=quantile(restData$zipCode))
#cut(x, breaks, labels = NULL,include.lowest = FALSE, right = TRUE, dig.lab = 3,ordered_result = FALSE, ...)
class(restData$zipGroup)
table(restData$zipGroup)
table(restData$zipGroup,restData$zipCode)

####Easier cutting with Hmisc(Harrell Miscellaneous) 
if(!require("Hmisc")){install.packages("Hmisc")}
library("Hmisc")
restData$zipGroups<-cut2(restData$zipCode,g=4)
#restData$zipGroups<-cut2(restData$zipCode,cuts=quantile(restData$zipCode))
str(cut2) #function (x, cuts, m = 150, g, levels.mean = FALSE, digits, minmax = TRUE, oneval = TRUE, onlycuts = FALSE)
        #cut2: Function like cut but left endpoints are inclusive and labels are of the form 
        #[lower, upper), except that last interval is [lower,upper]. If cuts are given, will 
        #by default make sure that cuts include entire range of x. Also, if cuts are not given, 
        #will cut x into quantile groups (g given) or groups with a given minimum number of 
        #observations (m)(the algorithm does not guarantee that all groups will have at least m observations.. 
class(restData$zipGroups)
table(restData$zipGroups)
head(restData)

x <- runif(1000, 0, 100)
z <- cut2(x, c(10,20,30))
table(z)
table(cut2(x, g=10))      # quantile groups
table(cut2(x, m=50)) 

#Creating factor variables
restData$zcf<-factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)
####levels of factor variables
yesno<-sample(c("yes","no"),size=10,replace=TRUE)
yesnofac<-factor(yesno,levels=c("no","yes"))
relevel(yesnofac,ref="yes") #The levels of a factor are re-ordered so that the level specified by ref is first and the others are moved down.
as.numeric(yesnofac)
#use the mutate function to create a new variable and add it to the data set
head(restData)
names(restData)
library(Hmisc);library(plyr)
restData2<-mutate(restData,zipGroups2=cut2(zipCode,g=4))
names(restData2)
table(restData2$zipGroups2)
restData2$zipGroups2==restData2$zipGroups2

#4.Transform variables
#abs(x),sqrt(x),ceiling(x),floor(x),cos(x),sin(x),log(x),log2(x),log10(x),exp(x)
#round(x,digits=n), round(3.475,digits=2)
#signif(x,digits=n),signif(3.475,digits=2) is 3.5 #signif rounds the values in its first argument to the specified number of significant digits.

#5.Reshape data
library(reshape2)
head(mtcars)
#dcasting data frames
mtcars$carname<-rownames(mtcars)
head(mtcars)
carMelt<-melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp")) #id variables, measure variables
?melt
head(carMelt,n=3)  #in variable column, you will see mpg and hp(both show up for each car)
nrow(mtcars)*2== nrow(carMelt)
#recast after melting -- acast and dcast are the same
cylData<-dcast(carMelt,cyl~variable)  #put cyl on the rows, and variable on the columns
cylData
?dcast
cylData<-dcast(carMelt,cyl~variable,mean)
cylData
carNameData<-acast(carMelt,carname~variable,mean)
carNameData
#averaging values
head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,mean)  #take mean grouped by spray
#another way - split, apply, combine
#split
spIns = split(InsectSprays$count,InsectSprays$spray)  #group by spray
spIns
#apply
sprCount=lapply(spIns, sum)
sprCount
#combine
unlist(sprCount)
#or take apply and combine together
sapply(spIns, sum)

#using plyr package - - the first argument is always the data frame
#summarise(mtcars, mean=mean(disp))
#summarise(baseball,duration = max(year) - min(year),nteams = length(unique(team)))
ddply(InsectSprays,.(spray),summarise,sum=sum(count))#we use sum as the way to summarise variable spray
ddply(InsectSprays,"spray",summarise,sum=sum(count)) #same with above

library(dplyr) 
summarise(group_by(mtcars, cyl), mean(disp)) #group_by(data frame, variable) generate a data frame
summarise(group_by(mtcars, cyl), m = mean(disp), sd = sd(disp))
ddply(InsectSprays,"spray",summarize,sum=sum(count)) #the only difference is summarize instead of summarise
detach(package:dplyr,unload=TRUE)
#use plyr to create a new variable
spraySums<-ddply(InsectSprays,.(spray),summarise,sum=ave(count,FUN=sum)) 
        #surprisingly, we could use sum function in ave function 
        #with ave, we generate a column with the same length as the summarized variable
dim(spraySums)
head(spraySums)

#different ways to summarize the sum by spray - the first argument is always the data frame
(count_by_spray <- with(InsectSprays, split(count, spray)))
(mean_by_spray <- lapply(count_by_spray, mean))
unlist(mean_by_spray)
sapply(count_by_spray, mean)  #same with the above two lines but with higher efficiency
vapply(count_by_spray, mean, numeric(1)) #same with the above line
with(InsectSprays, tapply(count, spray, mean))
tapply(InsectSprays$count, InsectSprays$spray, mean)
with(InsectSprays, by(count, spray, mean))
by(InsectSprays$count, InsectSprays$spray, mean)
aggregate(count ~ spray, InsectSprays, mean)
ddply(InsectSprays, .(spray), summarise, mean.count = mean(count))
dlply(InsectSprays, .(spray), summarise, mean.count = mean(count)) #list, dlply is similar to by except that the results are returned in a different format.
with(InsectSprays, ave(count, spray)) #used when when you want the output statistic vector to have the same length as the original input vectors


#6.tutorial of reshape
#http://www.slideshare.net/jeffreybreen/reshaping-data-in-r
pop<-read.csv('http://www.census.gov/2010census/csv/pop_density.csv', skip=3)
pop<-pop[,1:12]
colnames(pop)
colnames(pop)=c('state',seq(1910,2010,10))
colnames(pop)
head(pop,n=40)
#library doBy - split data into groups: summaryBy, splitBy, orderBy, sampleBy, transformBy etc.
if(!require("doBy")){install.packages("doBy")}
library(doBy)
top=orderBy(~-2010,pop)  
top1=orderBy(-2010~,pop) 
dim(top)
dim(top1)
?orderBy #orderBy(formula, data)
#This function is essentially a wrapper for the order() function - the important difference 
#being that variables to order by can be given by a model formula.
#The sign of the terms in the formula determines whether sorting should be ascending or decreasing
top=subset(top, state!='United States') #subet is a function in {base}
top1=top[top$state!='United States',] #equivalent
top=head(top,10)
top$state=factor(top$state)
head(top)
#how would you plot population versus year?
names(top)
mtop=melt(top,id.vars='state',variable.name='year',value.name='population') #measure.vars is omitted
####### melt's introduction:
#put measured variables into one column, and give the column as name "variable.name".
#melt.data.frame: melt(data, id.vars, measure.vars, variable.name = "variable", ..., na.rm = FALSE, value.name = "value",factorsAsStrings = TRUE)
        #id.vars: vector of id variables. Can be integer (variable position) or string (variable name). If blank, will use all non-measured variables.
        #variable.name: name of variable used to store measured variable names
        #measure.vars: vector of measured variables. Can be integer (variable position) or string (variable name). If blank, will use all non id.vars
        #value.name: name of variable used to store values
        names(airquality) <- tolower(names(airquality))
        melt(airquality, id=c("month", "day"))
        names(ChickWeight) <- tolower(names(ChickWeight))
        melt(ChickWeight, id=2:4) #id = c("chick","diet","variable"), measure = 
        melt(ChickWeight, id = c(" time","chick","diet"))
#melt.array: melt(data, varnames = names(dimnames(data)), ...,na.rm = FALSE, as.is = FALSE, value.name = "value")
        a <- array(c(1:23, NA), c(2,3,4))
        melt(a)
        melt(a, na.rm = TRUE)
        melt(a, varnames=c("X","Y","Z"))
#melt.list: melt(data, ..., level = 1)
        a <- as.list(c(1:4, NA))
        melt(a)
        names(a) <- letters[1:4]
        melt(a)
        a <- list(matrix(1:4, ncol=2), matrix(1:6, ncol=2))
        melt(a)
        a <- list(matrix(1:4, ncol=2), array(1:27, c(3,3,3)))
        melt(a)
###############
head(mtop)
library(ggplot2)
ggplot(data=mtop,aes(group=state))+geom_line(aes(x=year,y=population,color=state))
?ggplot
####ggplot & geom_line
        mry <- do.call(rbind, by(movies, round(movies$rating), function(df) {
                nums <- tapply(df$length, df$year, length)
                data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
        }))        
        p <- ggplot(mry, aes(x=year, y=number, group=rating))
        p + geom_line()
        # Add aesthetic mappings
        p + geom_line(aes(size = rating))
        p + geom_line(aes(colour = rating))
#######
dcast(mtop,state~year,value.var='population') #value.var: name of column which stores values
#The cast formula has the following format: x_variable + x_2 ~ y_variable + y_2 ~ z_variable ~ ... 


#6.Managing Data Frame with dplyr - an optimized and distilled version of plyr package. in C++
#arrange, filter(extracct a subset of rows based on logical conditions), select(return a subset of the columns), 
#mutate(add new variables), rename, summarize/summarise(summary statistics of different variables)
#the first argument should always be a data frame, and the result is also a data frame
#the subsequent arguments describe what to do with the first argument and you can refer to columns directly without using $
library(dplyr)
chicago<-readRDS("./GetAndCleanData/chicago.rds")
head(chicago)
dim(chicago)
str(chicago)
#select
head(select(chicago,city:dptp)) #such expression as city:dptp are normally not be able to use in other functions, but you can use it in the select function
head(select(chicago,-(city:dptp)))
i<-match("city",names(chicago))
j<-match("dptp",names(chicago))
head(chicago[,-(i:j)])  #without select, we could only use integer indecxes
#filter #same as select, we could directly refer to the variable names
chic.f<-filter(chicago,pm25tmean2>30) #directly refer to the variable names
head(chic.f,10)
dim(chic.f)
chic.f<-filter(chicago,pm25tmean2>30 & tmpd>80)
dim(chic.f)
#arrange
chicago<-arrange(chicago,date) #without arrange, this ordering would be hard to realize
head(chicago)
tail(chicago)
chicago<-arrange(chicago,desc(date))
head(chicago)
tail(chicago)
#rename
chicago<-rename(chicago,pm25=pm25tmean2,dewpoint=dptp)
head(chicago)
#mutate --> transform exsiting variables or to create new variables
#create a new variable
chicago<-mutate(chicago,pm25detrend=pm25-mean(pm25,na.rm=TRUE))
head(select(chicago, pm25, pm25detrend))
#group_by - split a data frame according to certain categorical variables
#first use mutate to create a categorical variable
chicago<-mutate(chicago, tempcat=factor(1*(tmpd>80),labels=c("cold","hot")))
head(chicago)
hotcold<-group_by(chicago,tempcat)
#summarize
summarize(hotcold, pm25=mean(pm25),o3=max(o3tmean2),no2=median(no2tmean2))
hotcold[is.na(hotcold$tempcat),] #one NA
#summarize each year
#first use mutate to create a categorical variable
chicago<-mutate(chicago,year=as.POSIXlt(date)$year+1900)
head(chicago)
years<-group_by(chicago,year)
head(years)
summarize(years,pm25=mean(pm25,na.rm=TRUE),o3=mean(o3tmean2),no2=median(no2tmean2))
#pipeline operator %>% -- chain operators together
#the idea is that you that a date set and you feed through a pipeline of operations to create a new data set
months<-chicago %>% mutate(month=as.POSIXlt(date)$mon+1) %>% group_by(month) %>% summarize(pm25=mean(pm25,na.rm=TRUE),o3=max(o3tmean2),no2=median(no2tmean2))
####dplyr could be used for data.table, SQL interface for relational databases via the DBI package


#7. Merging Data
if(!file.exists("./GetAndCleanData")){dir.create("./GetAndCleanData")}
fileUrl1<-"http://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2<-"http://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./GetAndCleanData/reviews.csv")
download.file(fileUrl2,destfile="./GetAndCleanData/solutions.csv")
reviews<-read.csv("./GetAndCleanData/reviews.csv")
solutions<-read.csv("./GetAndCleanData/solutions.csv")
head(reviews,2)
head(solutions,2)
names(reviews)
names(solutions)
#be default, data sets would merge by all common attributes
str(merge) #important parameters: x,y,by.x,by.y,all(default)
mergedData<-merge(reviews,solutions,by.x="solution_id",by.y="id",all=TRUE)
    #all=TRUE is full join, all = FALSE is natural join, all.x = TRU is left join, all.y = TRUE is right join
head(mergedData) #could find out that solution_id goes to the first column
#without telling merge by which variables
intersect(names(solutions),names(reviews))
?intersect #in {base}
mergedData2<-merge(reviews,solutions,all=TRUE) #full join
head(mergedData2)

#using join in the plyr package -- faster but less full featured -- default to left join
#could only merge based on common column names. couldn't merge based on different names, 
#as we didn't before joining based on solution_id and id, two different column names.
library(plyr)
df1<-data.frame(id=sample(1:10),x=rnorm(10))
df2<-data.frame(id=sample(1:10),y=rnorm(10))
#use join:
str(join) #function (x, y, by = NULL, type = "left", match = "all")  
#by: character vector of variable names to join by. If omitted, will match on all common variables.
#type of join: left (default), right, inner or full
#match: how should duplicate ids be matched? Either match just the "first" matching row, or match "all" matching rows. 
arrange(join(df1,df2),id) #arrange is just used for reordering
join(df1,df2)
df1<-data.frame(id=sample(1:10),x=rnorm(10))
df2<-data.frame(id=sample(1:10),y=rnorm(10))
df3<-data.frame(id=sample(1:10),z=rnorm(10))
dfList<-list(df1,df2,df3)
join_all(dfList)
?join_all













































