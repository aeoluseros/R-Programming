setwd("D:\\study\\DataScience")

#1.Editing text variables
if(!file.exists("GetAndCleanData")){
        dir.create("GetAndCleanData")
}
fileUrl<-"http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./GetAndCleanData/cameras.csv") #if in MAC, should add option, method="curl" because of https instead of http
cameraData<-read.csv("./GetAndCleanData/cameras.csv")
names(cameraData)
head(cameraData)
tolower(names(cameraData)) #a good behavior is to make the variables names lower case
#fixing character vectors(remove special characters) -- strsplit()
str(strsplit) #function (x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
splitNames = strsplit(names(cameraData),"\\.")
class(splitNames) #list
splitNames[[5]]
splitNames[[6]]
###quick refresh of list
mylist<-list(letters=c("A","b","c"),numbers=1:3,matrix(1:25,ncol=5))
head(mylist)
#######
splitNames[[6]][1]
firstElement<-function(x) {x[1]}
sapply(splitNames,firstElement)

#other ways to manipulate text variables

if(!file.exists("./GetAndCleanData")){dir.create("./GetAndCleanData")}
fileUrl1<-"http://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2<-"http://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./GetAndCleanData/reviews.csv")
download.file(fileUrl2,destfile="./GetAndCleanData/solutions.csv")
reviews<-read.csv("./GetAndCleanData/reviews.csv")
solutions<-read.csv("./GetAndCleanData/solutions.csv")
names(reviews) #some names have _ in it
#sub()
#sub(pattern,replacement,x,ignore.case = FALSE,perl=FALSE,fixed=FALSE,useBytes=FALSE)
sub("_","",names(reviews))
#gsub() -- replace multiple instances of a particular character
#gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
# sub replaces only the first occurrence of a pattern whereas gsub replaces all occurrences
testName<-"this_is_a_test"
sub("_","",testName)
gsub("_","",testName)

#searching for specific values in variable names or in variables themselves
#grep(), grepl()
?grep #grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,fixed = FALSE, useBytes = FALSE, invert = FALSE)
#grep(value = FALSE) returns a vector of the indices of the elements of x that yielded a match.
#grepl(pattern, x, ignore.case = FALSE, perl = FALSE,fixed = FALSE, useBytes = FALSE)
#grep(value = TRUE) returns a character vector containing the selected elements of x
#grepl returns a logical vector (match or not for each element of x).
grep("Alameda",cameraData$intersection)
table(grepl("Alameda",cameraData$intersection)) #77*3
cameraData2<-cameraData[!grepl("Alameda",cameraData$intersection),]
dim(cameraData2) #77
grep("Alameda",cameraData$intersection,value=TRUE)
grep("JeffStreet",cameraData$intersection) #integer(0)
length(grep("JeffStreet",cameraData$intersection))

###more useful string functions
library(stringr)
nchar("Jeffrey Leek")
substr("Jeffrey Leek",1,7)
paste("Jeffrey","Leek")
paste("Jeffrey","Leek",sep="-")
paste0("Jeffrey","Leek") #no space in between
str_trim("Jeff    ") #Trim whitespace from start and end of string.
#str_trim(string, side = "both") #side: left, right or both

###Names of Variables should not have underscores or dots or white spaces.###

#2.Regular Expression - used to search text: 
#(1)whitespace word boundaries (2)sets of literals (3)the beginning and end of a line
#(4)alternatives("war" or "peace") Metacharacters to the rescue!

#Metacharacters
#(1)^(caret) represent the start of a line. ^i think matches lines such as "i think we all rule for participating"

#(2)$ represents the end of a line. morning$ matches the lines such as "I walked in the rain this morning"

#(3)[AsBbCc] means that we will accept all characters in the brackets

#(4)^[Ii] am will match: I am twittering from iPhone, and, i am boycotting the apple store

#(5)it could be range in []: ^[0-9][a-zA-Z] will match: 7th inning stretch or 2nd half soon to begin
    #notice that the order doesn't matter

#(6)when ^ is inside []: [^ ]:Matches a single character that is not contained within the brackets.
    #[^?.]$: any line that ends in anything other than a period or a question mark.
    #such as: i like basketballs   or   not in Bagdhad

#(7).(dot) is used to refer to any character. for example: 9.11 matches: its stupid the post 9-11 rules. front door 9:11:46 am.  

#(8)|(or) means alternatives. for example: flood|fire matches: its firewire like usb on none macs? the global flood makes sense
    #could include any # of alternatives: flood|earthquake|hurricane|coldfire
    #^[Gg]ood|[Bb]ad (alternatives could be regular expression and not just literals). 
        #Notice that [Gg]ood has to be at the beginning of line, but [Bb]ad doesn't have to be
        #therefore it matches: guess they had bad experience. Good to hear some good news
    #^([Gg]ood|[Bb]ad) will match: bad habbit, bad coordination today, Good Monday Holiday
        ####notice that () parenthesis only works as "Metacharacters" with alternative (|) or optional (?)

#(9)? indicates that the indicated expression is optional: 
    #Example: [Gg]eorge( [Ww]\.)? [Bb]ush       #\is the "escape" sign so that . is a literal period
    #i bet i can spell better than you and george bush combined.
    #a bird in the hand is worth two George W. Bushes

#(10)* and +: "* means any number, including none, of the preceding item", "+ means at least one of the preceding item"
    # (.*) means any characters in-between parenethsis, so it will match the lines:
      #anyone wanna chat? (24,m,germany)
      #hello, 20.m here...(east area + drives + webcam)
      #()
      #(he means older men)
    #[0-9]+ (.*)[0-9]+ means that any possible combination of numbers that are separated by something other than numbers, so it matches:
      #working as MP here 720 MP battallion, 42nd biradge
      #so say 2 or 3 years at colleage and 4 at uni makes us 23 when and if we fin
          #(.*)

#(11){} are referred to as interval quantifiers; the let us specify the minimum and maximum number of matches of an expression
    #[Bb]ush( +[^ ]+ +){1,5} debate: means [Bb]ush + (at least one space + followed by something not space + followed by at least one space) between 1 and 5 times
      #Bush has historically won all major debates he's done.
      #Bush doesn't need the debate?

#(12){m,n} means at least m but not more than n matches. {m} means exactly m matches. {m,}means at least m mathces

#(13)()parenthesis not only limit the scope of alternatives divided by a "|", but also can be used to "remember" text
     #matched by the subexpression enclosed. We refer to the matched text with \1, \2, etc.
     #for example:" +([a-zA-Z]+) +\1 +" will match: at least one space + at least one character + at lease one space + repeat of exact match as what we saw in the parenthesis + at least one space
        #time for bed, night night twitter! #night is repeated
        #blah blah blah blah  #blah is repeated
        #my tattoo is so so itchy today    #so is repeated

#(14)* is "greedy" so it always matches the longest possible string that satisfies the regular expression:
     #^s(.*)s matches:
        #sitting at starbucks.    #instead of matching just starbucks.
        #spaghetti with marshmallows
        #sore shoulders, stupid ergonomics
     #####to make it less greedy, we can use ? to change the greediness of the *
        #^s(.*?)s$
#regular expressions could be used with functions grep, grepl, sub, gsub and others that involve searching for text strings.


#3.Working with dates 
d1<-date()
d1
class(d1)  #"character"
d2<-Sys.Date()
class(d2)  #"Date"
#formatting dates
#%d=days as number(0-31), %a=abbr. weekday, %A=unabbr. weekday, %m=month(00-12), %b=abbr. month, %B=unabbr. month
#%y=2 digit year, %Y=four digit year
?format
format(d2, "%a %b %d")
format(d2, "%A %B %d")
x<-c("1jan1960","2jan1960","31mar1960","30jul1960")
z=as.Date(x,"%d%b%Y")
z;class(z)
(z[1]-z[2]);class(z[1]-z[2])
as.numeric(z[1]-z[2])
?julian #julian returns the number of days (possibly fractional) since the noon UTC on the first day of 4317 BC, with the origin as a "origin" attribute. All time calculations in R are done ignoring leap-seconds.
julian(Sys.Date(), -2440588) #today's Julian Date
julian(Sys.Date()) #difference between today and "1970-01-01"
Sys.Date()-as.Date("1970-01-01")  #same as above
floor(as.numeric(julian(Sys.time())) + 2440587.5)
.leap.seconds
weekdays(.leap.seconds)
months(.leap.seconds)
quarters(.leap.seconds)

###Lubridate package makes working with dates simpler than it is with the as.Date
#http://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
install.packages("lubridate")
library(lubridate)
?ymd #look for the year first, followed by the month, followed by the date
ymd("20140108")  #you don't have to format the date
dmy(010210)
mdy(010210)
mdy("08/04/2013")
dmy("03-04-2013")
ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03",tz="Pacific/Auckland")
Sys.timezone()
ymd_hms("2011-08-03 10:15:03",tz=Sys.timezone())
x=dmy(c("1jan1960","2jan1960","31mar1960","30jul1960"))
wday(x[1]) #TRUE will display the day of the week as a character string label such as "Sunday." 
           #FALSE will display the day of the week as a number.
           #Only available for wday
wday(x[1],label=TRUE)
yday(x[3])
mday(x[4])
#another example
arrive <- ymd_hms("2011-06-04 12:00:05", tz = "Pacific/Auckland")
leave <- ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")
second(arrive)
second(arrive) <- 25
arrive
wday(arrive)
wday(arrive, label = TRUE)
#with_tz: display the same moment in a different time zone
#force_tz: create a new moment by combining an existing clock time with a new time zone.
meeting <- ymd_hms("2011-07-01 09:00:00", tz = "Pacific/Auckland")
with_tz(meeting, "America/Chicago")
mistake <- force_tz(meeting, "America/Chicago")
with_tz(mistake, "Pacific/Auckland")
#Time Intervals
auckland <- interval(arrive, leave)
auckland <- arrive %--% leave
#As mentioned, my stay in Auckland lasted from June 4, 2011 to August 10, 2011
#Suppose my mentor at the University of Auckland, Chris, traveled to various conferences that 
#year including the Joint Statistical Meetings (JSM). Will my visit overlap with and his travels? Yes.
#For what part of my visit will Chris be there? use setdiff
jsm <- interval(ymd(20110720, tz = "Pacific/Auckland"), ymd(20110831, tz = "Pacific/Auckland"))
setdiff(auckland, jsm)  #available time to meet

#Arithmetic with date times
minutes(2);class(minutes(2))
dminutes(2);class(dminutes(2))   #"d"(for duration) 
eminutes(2)==dminutes(2);class(eminutes(2))   #"e"(for exact)
#Why two classes? Because the timeline is not as reliable as the number line(duration).
#The Duration class will always supply mathematically precise results. 
#A duration year will always equal 365 days. 
#Periods, on the other hand, fluctuate the same way the timeline does to give intuitive results. 
#This makes them useful for modeling clock times. For example, durations will be honest in the 
#face of a leap year, but periods may return what you want:
leap_year(2011)  ## regular year
ymd(20110101) + dyears(1)
ymd(20110101) + years(1)  #the same for regular year
leap_year(2012)  ## leap year
ymd(20120101) + dyears(1)  #"2012-12-31 UTC"
ymd(20120101) + years(1)   #"2013-01-01 UTC"

#if I wanted to set up a reoccuring weekly skype meeting with Hadley, it would occur on:
meetings <- meeting + weeks(0:5)
#Hadley travelled to conferences at the same time as Chris. Which of these meetings would 
#be affected? The last two.
meetings %within% jsm
#How long was my stay in Auckland?
auckland/edays(1)
auckland/edays(2)
auckland/eminutes(1)
#modulo and integer division
#%% indicates x mod y and %/% indicates integer division.
auckland%/%months(1)  #zheng yue
auckland%%months(1)   #duo duo tou 
as.period(auckland%%months(1))
as.period(auckland)

#Consider a simple operation, January 31st + one month. Should the answer be
        #(1)February 31st (which doesn't exist)
        #(2)March 4th (31 days after January 31), or
        #(3)February 28th (assuming its not a leap year)
#if adding or subtracting a month or a year creates an invalid date, lubridate will return an NA
jan31 <- ymd("2013-01-31")
jan31 + months(0:11)
#solution to the (2)
floor_date(jan31, "month") + months(0:11) + days(31)
#floor_date takes a date-time object and rounds it down to the nearest integer value of the 
#specified time unit.        #str(floor_date)
#solution to the (3)
jan31 %m+% months(0:11)


















