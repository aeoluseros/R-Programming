setwd("D:\\study\\DataScience")

#1. Combining Predictors/classifiers -- Ensembling Methods in Learning by averaging/voting
#Boosting, bagging and random forests are variants on this theme
#increase accuracy, but reduce interpretability
#Approach for combining classifiers:
   #Boosting, bagging and random forests -- Usually combine similar classifiers 
   #Modeling Stacking, Model ensembling -- Combine different classifiers
library(ISLR);data(Wage);library(ggplot2);library(caret)
Wage<-subset(Wage,select=-c(logwage))  # we leave out the logwage data beccause logwage is a pretty good predictor...
inBuild<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
buildData<-Wage[inBuild,]
validation<-Wage[-inBuild,]
inTrain<-createDataPartition(y=buildData$wage,p=0.7,list=FALSE)
training<-buildData[inTrain,]
testing<-buildData[-inTrain,]
dim(training);dim(testing);dim(validation)
# train
mod1<-train(wage~.,method="glm",data=training)
mod2<-train(wage~.,method="rf",data=training,trControl=trainControl(method="CV"),number=3)
# predict
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
# plot it: will demonstrate difference between model predictions + highlight how
# accurate / inaccurate the overall prediction was graphically
qplot(pred1,pred2,color=wage,data=testing)

#Fit a model that combines predictors
predDF<-data.frame(pred1,pred2,wage=testing$wage)
#instead of just fitting a model that relates the covariates to the outcome, I've fit two separate
   #prediction models for the outcome.
####so easy!!!!
combModFit<-train(wage~.,method="gam",data=predDF)  #GAM: Generalized additive models with integrated smoothness estimation
combPred<-predict(combModFit,predDF)

#Testing errors
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))

#Predict on Validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV <- predict(combModFit, predVDF)

#Validation errors
sqrt(sum((pred1V - validation$wage)^2))
sqrt(sum((pred2V - validation$wage)^2))
sqrt(sum((combPredV - validation$wage)^2))


#2. forecasting -- typically applied to things like time series data
#Characteristics:
   #Data are dependent over time
   #Specific pattern types: trends, seasonal patterns, cycles
   #subsampling into training/test is more complicated
   #similar issues arise in spatial data - dependency between nearby obs.; local specific effects
install.packages("quantmod")
#http://cran.r-project.org/web/packages/quantmod/quantmod.pdf
#Quandl:  https://www.quandl.com/
library(quantmod)   
from.date<-as.Date("01/01/08",format="%m/%d/%y")
to.date<-as.Date("12/31/13",format="%m/%d/%y")
getSymbols("GOOG",src="google",from=from.date,to=to.date) #Current src methods available are: yahoo, google, MySQL, FRED, csv, RData, and oanda.
head(GOOG)

setSymbolLookup(QQQ='yahoo',SPY='google')
getSymbols(c('QQQ','SPY'))  
# loads Ford as time series class ts 
getSymbols('F',src='yahoo',return.class='ts') 
# loads symbol from MySQL database (set with setDefaults)
#getSymbols('DIA', verbose=TRUE, src='MySQL') 
# load into a new environment
data.env <- new.env()
getSymbols("YHOO", env=data.env)
ls.str(data.env)
# assign into an attached environment
attach(NULL, name="DATA.ENV")
getSymbols("AAPL", env=as.environment("DATA.ENV"))
ls("DATA.ENV")
detach("DATA.ENV")

#Summarize monthly and store as time series
GOOG<-as.matrix(GOOG)
mGoog<-to.monthly(GOOG[,-ncol(GOOG)])
googOpen<-Op(mGoog)
ts1<-ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1",ylab="GOOG")

#time series decomposition
plot(decompose(ts1),xlab="Years+1") #Decompose a time series into seasonal, trend and irregular components using moving averages.
ts1Train<-window(ts1,start=1,end=5-0.01)
ts1Test<-window(ts1,start=5,end=(6-0.01))
ts1Train
ts1Test

#simple moving average  -- Y_t = 1/(2k+1) * sum{j=-k~k}(y_(t+j))
plot(ts1Train)
library(forecast)
lines(ma(ts1Train,order=3),col="red")

#simple exponential smoothing -- y_(t+1)_hat = alpha * y_t + (1-alpha) * y_(t-1)_hat 
#ets: Exponential smoothing state space model
ets1<-ets(ts1Train,model="MMM")  #multiplicative trend and multiplicative season with multiplicative error
   # model: The first letter denotes the error type ("A", "M" or "Z"); 
          # the second letter denotes the trend type ("N","A","M" or "Z"); 
          # and the third letter denotes the season type ("N","A","M" or "Z").
          # In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected. 
          # for example, "ANN" is simple exponential smoothing with additive errors, 
                    #"MAM" is multiplicative Holt-Winters' method with multiplicative errors, and so on.
   #damped trend: it approaches a constant some time in the future
names(ets1)
ets1$method
ets1$initstate
fcast<-forecast(ets1, h = 12)  #forecast the following 12 months
#forecast(object, h = ifelse(frequency(object) > 1, 2 * frequency(object), 10) , 
        #level=c(80,95), fan=FALSE, robust=FALSE, lambda=NULL, find.frequency=FALSE, ...)
   #h: Number of periods for forecasting. because frequency(ts1Train)==12, so by default forecast in the following 24 periods -- 2 years
   #level: Confidence level for prediction intervals.
   #fan: If TRUE, level is set to seq(50,99,by=1). This is suitable for fan plots.
        #if TRUE, fcast$level would be: 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99
        #if FALSE(default), fcast$level would be 80 95
   #robust: If TRUE, the function is robust to missing values and outliers in object. This argument is only valid when object is of class ts.
   #lambda: Box-Cox transformation parameter.
   #find.frequency:If TRUE, the function determines the appropriate period, if the data is of unknown period.
plot(fcast)
lines(ts1Test,col="red")

#get the accuracy
accuracy(fcast,ts1Test)

#see quantmod or quandl packages for finance-related problems


#3. Unsupervised Prediction
data(iris);library(ggplot2)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training);dim(testing)
#let's ignore the species clusters, and relabel the data based on clustering analysis
kMeans1<-kmeans(subset(training,select=-c(Species)),center=3)
training$clusters<-as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,color=clusters,data=training)
#Compare to real labels
table(kMeans1$cluster,training$Species)

#Build Predictors
modFit<-train(clusters~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)

#Apply on test
testClusterPred<-predict(modFit,testing)
table(testClusterPred,testing$Species)

#cl_predict function in the clue package provides similar functionality
install.packages("clue")
library(clue)
?cl_predict #Predict class ids or memberships from R objects representing partitions.
#cl_predict(object, newdata = NULL,type = c("class_ids", "memberships"), ...)

## Run kmeans on a random subset of the Cassini data, and predict the memberships for the "test" data set.
data("Cassini")
nr <- nrow(Cassini$x)
ind <- sample(nr, 0.9 * nr, replace = FALSE)
party <- kmeans(Cassini$x[ind, ], 3)
table(cl_predict(party, Cassini$x[-ind, ]), Cassini$classes[-ind])


#be aware of over-interpretation of clusters
#This is one basic approach to recommendation engines.

