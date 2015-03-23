setwd("D:\\study\\DataScience")

#1.Introduction
#Machine Learning is about prediction
#The first step of designing machine-learning algorithm is Probability/Sampling
#Process: Question->input data->features->algorithm->parameters->evaluation
    #Question: - General question: Can I automatically detect emails that are SPAM that are not?
             # - Concrete: Can I use quantitative characteristics of the emails to classify them as SPAM/HAM?
    #input data: The SPAM example is in package kernlab
library(kernlab)
data(spam)
head(spam)
dim(spam)
    #features: we can calculate the frequency of particular words.
plot(density(spam$your[spam$type=="nonspam"]),col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
#x-axis is the frequency with 'your' appeared in the email.
#y-axis is the density, the number of times that frequency appears amongst the emails.
    #algorithm: find a cut-off constant C, is frequency of 'your' > C, then predict "spam"
#let's choose cut-off as 0.5
abline(v=0.5,col="black")
    #evaluation
prediction<-ifelse(spam$your>0.5,"spam","nonspam")
table(prediction,spam$type)/length(spam$type)
    #accuracy = 0.459+0.292 = 0.751  -->later on, we'll know this is an optimistic estimate of the overall error rate.(in-sample error rate)
#relative importance of steps: question > data > features > algorithms
    #frequently the question that you actually really want to answer, the data won't be available, or the 
    #data will be only avilable in a sort of tangential.
#common mistakes in feature slection: automate feature selection in a way that doesn't allow you to understand how those features are actually being applied to make good predictions.
                                     #not paying attention to data-specific quirks
                                     #throwing away information unnecessarily
   #you can automate feature selections somtimes with care. semi-supervised learning, or deep learning.
#in sample v.s. out of sample errors
library(kernlab);data(spam)
set.seed(333)
smallSpam<-spam[sample(dim(spam)[1],size=10),]
spamLabel<-(smallSpam$type=="spam")+1
plot(smallSpam$capitalAve,col=spamLabel,pch=19)
#capitalAve>2.7 = "spam", capitalAve<2.4 = "nonspam"; capitalAve between 2.40 and 2.45 = "spam"; capitalAve between 2.45 and 2.7 = "nonspam"
#prediction rule1
rule1<-function(x){
        prediction<-rep(NA,length(x))
        prediction[x>2.7]<-"spam"
        prediction[x<2.40]<-"nonspam"
        prediction[x>=2.40&x<=2.45]<-"spam"
        prediction[x>2.45&x<=2.70]<-"nonspam"
        return(prediction)
}
table(rule1(smallSpam$capitalAve),smallSpam$type)
#prediction rule2
rule2<-function(x){
        prediction<-rep(NA,length(x))
        prediction[x>2.8]<-"spam"
        prediction[x<=2.8]<-"nonspam"
        return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)  #will miss one value
table(rule1(spam$capitalAve),spam$type)
table(rule2(spam$capitalAve),spam$type)
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)  #more accurate for the whole sample when we use simplified rule
#the reason that the simplied rule is better than the more complicated rule is overfitting:
#Data have two parts: signal and noise. The goal of a predictor is to find signal.
#you can always build a prefect in-sample predictor in any samll dataset, but you will capture both signal and noise when you do that.

#prediction study design
   #split data into: training set, testing set, validation set.
   #in the training set, you pick up features and prediction function using cross-validation
   #if no validation set, apply only the best model we have to test set 1 time.
   #if validation set exists, apply predication models to test set and refine, apply only the best one 1 time to validation set.
#In general, randomly sample training and test. 
        #if you have a large sample size: 60% training, 20% test, 20% validation. 
        #if you have a medium sample sizeL 60% training, 40% test
        #small sample size: Do cross validation, report caveat of small sample size
#Your data sets must reflect structure of problem.
   #for example, if predictions evolve with time, split training/test in time chunks (called backtesting in finance - to use chunks of data that consist of obs. over time)

#type of errors (different name in machine learning)
#TP+FN = 1; #FP+TN = 1
   #sensitivity (Pr(positive test|disease) = TP/(TP+FN))   #TP:True Positive(good) | FN:False Negative(bad)
   #specificity (Pr(negative test|no disease)=TN/(FP+TN))  #TN:True Negative(good) | FP:False Positive(bad)
   #positive predictive value (Pr(disease|positive test)=TP/(TP+FP))
   #Negative predictive value (Pr(no disease|negative test)=TN/(FN+TN))
   #Accuracy((TP+TN)/(TP+FP+FN+TN))
  #The above four could be easiliy recognized once you draw the table.
      #In relation to Bayesian statistics, the sensitivity and specificity are the conditional 
  #probabilities, the prevalence is the prior, and the positive/negative predicted values are 
  #the posterior probabilities.

####If we predict a rare event, you have to be aware of how rare that event is.
  #when it's very rare, then the negative predictive value would be small even the screen machenism is relatively good.
  #you're getting a lot of false positives that are as a fraction of the total number of positives that you are getting.
  #This goes back to the idea of knowing what population you're sampling from when you're building a predictive model.
#for cts data, we use MSE and RMSE(root MSE)
#Common error measures:
  #1. MSE/RMSE: cts data, data sensitive to outliers
  #2. Median absolute deviation: cts data, often more robust
 #Sensitivity and Specificity are commonly used when you care about one type of error more than the other type of error.
  #3. Sensitivity (recall): if you want few missed positives
  #4. Specificity: if you want few negatives called positives
  #5. Accuracy: weights false positives/negatives equally
  #6. Concordance: One example is Kappa.

#An ROC curve is the most commonly used way to visualize the performance of a binary classifier, 
#and AUC(area under the curve) is (arguably) the best way to summarize its performance in a single number.

#ROC Curves(receiver operating characteristics curves) -- test the quality or goodness of a prediction algorithm
#In binary classification, you're predicting one of two categories. The cutoff point you choose gives you different results.
#on x-axis, P(False Positive Rate) or 1-specificity(FP/(FP+TN)=1-TN/(FP+TN))
#y-axis,P(True Positive Rate) or sensitivity, 
#every single point along the curve corresponds to exactly one cutoff
  #so for a particular cutoff, you get a certain probablity of being a false positive or a certain 1-specificity(FP/(FP+TN)=1-TN/(FP+TN))
  #from the ROC curve, we could see that sensivity and specificity sometimes are oppositely related.
#The area under ROC curve(AUC) quantifies the goodness of an algorithm.
#AUC = 0.5: random guessing. AUC = 1: perfect classifier(sensitivity and specificity are both 1). AUC>=0.8: good classifier.

#Cross Validation - detecting relevant features and building models and estimating parameters.
#we use cross validation to estimate the "test set accuracy" with the training set.
#Approach:
     #use the training set, split it into training/test sets(subsampling), build a model on the training set, evaluate on test set.
     #repeat above and average the estimated errors.
#Used for:
     #Picking variables to include in a model, #picking the type of prediction function to use
     #picking the parameters in the prediction function, #comparing different predictors

#random subsampling cross validation
#k-fold cross validation: for example, 3-fold cross validation. (see video 1-8 4:00)
   #larger K = less bias, more variance
   #smaller k = more bias, less variance
#leave-one-out cross validation: just leave one sample out every time.

#Considerations:
     #for time series data, data must be used in "chunks" -- you have to get blocks of time that are all 
            #contiguous, because time point might depend on all the time points that came previously. You will
            #ignore a huge, rich structure in the data if you just randomly take samples. 
     #random sampling must be done without replacement, even though we have to break our training setup to smaller samples
            #random sampling with replacement is called bootstrap(inference class). -- underestimates of the error (can be correct by 0.632 bootstrap, but it is complicated)
                        #(http://stats.stackexchange.com/questions/96739/what-is-the-632-rule-in-bootstrapping)
     


#2.Caret Functionality - Preprocessing(preProcess) 
                      #data splitting(createDataPartition, createResample, createTimeSlices),
                      #training/testing functions(train, predict)
                      #model comparison(confusionMatrix)
#machine learning algorithms in R: #Linear Discriminant analysis #Regression #Naive Bayes #SVM: support vector machine
                #Classification and Regression Trees  #Random Forests  #Boosting
#predict function syntax:
      #lda(MASS):Linear Discriminant Analysis. predict(obj) #no option needed
      #glm(stats): Generalized linear models. predict(obj,type="response")
      #gbm(gbm): Generalized Boosted Regression Models. predict(obj,type="response,n.trees)
      #mda(mda): Mixture and flexible discriminant analysis. predict(obj,type="posterior")
      #rpart(rpart): Recursive Partitioning and Regression Trees. predict(obj,type="prob")
      #Weka(RWeka): Waikato Environment for Knowledge Analysis. predict(obj, type="probability")
      #LogitBoost(caTools): logitboost classification algorithm. predict(obj,type="raw",nIter)

#SPAM Example: Data Splitting
library(caret);library(kernlab);data(spam)
#A series of test/training partitions are created using createDataPartition 
                #while createResample creates one or more bootstrap samples(simple random sampling is used)
#createFolds splits the data into k groups
#while createTimeSlices creates cross-validation sample information to be used with time series data.
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)  
        #the random sampling is done within the levels of y when y is a factor
        #For numeric y, the sample is split into groups sections based on percentiles and sampling is done within these subgroups.
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)
dim(testing)
#fit a model
install.packages("e1071")
library(e1071)
#e1071: Misc Functions of the Department of Statistics
set.seed(32343)
modelFit<-train(type~.,data=training,method="glm")
#train(formula, data, ..., weights, subset, na.action, contrasts = NULL)
#result of train function:
   #method, modelType, results(a data frame the training error rate and values of the tuning parameters.),
   #bestTune(a data frame with the final parameters), call(the (matched) function call with dots expanded)
   #metric(a string that specifies what summary metric will be used to select the optimal model.), control(the list of control parameters.)
   #preProcess,finalModel,trainingData,resample,perfNames,maximize(a logical recycled from the function arguments.),
   #yLimits,times
modelFit
str(modelFit)
summary(modelFit)
names(modelFit)
modelFit$finalModel
modelFit$metric
modelFit$perfNames
modelFit$bestTune
modelFit$call
modelFit$modelType
modelFit$results
modelFit$maximize
modelFit$yLimits
modelFit$times
#predict
predictions<-predict(modelFit,newdata=testing)
predictions
#table(predictions,testing$type)
confusionMatrix(predictions,testing$type)  #table's entension version
#another regression example
library(mlbench)
data(BostonHousing)
lmFit <- train(medv ~ . + rm:lstat, data = BostonHousing, method = "lm")
library(rpart)
rpartFit <- train(medv ~ .,data = BostonHousing,method = "rpart",tuneLength = 9)


###Data Slicing:
#normal data splitting:
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)  
training<-spam[inTrain,]
testing<-spam[-inTrain,]
#k-fold splitting:
set.seed(32323)
trainFolds<-createFolds(y=spam$type,k=10,list=TRUE, returnTrain=TRUE)
length(spam$type)
#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
    #list: logical - should the results be in a list (TRUE) or a matrix with the number of rows equal to floor(p * length(y)) and times columns
    #returnTrain: a logical. When true, return training set. when false, return test set. This argument only works in conjunction with list = TRUE
sapply(trainFolds,length)  #4140-4142 (around 90% of the length of spam$type)
length(spam$type)  #4601
trainFolds[[1]][1:10]
trainFolds[[2]][1:10]
trainFolds[[3]][1:10]
trainFolds[[4]][1:10]
trainFolds[[9]][1:10]
trainFolds[[10]][1:10]
set.seed(32323)
testFold<-createFolds(y=spam$type,k=10,list=TRUE, returnTrain=FALSE)
testFold[[1]][1:10]
testFold[[2]][1:10]
testFold[[3]][1:10]
testFold[[4]][1:10]
testFold[[9]][1:10]
testFold[[10]][1:10]
spam$type[-testFold[[10]]]==spam$type[trainFolds[[10]]] #All True's
#Resampling -- Bootstrapping
set.seed(32323)
bootFolds<-createResample(y=spam$type,times=10,list=TRUE) #times: the number of partitions to create
sapply(bootFolds,length)
bootFolds[[1]][1:10]
bootFolds[[2]][1:10]
bootFolds[[3]][1:10]
bootFolds[[4]][1:10]
bootFolds[[9]][1:10]
bootFolds[[10]][1:10]
#time slices:
#createTimeSlices(y, initialWindow, horizon = 1, fixedWindow = TRUE)
   #y: For createTimeSlices, these should be in chronological order.
   #initialWindow: The initial number of consecutive values in each training set sample
   #horizon: The number of consecutive values in test set sample
   #fixedWindow: A logical: if FALSE, the training set always start at the first sample.
set.seed(32323)
tme<-1:1000
timeFolds<-createTimeSlices(y=tme,initialWindow=20,horizon=10) #I want to predict the next 10 samples out after I take the initial window of 20.
names(timeFolds)
timeFolds$train[[1]]
timeFolds$test[[1]]


#training control options
library(caret);library(kernlab);data(spam)
set.seed(32343)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)  
training<-spam[inTrain,]
testing<-spam[-inTrain,]
modelFit<-train(type~.,data=training,method="glm")
names(modelFit)
modelFit$finalModel

#train.options
args(train.default)
#function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL, 
#          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), maximize = ifelse(metric == "RMSE", FALSE, TRUE), 
#          trControl = trainControl(), tuneGrid = NULL, tuneLength = 3) 
#we can use "weights" to upweight or downweight certain observations
#in metric: for cts outcomes, we can use RMSE or R^2(a measure of linear agreement.not that useful for non-linear things like random forest) 
           #for categorical outcomes, we can use Accuracy or Kappa((Pr(a)-Pr(e))/(1-Pr(e)))

#trainControl:
#trainControl(method = "boot", number = ifelse(grepl("cv", method), 10, 25),
#             repeats = ifelse(grepl("cv", method), 1, number), p = 0.75, initialWindow = NULL, horizon = 1,
#             fixedWindow = TRUE, verboseIter = FALSE, returnData = TRUE, returnResamp = "final",
#             savePredictions = FALSE, classProbs = FALSE, summaryFunction = defaultSummary,selectionFunction = "best",
#             preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5), index = NULL, indexOut = NULL,
#             timingSamps = 0, predictionBounds = rep(FALSE, 2), seeds = NA, adaptive = list(min = 5, alpha = 0.05, 
#             method = "gls", complete = TRUE), allowParallel = TRUE)
   #method: bootstrapping or cross validation. 
       #boot = bootstrapping, boot632 = bootstrapping(reduce bias) that adjusts for the fact that multiple samples are repeatedly resampled when you're doing sub-sampling 
       #cv = cross-validation, repeatedcv = repeated cross-validation, LOOCV = leave one out cross validation
   #number: number of times to do bootstrapping or cross validation; number of subsamples to take
   #repeats: how many times to repeat that subsampling process if you want to be careful about.
   #p: size of the training set. 
   #other parameters depend on the specific problems you're working on. for example, for time course data, initialWindow
           #tells you the size of time points in the training data. horizon will be the # of time points that you'll be predicting.
   #savePredictions: return the actual predictions from each of the iterations when it's building the model
   #summaryFunction: you can return a different summary
   #preProcessing, predicitonBounds, parallelizing.
   #seeds: it's often used to set an overall seed; if you do parallel computation, you can also set a seed for each resampling
#example:
set.seeds(1235)  #it will generate a set of random numbers that is consistent.
modelFit3<-train(type~.,data=training,method="glm") #if I fit model like this, then when it generates bootstrap samples,
                                         #it will generate those bootstrap samples according to the random number that come for the seed 1235
modelFit3

#another example:
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)
seeds[[51]] <- sample.int(1000, 1)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     seeds = seeds)
set.seed(1)
mod <- train(Species ~ ., data = iris, 
             method = "knn", 
             tuneLength = 12,
             trControl = ctrl)
ctrl2 <- trainControl(method = "adaptive_cv", 
                      repeats = 5,
                      verboseIter = TRUE,
                      seeds = seeds)
set.seed(1)
mod2 <- train(Species ~ ., data = iris, 
              method = "knn", 
              tuneLength = 12,
              trControl = ctrl2)



###plotting predictors - to understand how the data actually look and how the data interact with each other
#things you should be looking for:
    #-Imbalance in outcomes/predictors: if you see all of the predictors tend to be one value in the 
          #one outcome group and not in another outcome group, then you see that's a good predictor.
          #but if you see that you only have three of one outcome and 150 of the other outcome, that means
          #it's going to be very hard to build an accurate classifier between those two classes.
    #-Outliers: they might suggest that there are some variables you're missing.
    #-Groups of points not explained by any predictor
    #-Skewed variables: you may want to transform and make look better, more sort of nicely normally
          #distributed if you're using things like regression models, but that may not matter as much as 
          #if you're using more of machine learning methods.
#let's use Wage data in ISLR(introduction to statistical learning) package. 
library(ISLR); library(ggplot2); library(caret)
data(Wage)
summary(Wage)
set.seed(12345)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training);dim(testing)
#feature plot
#use caret
library(caret)
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")
#outcome(topright) is the wage. #we can see a trend between education and wage
#use qqplot2 plotting system
qplot(age,wage,data=training)
qplot(age,wage,data=training,color=jobclass)
#so we could see that most of points in the upper chunk come from the information-based jobs.
#this may help you detect which variable is important in your model.

#add regression smoothers(ggplot2)
qq<-qplot(age,wage,color=education,data=training)
qq+geom_smooth(method="lm",formula=y~x)
#you can see for every different education class, it fits a linear model.

#break wage variable into different categories (Hmisc package)
library(Hmisc)  #Harrell Miscellaneous  #see Data_Cleaning-3-plyr&dplyr&reshape.R
cutWage<-cut2(training$wage,g=3)
table(cutWage)
#boxplot with cut2
p1<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p1
p2<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
p2
if(!require("gridExtra")){install.packages("gridExtra")}
library("gridExtra")
grid.arrange(p1,p2,ncol=2)
#we could see that there's a large number of dots in each of the different boxes and so that suggest
   #that any trend may actually be real. If you observe just a few dots in the one boxes, maybe it means that
   #the particular box isn't very well representative of what the data actually look like.
#Table
t1<-table(cutWage,training$jobclass)
t1
prop.table(t1,1)  #proportional to the fist dimension -- row (sum of each row = 1)
#Density plots - If you want to see how all the distributions change by group, density plot will be useful
qplot(wage,color=education,data=training,geom="density",lwd=1)
#we can see that <HS Grad have more value down and centralized
#college degree and advanced degree have two small bumps in the right-hand side.


#Preprocessing Predictor Variables:
  #you need to plot the variables upfront so you can see if there is any sort of weird behavior of those variables
  #sometimes, predictors will look very strange or the distribution will be very strange and you might need to 
  #transform them in order to make them more useful for prediction algorithms (
  #particularly model-based algorithms like linear discriminant analysis, naive Bayes, linear regressions
library(caret); library(kernlab); data(spam)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]   #if list = TRUE in the above function, we could subset spam with list inTrain.
testing<-spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")
  #we can see almost all of the run links are very small, but there are a few that are much, much larger.
mean(training$capitalAve)
sd(training$capitalAve)
trainCapAve<-training$capitalAve
trainCapAveS<-(trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
#we must adjust the test set with parameters of the training set.
testCapAve<-testing$capitalAve
testCapAveS<-(testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)
#we could also use preProcess function
names(training[,58]) #the 58th column is the actual outcome 
preObj<-preProcess(training[,-58],method=c("center","scale"))  #record center and scale of every variable in the training set, and defined preObj as preProcess class.
trainCapAves<-predict(preObj,training[,-58])$capitalAve
  ## Default S3 method:
  #preProcess(x, method = c("center", "scale"), thresh = 0.95, pcaComp = NULL,na.remove = TRUE,k = 5, 
          #knnSummary = mean, outcome = NULL,fudge = .2, numUnique = 3,verbose = FALSE,...)
  ## S3 method for class 'preProcess'
  #predict(object, newdata, ...)
testCapAves<-predict(preObj,testing[,-58])$capitalAve  #apply to the test set
mean(testCapAves)
sd(testCapAves)

#we can also pass the preProcess object directly to the train function as an argument
set.seed(32343)
modelFit<-train(type~.,data=training,preProcess=c("center","scale"),method="glm")
modelFit

#Other kinds of transformation -- Box-Cox Transformation
preObj<-preProcess(training[,-58],method=c("BoxCox"))  #Box-Cox takes cts data and try to make them look like normal data, by estimating a specific data set of parameters using MLE.
trainCapAves<-predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2));
hist(trainCapAves) #you could see the data are more like a normal distribution, but it doesn't take 
    #care of all of the problems. There's still a stack set of values at zero.
qqnorm(trainCapAves)  #normal qq plot  #you could see data are chunked down in the bottom.
sum(trainCapAves==0)  #245 values are equal to 0, Box-Cox as a cts transform doesn't take care of values that are repeated.
                #so it doesn't take care of a lot of the problems that would occur using a variable that's highly skewed.

#we could impute data for these data sets
#We also do this for missing data, prediction algorithms are built not to handle missing data in most cases.

#make some values NA
set.seed(13343)
training$capAve<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA]<-NA
#impute and standardize
preObj<-preProcess(training[,-58],method="knnImpute")  #will also automatically standardize.
#install.packages("RANN")  #required for knnImpute
capAve<-predict(preObj,training[,-58])$capAve
#standardize true values
capAveTruth<-training$capitalAve
capAveTruth<-(capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)     #most value are close to zero, so the imputation works well
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA]) #we can see these values that are not imputed are closer to capAveTruth.

#All the transformations talked are for cts data.careful when transforming factor variables. It's more difficult to know what is the right transformation.
  #most machine learning algorithms are designed to deal with binary predictors, in which the binary predictors are not preprocessed, 
     #and cts data which are preprocessed to be more normal.


###Covariate Creation
#Covariate are somtimes called predictors and sometimes called features
#They're the variables that you will actually include in your model that you're going to be using 
   #to combine them to predict whatever outcome that you care about.
#Two levels/steps of covariate creation: 
#1. from raw data(text file, image, website) to covariate 
#no example
#the more knowledge of the system you have the better the job you will do.
#when in doubt, err on the side of more features

#use Google "feature extraction for [data type]" to search for features

#2. transforming tidy covariates to more useful covariates
#should be done only on the training set
#more necessary for some methods(regression, svms) than for others(classification trees)
#the best approach is through EDA(plotting/tables)
#New covariates should be added to data frame

#be careful about overfitting!

#e.g.:
library(kernlab); data(spam)
spam$capitalAveSq<-spam$capitalAve^2

#a full e.g.
library(ISLR);library(caret);data(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
#common covariates to add: turn qualitative or factor covariates into dummy variables(quantitative)
table(training$jobclass)
dummies<-dummyVars(wage~jobclass,data=training)  #outcome~predictor variable
head(predict(dummies,newdata=training))
#removing zero-variability covariates
   #nzv: nearZeroVar diagnoses predictors that have one unique value (i.e. are zero variance predictors)  
   #or predictors that are have both of the following characteristics: they have very few unique 
   #values relative to the number of samples and the ratio of the frequency of the most common value 
   #to the frequency of the second most common value is large.
#for example, we have a feature that says for emails, does it have any letters in it at all? 
   #almost every single email will have at least one letter in it, so that variable will always
   #be equal to true. So the feature has no variabilty and it's probabiliy not going to be useful.
nsv<-nearZeroVar(training,saveMetrics=TRUE)  #near-zero variability
   #freqRatio: the ratio of frequencies for the most common value over the second most common value
   #percentUnique: the percentage of unique data points out of the total number of data points

#Spline basis - splines package
library(splines)
bsBasis<-bs(training$age,df=3) # B-spline function will create a 3-degree polynomial variable. 
bsBasis  #three columns' coefficients: original age variable, age squared, age cubed
#fitting curves with splines
par(mfrow=c(1,1))
plot(training$age,training$wage,pch=19,cex=0.5)
lm1<-lm(wage~bsBasis,data=training)
predictValue<-predict(lm1,newdata=training)
points(training$age,predictValue,col="red",pch=19,cex=0.5)
#splines on the test set
#Importnat in Machine Learning:
#on the test set, you have to predict the same variable -- you have to create the variable on 
   #the test set using the exact same procedure that you used on the training set,
   #instead of directly applying the BS function to the age variable in test set, which would create
   #a new set of variables on the test set that isn't relaed to the variables that you create on the 
   #training set and may introduce some bias.
testBasis<-predict(bsBasis,newx=testing$age)   
#identical(bsBasis, predict(bsBasis, testing$age))
getAnywhere(predict.bs)

#if you want to fit spline models, use the "gam"(generalized additive model using Splines) method in caret, which allows smoothing of multiple variables.
#caret uses gam from package mgcv
#e.g.:
library(mgcv)
dat <- gamSim(1,n=400,dist="normal",scale=2) #Function used to simulate data sets to illustrate the use of gam and gamm. 
test<-train(y~x1, data=dat, method='gam') # Next time, provide any data like this.
test$finalModel$fitted.values
#Using method = "gam" gets you gam() from the mgcv package and using "gamLoess" and "gamSpline" use gam() from the gam package.

#Preprocessing with PCA
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)  
training<-spam[inTrain,]
testing<-spam[-inTrain,]
M<-abs(cor(training[,-58]))
diag(M)<-0
index<-which(M>0.8,arr.ind=T)  #could be used in pairs trading
   #which (x, arr.ind = FALSE, useNames = TRUE)
   #logical; should array indices be returned when x is an array
index
M[32,31]
M[34,31]
paste(rownames(M)[index[,1]], colnames(M)[index[,2]], sep=", ")
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

#rotate the plot
x<-0.71*training$num415+0.71*training$num857
y<-0.71*training$num415-0.71*training$num857
plot(x,y) #we could see that most variability is happening in the x-axis, but most points are
          #clustered at 0 on the y-axiss (almost all of these points have y=0)
#prcomp
smallSpam<-spam[,c(34,32)]
prComp<-prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])  #similar to the above plot
#PCA on Spam data
typeColor<-((spam$type=="spam")+1) #black ones are spam, red ones are ham
prComp<-prcomp(log10(spam[,-58]+1))  #use log10 to make data look more Gaussian because some variable are normal looking, some others are skewed.
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab='PC1',ylab='PC2')
#we could see that long PC1, there is separation of the ham messages from the spam messages.
#do the principal component in preProcess
preProc<-preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)  #pcaComp: # of pc to computer
#The preProcess function normalizes the data and then performs PCA
   #the preProcess equivalent to prcomp(scale(log10(spam[,-58]+1))) or prcomp(log10(spam[,-58]+1),scale. = TRUE).
      #prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE,  tol = NULL, ...)
spamPC<-predict(preProc,log10(spam[,-58]+1))  #calculate the values of each new principal components
head(spamPC)
str(spamPC)
plot(spamPC[,1],spamPC[,2],col=typeColor)
#####full steps:
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))   #predict on training data
modelFit <- train(training$type ~ .,method="glm",data=trainPC)  #change the data.
modelFit

testPC<-predict(preProc,log10(testing[,-58]+1))
fitValue<-predict(modelFit,testPC)  #we also need to use testPC instead of testing data
confusionMatrix(testing$type,fitValue)

#integrate pcs into the train -- now you couldn't select #of PC's 
#By default, caret keeps the components that explain 95% of the variance.
modelFit<-train(training$type~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

#watch out for outliers:
  #transform first (with logs/Box Cox)
  #plot predictors to identify problems
#PCs are more useful for linear-type models
#can make it harder to intepret predictors


###Predicting with Regression
#often poor performance in nonlinear settings
library(caret);data(faithful);
head(faithful)
set.seed(333)
inTrain<-createDataPartition(y=faithful$waiting,p=0.75,list=FALSE)
trainFaith<-faithful[inTrain,]
testFaith<-faithful[-inTrain,]
head(trainFaith)
#fit a linear model
lm1<-lm(eruptions~waiting,data=trainFaith)
summary(lm1)
names(lm1)
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted.values,lwd=3)
?lines  #lines(x, y = NULL, type = "l", ...)

#predict a new value
#method 1
coef(lm1)[1]+coef(lm1)[2]*6
#method 2
newdata<-data.frame(waiting=6)
predict(lm1,newdata)  #newdata should be data frame
predict(lm1,newdata=testFaith)

#predFun<-function(x){predict(lm1,data.frame(waiting=x))}
#predFun(80)
#fitTest<-sapply(testFaith$waiting,predFun)
#as.numeric(fitTest)

#plot predictions
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

#get training set/test set errors
#RMSE
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
#prediction intervals
par(mfrow=c(1,1))
pred1<-predict(lm1,newdata=testFaith,interval="prediction") #interval="prediction"
ord<-order(testFaith$waiting)  #return the index of ordered vector  # seems we don't have to order actually.
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty=c(1,1,1),lwd=3)
#matlines: Plot the columns of one matrix against the columns of another.
args(matlines)
#function (x, y, type = "l", lty = 1:5, lwd = 1, pch = NULL, col = 1:6, ...)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting,pred1,type="l",,col=c(1,2,2),lty=c(1,1,1),lwd=3)

#same process with caret
library(caret)
modFit<-train(eruptions~waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)

##multiple regression
library(ISLR);library(ggplot2);library(caret);data(Wage)
names(Wage)
Wage<-subset(Wage,select=-c(logwage))
summary(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training);dim(testing)
#first thing: feature plot:
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")
?featurePlot #A shortcut to produce lattice graphs
pairs(training[,c("age","education","jobclass","wage")]) #same but the color and sequence
#seems to be two separate groups for the wage and age, giving us some indication to analyze
qplot(age,wage,data=training)
#qplot(age,wage,data=training)+geom_smooth(method="lm",formula=y~x)
qplot(age,wage,data=training,color=jobclass)
qplot(age,wage,data=training,color=education)
#we could see information and advanced degree acount or most of high-wage peopple.
?qplot
#fit a linear model
modFit<-train(wage~age+jobclass+education,method="lm",data=training)
finMod<-modFit$finalModel
print(finMod)
par(mfrow=c(2,2))
plot(finMod)
par(mfrow=c(1,1))
plot(finMod,1,pch=19,cex=0.5,col="#00000010")  #plot functions: plot(functions,the # of the funciton)
#we could see some outliers above, labeled with numbers. those variables might be variables
   #that we want to explore a little bit further to see if we can identify any other predictors

#color by variables not used in the model
qplot(finMod$fitted,finMod$residuals,color=race,data=training)
#we can see some of the outliers above may could be explained by the race variable: white and black
#plot by index
plot(finMod$residuals,pch=19)  #index(row number) v.s. residuals
#we can see that high residuals seem to be happening at the highest row numbers.
#we can also see a trend with respect to row numbers.
#whenever you can see a trend or a outlier w.r.t. row numbers, it suggests there is a variable missing from your model.

#predicted v.s. truth in test set
pred<-predict(modFit,testing)
qplot(wage,pred,color=year,data=testing)

#if you want to use all covariates
modFitAll<-train(wage~.,data=training,method="lm")
pred<-predict(modFitAll,testing)
qplot(wage,pred,data=testing)














