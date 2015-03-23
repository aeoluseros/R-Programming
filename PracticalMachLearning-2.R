setwd("D:\\study\\DataScience")

#1. prediction with trees
   #pros: easy to interpret; better performance in nonlinear settings
   #cons: without pruning/cross-validation can lead to overfitting; results may be variable
#basic algorithm
   #start with all variables in one group
   #find the variable/split that best separates the outcomes, and divide data into two groups(leaves) on that split(nodes)
   #within each split, find the best variable/split that separates the outcomes
   #continue until the groups are too small or sufficiently "pure"
#measure of impurity:
   #misclassification error(1-percentage of classifying to one leaf on a particular split) (0=perfect purity; 0.5=no purity)
   #Gini Index: 1-sum{k=1~K}((p_mk)^2). (0=perfect purity; 0.5=no purity)
   #Deviance/Information Gain: -sum{k=1~K}((p_mk)*log2(p_mk))  (0=perfect purity, 1=no purity = -0.5*(-1)-0.5*(-1))
#Classification Trees use interactions between variables
#Data transformation may be less important.
data(iris)
library(ggplot2)
names(iris)
table(iris$Species)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training);dim(testing)
#iris petal widths/sepal width
qplot(Petal.Width,Sepal.Width,color=Species,data=training)
#we could see three distinct classes. It might be a little bit challenging for a linear model.
library(caret)
modFit<-train(Species~.,method='rpart',data=training)
#rpart: Recursive Partitioning and Regression Trees. predict(obj,type="prob")
print(modFit$finalModel)
par(mar=c(2,2,2,2))
plot(modFit$finalModel,uniform=TRUE,main="Classification Tree")  
     #notice that this dendrogram is drawn in different method in the dendrogram in EDA
text(modFit$finalModel,use.n=TRUE,all=TRUE,cex=0.8)
#prettier plots
if(!require("rattle")){install.packages("rattle")}
library(rattle)
if(!require("rpart.plot")){install.packages("rattle")}
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)
#predict new values
testPredict<-predict(modFit,newdata=testing)
table(testPredict,testing$Species)

#Regression Trees can also be used for regression problems(cts. outcome)

#2. Bagging -- short for bootstrap aggregating.
#the basic idea is that when you fit complicated models, sometimes if you average those models together,
#you get a smoother model fit. That gives you a better balance between potential bias and variance in your fit.
#Similar bias for each model and reduced variance
#more useful for non-linear funtions
#Basic Idea:
   #(1)Resample cases and recalcualte predictions
   #(2)Average or majority vote
if(!require("ElemStatLearn")){install.packages("ElemStatLearn")}
library(ElemStatLearn); #data(ozone,package="ElemStatLearn")
ozone<-ozone[order(ozone$ozone),]
head(ozone)
dim(ozone)
#predict temperature as a function of ozone.
#Bagged loess(Bagged Local Polynomial Regression Fitting)
ll<-matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss<-sample(1:dim(ozone)[1],replace=T) #length(ss) = 111, #why we use substituted sampling here?
        #print(ss)   #if we use replaece=F, then all training sets are the same. Therefore the prediction results are the same.
                     #Bootstrapping means sampling with replacement, so bagging also randomizes the size the training set.
        #we ss = 109, we could retreive the 109th row from ozone (be careful, not the row labeled as 109 but the 109th row counting from top)
        ozone0<-ozone[ss,];  #if retrieve the 63th row twice, the second row would be labeled as 63.1
        ozone0<-ozone0[order(ozone0$ozone),]
        loess0<-loess(temperature~ozone,data=ozone0,span=0.2)  #the only predictor is "ozone"
        ll[i,]<-predict(loess0,newdata=data.frame(ozone=1:155))
}
par(mar=c(5.1,4.1,4.1,2.1))
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=1)}
lines(1:155,apply(ll,2,median),col="red",lwd=2)

#some models perform bagging for you, in "train" function consider "method" options.
   #-bagEarth: A bagging wrapper for multivariate adaptive regression splines (MARS) via the earth function
   #-treebag; 
   #-bagFDA:A bagging wrapper for flexible discriminant analysis (FDA) using multivariate adaptive regression splines (MARS) basis functions
#alternatively, you can bag any model you choose using the bag function.
#bag(x, y, B = 10, vars = ncol(x), bagControl = NULL, ...)
if(!require("party")){install.packages("party")}
library(party)
predictors<-data.frame(ozone=ozone$ozone)
temperature=ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,  #the function (ctree in this case) applied to fit the model
                                       predict = ctreeBag$pred, #a call to the predict function from a trained model
                                         #ctreeBag$pred(object, x): takes into object created by ctreeBag and a new data set x, and it's going to get a new prediction
                                       aggregate = ctreeBag$aggregate))  #the way that we'll put predictions together and take the median
#bagControl: ldaBag, plsBag, nbBag, ctreeBag(conditional regression tree), svmBag, nnetBag

#a function in ctreeBag$pred: treeresponse(object, newdata = NULL, ...): compute statistics for the conditional distribution
#of the response as modelled by the tree. For regression problems, this is just the mean. For
#nominal or ordered responses, estimated conditional class probabilities are returned. KaplanMeier
#curves are computed for censored responses. Note that a list with one element for each
#observation is returned.

#predict(treebag,predictors)   
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag$fits[[2]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

#another example
data(mdrr)
## remove some zero variance predictors and linear dependencies
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .95)]
basicLDA <- train(mdrrDescr, mdrrClass, "lda")
bagLDA2 <- train(mdrrDescr, mdrrClass, method="bag", B = 10, 
                 bagControl = bagControl(fit = ldaBag$fit,
                                         predict = ldaBag$pred,
                                         aggregate = ldaBag$aggregate),
                 tuneGrid = data.frame(vars = c((1:10)*10 , ncol(mdrrDescr))))
#The final value used for the model was vars = 20. 

#Conditional inference trees vs traditional decision trees
#the main difference seems to be that ctree uses a covariate selection scheme that is 
#based on statistical theory (i.e. selection by permutation-based significance tests) 
#and thereby avoids a potential bias in rpart, otherwise they seem similar; e.g. 
#conditional inference trees can be used as base learners for Random Forests.


#3. Random Forest - an extension to bagging for classification and regression trees
#Process:
    #-Bootstrap samples.
    #-here is the difference: At each split, we also bootstrap variables. This makes for
           #a diverse set of potential trees that can be built.
    #-Grow a large number of trees and vote or average those trees
#Pros: Accuracy
#Cons: speed(slow); overfitting
    #interpretability(hard): because different trees represent bootstrap samples with bootstrap nodes and take aggregation
data(iris)
library(ggplot2)
inTrain<-createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
library(caret)
library(randomForest)
modFit<-train(Species~., data=training, method="rf",prox=TRUE)  #rf is random forest
#Proximities: The proximities originally formed a NxN matrix. After a tree is grown, put 
    #all of the data, both training and oob, down the tree. If cases/records k and n are in the 
    #same terminal node increase their proximity by one. At the end, normalize the 
    #proximities by dividing by the number of trees.
modFit
#getting a single tree from a forest -- getTree
getTree(modFit$finalModel,k=2)
#result: (terminal means leaf)
  #each column corresponds to particular split.
  #split var: which variable we're splitting on. Split point:what's the value where that variable is split.
  #status: is the node terminal (-1) or not (1)
  #prediction: what the perdiction would be out of that particular split. 0 if the node is not terminal
modFit$finalModel
names(modFit$finalModel)
#class "centers"
irisP<-classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
#classCenter(x, label, prox, nNbr = min(table(label))-1) 
#prox: the proximity (or similarity) matrix, assumed to be symmetric with 1 on the 
#diagonal and in [0, 1] off the diagonal (the order of row/column must match that of x)
class(irisP)  #matrix
irisP
irisP<-as.data.frame(irisP)
irisP$Species<-rownames(irisP)
p<-qplot(Petal.Width,Petal.Length,col=Species,data=training)
p+geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)
#predicting new values
pred<-predict(modFit,testing)
testing$predRight<-pred==testing$Species
sum(testing$predRight)/length(testing$predRight)
table(pred,testing$Species)

#Random Forest are usually one of the two top performing algorithms along with boosting.
#Care should be taken to avoid overfitting. (rfcv function or there is an option in train function)
?rfcv  #random forest cross-validation
#rfcv(trainx, trainy, cv.fold=5, scale="log", step=0.5,
#     mtry=function(p) max(1, floor(sqrt(p))), recursive=FALSE, ...)
set.seed(647)
myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96))
result <- rfcv(myiris, iris$Species, cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

result <- replicate(5, rfcv(myiris, iris$Species), simplify=FALSE)
#replicate(n, expr, simplify = "array")
error.cv <- sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")


#4. boosting - weightedly add up weak predictors
#iterative, select one classifier at each step, calculate weights based on errors
#upweight missed classifications and select next h
#weight alpha=0.5*ln((1-error)/error)
#Boosting can be used with any subset of classifiers
#one large subclass is gradient boosting
#R libraries: most are available in caret package
             #- gbm: boosting with trees
             #- mboost: model based boosting
             #- ada: statistical boosting based on additive logistic regression
             #- gamBoost: boosting generalized additive models
library(ISLR);data(Wage);library(ggplot2);library(caret)
Wage<-subset(Wage,select=-c(logwage))
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
library(gbm)
modFit<-train(wage~.,method="gbm",data=training,verbose=FALSE)
modFit
qplot(predict(modFit,testing),wage,data=testing,xlim=c(50,300),ylim=c(50,300))


#5. Model-based Prediction
#Basic idea:
   #(1) Assume the data follow a probabilistic model
   #(2) Use Bayes' theorem to identify optimal claasifiers
#Goal is to build a parametric model for conditional distribution P(Y=k|X=X)
#A typical approach is to apply Bayes Theorem: P(Y=k|X=x)=(f(x)_k * pi_k)/(sum{l=1~k}f(x)_l * pi_l) 
   #pi_k is P(Y=k) ; f(x)_k is P(X=x|Y=k)
   #typical prior probabilities pi_k are set in advance
   #f(x)_k often uses normal distribution (estimate mean and variance from data)
   #classify to the class with the highest value of P(Y=k|X=x) #same logic with MLE
#A range of models use this approach
   #Linear Discriminant Analysis: assumes f(x)_k is multivariate Gaussian with same covariances (covariances across classes are identical)
   #Quadratic Discriminant Analysis: assumes f(x)_k is multivariate Gaussian with different covariances (covariances across classes are different)
   #Naive Bayes assumes independence between features for model building (the covariance matrix is a diagonal matrix)
#Model based prediction assumes more complicated versions for the covariance matrix

#Lnear discriminant anlaysis?
#log(P(Y=k|X=x)/P(Y=j|X=x)) = log(f(x)_k/f(x)_j)+log(pi_k/pi_j)
                           #= log(pi_k/pi_j) - 0.5*(miu_k+miu_j)' * sigma^(-1) * (miu_k+miu_j) + x' * sigma^(-1) * (miu_k-miu_j)
#discriminant function: d(x)_k = - 0.5*miu_k * sigma^(-1) * miu_k + x' * sigma^(-1) * miu_k
   #decide on class based on Y(x)_hat = argmax_k(d(x)_k)
   #we usually estimate parameters with MLE

#Naive Bayes - does something more to simplify the problem
#logic: suppose we have many predictors: we would want to model: P(Y=k|X1,...,Xm)
#By Bayes Theorem: P(Y=k|X1,...,Xm) = [pi_k * P(X1,...,Xm|Y=k)]/sum{l=1~k}(P(X1,...,Xm|Y=k)*pi_l) 
                                   # ~ pi_k * P(X1,...,Xm|Y=k)
                                   # = pi_k * P(X1|Y=k)* P(X3,...,Xm|X1,Y=k)
                                   # = pi_k * P(X1|Y=k)* P(X2|X1,Y=k)* P(X3,...,Xm|X1, X2,Y=k)
                                   # = pi_k * P(X1|Y=k)* P(X2|X1,Y=k)* ... * P(Xm|X1, X2, ..., Xm-1,Y=k)
                                   # = pi_k * P(X1|Y=k)* P(X2|Y=k)*... * P(Xm|Y=k)  because all of predictors are independent
data(iris);library((ggplot2))
names(iris)
table(iris$Species)
inTrain<-createDataPartition(iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training);dim(testing)
library(klaR)
modlda<-train(Species~.,data=training,method="lda")
modnb<-train(Species~.,data=training,method="nb")
plda<-predict(modlda,testing);
pnb<-predict(modnb,testing);
table(plda,pnb)
table(plda,testing$Species)
table(pnb,testing$Species)
#lda is a little bit better than pnb

#comparison of results
equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,color=equalPredictions,data=testing)


#5. Regularized Regression
#Basic idea: fit a regression model + penalize (or shrink) large coefficients
#Pros: can help with the bias/variance tradeoff - if certain variables are highly correlated
          #with each other, you might not want to include them both in linear regression 
          #model as they will have a very high variance; but leaving one of them out might
          #slightly bias your model(lose a little bit of prediction capability); 
      #Can help with model selection in certain cases for regular organization techniques like the lasso.

# regression subset selection in the prostate dataset
library(ElemStatLearn);data(prostate)
str(prostate)
head(prostate)

covnames <- names(prostate[-(9:10)])
y <- prostate$lpsa
x <- prostate[,covnames]

form <- as.formula(paste("lpsa~", paste(covnames, collapse="+"), sep=""))
class(form)  #"formula"
summary(lm(form, data=prostate[prostate$train,]))

set.seed(1)
train.ind <- sample(nrow(prostate), ceiling(nrow(prostate))/2)
y.test <- prostate$lpsa[-train.ind]
x.test <- x[-train.ind,]
y <- prostate$lpsa[train.ind]
x <- x[train.ind,]

p <- length(covnames)
rss <- list()   #create an empty list
for (i in 1:p) {
        #cat(i)   #good idea!
        Index <- combn(p,i)
        #combn(x, m, FUN = NULL, simplify = TRUE, ...)
           #x: vector source for combinations, or integer n for x <- seq_len(n).
           #m: number of elements to choose
        #cat(paste("index",Index))
        rss[[i]] <- apply(Index, 2, function(is){  #apply by columns
                form <- as.formula(paste("y~", paste(covnames[is], collapse="+"), sep=""))
          #"lcavol"  "lweight" "age"     "lbph"    "svi"     "lcp"     "gleason" "pgg45"  
                cat(paste(form),"\n")
                isfit <- lm(form, data=x)
                yhat <- predict(isfit)  #if not newdata, don't have to specify data
                train.rss <- sum((y - yhat)^2)                
                yhat <- predict(isfit, newdata=x.test)
                test.rss <- sum((y.test - yhat)^2)
                c(train.rss, test.rss)
        })
}

#png("./PracticalMachLearning/selection-plots-01.png", height=432, width=432, pointsize=12)
plot(1:p, 1:p, type="n", ylim=range(unlist(rss)), xlim=c(0,p), xlab="number of predictors", ylab="residual sum of squares", main="Prostate cancer data")
for (i in 1:p) {
        points(rep(i-0.15, ncol(rss[[i]])), rss[[i]][1, ], col="blue")
        points(rep(i+0.15, ncol(rss[[i]])), rss[[i]][2, ], col="red")
}
minrss <- sapply(rss, function(x) min(x[1,]))
lines((1:p)-0.15, minrss, col="blue", lwd=1.7)
minrss <- sapply(rss, function(x) min(x[2,]))
lines((1:p)+0.15, minrss, col="red", lwd=1.7)
legend("topright", c("Train", "Test"), col=c("blue", "red"), pch=1)
#dev.off()
#we could see apparent overfitting in the above plot.

#Model selection Approach - Split Samples
   #(1)divide data into training/test/validation
   #(2)treat validation as test data, train all competing models on the training data
      #and pick the best one on validation.
   #(3)to appropriate assess performance on new data, apply to test set
   #(4)You may re-split and reperform steps 1-3

#Model selection Approach - Decomposing Expected Prediction Error
#Assume Yi= f(Xi)+ei. EPE(lambda) = E[{Y-f(X)_lambda}^2], where f(X)_lambda is the estimate from the 
        #training data (predicted outcome).
#look at a new data point X=x*:
#E[{Y-f(x*)_lambda}^2] = sigma^2 + {E[{f(x*)_lambda}]-f(x*)}^2 + var[f(x0)_lambda]
                      #= irreducible error + Bias^2 + Variance


##
# ridge regression on prostate dataset
# ridge regression penalizes the size of the regression coefficients
#http://web.as.uky.edu/statistics/users/pbreheny/764-F11/notes/9-1.pdf
library(MASS)
#lambdas <- seq(0,50,len=10)
lambdas <- seq(0,50,by=0.1)
M <- length(lambdas)
train.rss <- rep(0,M)
test.rss <- rep(0,M)
betas <- matrix(0,ncol(x),M)
for(i in 1:M){
        Formula <-as.formula(paste("y~",paste(covnames,collapse="+"),sep=""))
        fit1 <- lm.ridge(Formula,data=x,lambda=lambdas[i])  #lambdas is the penalty factor
        betas[,i] <- fit1$coef
        
        scaledX <- sweep(as.matrix(x),2,fit1$xm) #xm: column means of x matrix
         #sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...) #default is substract
        scaledX <- sweep(scaledX,2,fit1$scale,"/")
         #devide by fit1$scale
        #xm<-apply(x,2,mean)  # same with fit1$xm
        #could just use: scaledX<-apply(x,2,scale)  #slightly different from sweep due to precision.
        
        yhat <- scaledX%*%fit1$coef+fit1$ym
        train.rss[i] <- sum((y - yhat)^2)
        
        scaledX <- sweep(as.matrix(x.test),2,fit1$xm)
        scaledX <- sweep(scaledX,2,fit1$scale,"/")
        yhat <- scaledX%*%fit1$coef+fit1$ym
        test.rss[i] <- sum((y.test - yhat)^2)
}

#png(file="./PracticalMachLearning/selection-plots-02.png", width=432, height=432, pointsize=12) 
plot(lambdas,test.rss,type="l",col="red",lwd=2,ylab="RSS",ylim=range(train.rss,test.rss))
lines(lambdas,train.rss,col="blue",lwd=2,lty=2)
best.lambda <- lambdas[which.min(test.rss)]
abline(v=best.lambda+1/9)
legend(30,30,c("Train","Test"),col=c("blue","red"),lty=c(2,1))
#dev.off()

png(file="./PracticalMachLearning/selection-plots-03.png", width=432, height=432, pointsize=8) 
plot(lambdas,betas[1,],ylim=range(betas),type="n",ylab="Coefficients")
for(i in 1:ncol(x))
        lines(lambdas,betas[i,],type="b",lty=i,pch=as.character(i))
abline(h=0)
legend("topright",covnames,pch=as.character(1:8),cex = 0.6)
dev.off()


#######
# lasso 
#http://statweb.stanford.edu/~tibs/lasso/simple.html
#Give a set of input measurements x1, x2 ...xp and an outcome measurement y, the lasso fits a linear model 
#yhat=b0 + b1*x1+ b2*x2 + ... bp*xp 
#The criterion it uses is: Minimize sum( (y-yhat)^2 ) subject to sum[absolute value(bj)] <= s 
#The first sum is taken over observations (cases) in the dataset. The bound "s" is a tuning parameter. 
#When "s" is large enough, the constraint has no effect and the solution is just the usual multiple linear least squares regression of y on x1, x2, ...xp. 
#However when for smaller values of s (s>=0) the solutions are shrunken versions 
#of the least squares estimates. Often, some of the coefficients bj are zero. 

#Choosing "s" is like choosing the number of predictors to use in a regression model, and cross-validation is a good tool for estimating the best value for "s". 

#The computation of the lasso solutions is a quadratic programming problem , and 
#can be tackled by standard numerical analysis algorithms. But the least angle 
#regression procedure is a better approach.

#The least angle regression procedure follows the same general scheme with 
#forward stepwise regression, but doesn't add a predictor fully into the model. 
#The coefficient of that predictor is increased only until that predictor is no 
#longer the one most correlated with the residual r. Then some other competing 
#predictor is invited to "join the club". 
  #Start with all coefficients bj equal to zero.
  #Find the predictor xj most correlated with y
  #Increase the coefficient bj in the direction of the sign of its correlation with y. Take residuals r=y-yhat along the way. Stop when some other predictor xk has as much correlation with r as xj has.
  #Increase (bj, bk) in their joint least squares direction, until some other predictor xm has as much correlation with the residual r.
  #Continue until: all predictors are in the model

if(!require("lars")){install.packages("lars")}
library(lars)  #(LARS: Least angle regression)
lasso.fit <- lars(as.matrix(x), y, type="lasso", trace=TRUE)

#png(file="./PracticalMachLearning/selection-plots-04.png", width=432, height=432, pointsize=8) 
plot(lasso.fit, breaks=FALSE)
legend("topleft", covnames, pch=8, lty=1:length(covnames), col=1:length(covnames))
#dev.off()

# this plots the cross validation curve
png(file="./PracticalMachLearning/selection-plots-05.png", width=432, height=432, pointsize=12) 
lasso.cv <- cv.lars(as.matrix(x), y, K=10, type="lasso", trace=TRUE)
dev.off()
















