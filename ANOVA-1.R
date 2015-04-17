setwd("D:\\study\\DataScience")

#Before start
#1. EDA
#Numerical summaries - means, sds, five-number summaries, correlations.
#Graphical summaries
  #- One variable - Boxplots, histograms etc.
  #- T wo variables - scatterplots.
  #- Many variables - interactive graphics.

library(faraway)
data(pima)
pima
summary(pima)
sort(pima$diastolic)
#set all zero values of the five variables to NA which is the missing value
pima$diastolic[pima$diastolic == 0] <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA
pima$test <- factor(pima$test)
summary(pima$test)
levels(pima$test) <- c("negative","positive")
summary(pima)
hist(pima$diastolic)
plot(density(pima$diastolic,na.rm=TRUE))
plot(sort(pima$diastolic),pch=".")
plot(diabetes ~ diastolic,pima)   #scatterplot
plot(diabetes ~ test,pima)   #boxplots as test is a factor
hist(pima$diastolic)
pairs(pima)

#2. Regression Analysis
#When p=1, it is called simple regression but when p=>1 it is called multiple regression 
#Objective:
   #- Prediction of future observations.
   #- Assessment of the effect of, or relationship between, explanatory variables on the response.
   #- A general description of data structure.

#e.g. Final and midterm scores in standard units
data(stat500)
stat500 <- data.frame(scale(stat500))
plot(final ~ midterm,stat500)
abline(0,1)
g <- lm(final ~ midterm,stat500)
abline(g$coef,lty=5)
cor(stat500)

#beta_hat = (sum[(xi-x_bar)y])/(sum(xi-x_bar)^2)
#beta_hat = (X'X)^(-1) * X'y; 
#E(beta_hat)=(X'X)^(-1) * X'X * beta = beta; 
#Var(beta_hat) = (X'X)^(-1) * X' * sigma^2 * I * X * (X'X)^(-1)  = (X'X)^(-1) * sigma^2
#e.g.
data(gala)
head(gala)
dim(gala)
gfit <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(gfit)

#(X'X)^(-1) is the unscaled covariance matrix
x <- cbind(1,gala[,-c(1,2)])  #the intercept should be included
y <- gala$Species
x <- as.matrix(x)   #Data frames are allowed to contain character variables, which would disallow 
                    #matrix arithmetic. We need to force x into the matrix form.
t(x) %*% x
xtxi <- solve(t(x) %*% x)
xtxi
#A somewhat more direct way to get (X'X)^(-1) is as follows:
gfit <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,data=gala)
gs <- summary(gfit)
gs$cov.unscaled   #same as xtxi
#scaled covariance:
summary(gfit)$cov.unscaled*summary(gfit)$sigma^2  #one way
vcov(gfit)  #another way. vcov returns the variance-covariance matrix of the main parameters of a fitted model object.

names(gs)
names(gfit)
gfit$fit
gfit$res
#we could get beta_hat directly
xtxi %*% t(x) %*% y
#or in a computationally efficient and stable manner
solve(t(x) %*% x, t(x) %*% y)

#We can estimate sigma using the estimator in the text
sqrt(sum(gfit$res^2)/(30-6))  #gfit$res is the residual for every observation

#We may also obtain the standard errors for the coefficients
sqrt(diag(xtxi))*60.975
#Finally we may compute R^2
1-sum(gfit$res^2)/sum((y-mean(y))^2)
summary(gfit)

#3. Inference
#F statistic = [(RSS_omega - RSS_OMEGA) / (df_omega - df_OMEGA)] / [RSS_OMEGA/df_OMEGA]
    #df_OMEGA = n - q; df_omega  = n - p
#Test of all predictors
#F_{p-1,n-p} = [(SYY-RSS)/(p-1)]/[RSS/(n-p)]
data(savings)
savings
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
summary(g)
sum((savings$sr-mean(savings$sr))^2)
sum(g$res^2)
((983.63-650.71)/4)/(650.706/45)
1-pf(5.7558,4,45)   # one-tail test  #p-value

#Testing just one predictor
#t statistic = beta_i_hat / se(beta_i_hat)
g2 <- lm(sr ~ pop75 + dpi + ddpi, data=savings)
sum(g2$res^2)
(797.72-650.71)/(650.71/45)
1-pf(10.167,1,45)
sqrt(10.167)   #t^2 will give you the F-statistic
2*(1-pt(3.1886,45))  #similar p-value
anova(g2,g)

#Testing a pair of predictors -  5 possible tests (omit both, omit one, test a subspace, test an offset)
#(1)Testing a subspace
#H0 : beta_j = beta_k
g <- lm(sr ~ .,savings)
gr <- lm(sr ~ I(pop15+pop75)+dpi+ddpi,savings) #The function I() ensures that the argument is evaluated 
                                               #rather than interpreted as part of the model formula.
anova(gr,g)
#(2)test whether one of the coefficients can be set to a particular value
##H0 : beta_ddpi = 2
#Such a fixed term in the regression equation is called an offset.
gr <- lm(sr ~ pop15+pop75+dpi+offset(2*ddpi),savings)
   #An offset is a term to be added to a linear predictor, such as in a generalised linear model, with 
   #known coefficient 1 rather than an estimated coefficient.
anova(gr,g)
#t statistic = (beta_hat - c)/se(beta_hat)
tstat <- (0.409695-2)/0.196197
tstat
2*pt(tstat,45)   #similar to the pvalue for anova's F statistic
tstat^2  #similar to F statistic in ANOVA

#notice that we couldn't test H0: beta_j * beta_k = 1
#This hypothesis is not linear in the parameters so we can't use our general method

#Concerns about Hypothesis T esting - permutation test
#Since the permutation test does not depend on the assumption of normality , we might regard it as 
    #superior to the normal theory based value.
g <- lm(sr ~ pop75+dpi,data=savings)
summary(g)
gs <- summary(g)
gs$fstat
#one way to simulate and calculate
permutations <- sapply(1 : 10000, function(i) summary(lm(sample(sr) ~ pop75+dpi,data=savings))$fstat[1])
mean(permutations>gs$fstat[1])
#another way
fstats <- numeric(10000)
for(i in 1:10000){
        ge <- lm(sample(sr) ~ pop75+dpi,data=savings)
        fstats[i] <- summary(ge)$fstat[1]
        }
length(fstats[fstats > gs$fstat[1]])/10000  #null isn't included in

#(3) Confidence Intervals for beta
#As with testing, we must decide whether to form confidence regions for parameters individually or
#simultaneously. Simultaneous regions are preferable but for more than two dimensions they are difficult to
#display and so there is still some value in computing the one-dimensional confidence intervals.

#simultaneous regions:
   #(beta_hat - beta)' X' X (beta_hat - beta) / sigma^2 ~ chisq_p and (n-p)*sigma_hat ^ 2 / sigma^2 ~ chisq_(n-p)
   #--> (beta_hat - beta)' X' X (beta_hat - beta)/(p*sigma_hat ^ 2) ~ (chisq_p/p) / (chisq_(n-p)/(n-P)) = F_{p,n-p}
   #--> (beta_hat - beta)' X' X (beta_hat - beta) <= (p*sigma_hat ^ 2) * F_{p,n-p}
#one-dimensional confidence intervals:
   #(beta_hat_i + c(-1,1) * qt_{alpha/2, n-p} * sigma_hat * sqrt((X'X_ii)^(-1))
g <- lm(sr ~ ., savings)
summary(g)
dim(savings)  #50 * 5
#individual 95% confidence intervals for the regression parameters of pop75:
qt(0.975,45)    #df = n-p = 45  #result: 2.014103, because symmetric, so directly use 0.975
qt(0.025,45)    #result: -2.014103  #previously we calcualte pt, now we calculate qt.
c(-1.69-2.01*1.08,-1.69+2.01*1.08)
#similarly for growth(ddpi):
c(0.41-2.01*0.196,0.41+2.01*0.196)   #the qt_{alpha/2, n-p} * sigma_hat * sqrt((X'X_ii)^(-1)) part is the same
#Notice that this confidence interval is pretty wide in the sense that the upper limit is about 50 times larger
#than the lower limit. This means that we are not really that confident about what the exact effect of growth
#on savings really is.

#drawing confidence ellipses
library(ellipse)
plot(ellipse(g,c(2,3)),type="l",xlim=c(-1,0))
#add the origin and the point of the estimates:
points(0,0)
points(g$coef[2],g$coef[3],pch=18)
abline(v=c(-0.461-2.01*0.145,-0.461+2.01*0.145),lty=2)
abline(h=c(-1.69-2.01*1.08,-1.69+2.01*1.08),lty=2)
#Why are these lines not tangential to the ellipse? The reason for this is that the confidence intervals are
#calculated individually. If we wanted a 95% chance that both intervals contain their true values, then the
#lines would be tangential. (but I don't know how to draw it). we prefer the joint test result.

#Examine the correlation of the two predictors:
cor(savings$pop15,savings$pop75)    #-0.9084787
#But from the plot, we see that coefficients have a positive correlation. 
#The correlation between predictors and the correlation between the coefficients of those predictors are 
   #often different in sign.
#Intuitively, this can be explained by realizing that two negatively correlated predictors are attempting 
#to the perform the same job. The more work one does, the less the other can do and hence the positive 
#correlation in the coefficients. 

#(4)Confidence intervals for predictions
#y_hat_0 = x_0' * beta_hat = x_0' * (X'X)^(-1) * x_0 * sigma^2
#prediction of a future value --> x_0' * beta_hat + epsilon
  #--> y_hat_0 + c(-1,1) * t_{alpha/2, n-p} * sigma_hat * sqrt(1 + x_0' * (X'X)^(-1) * x_0)
#prediction of the mean response  --> x_0' * beta_hat 
  #-->y_hat_0 + c(-1,1) * t_{alpha/2, n-p} * sigma_hat * sqrt(x_0' * (X'X)^(-1) * x_0)
g <- lm(Species ~ Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
#Suppose we want to predict the number of species (of tortoise) on an island with predictors 0.08,93,6.0,12.0,0.34
x0 <- c(1,0.08,93,6.0,12.0,0.34)
y0 <- sum(x0*g$coef)        #33.91967
qt(0.975,24)                #2.0639
#(X'X)^(-1) matrix
x <- cbind(1,gala[,3:7])
x <- as.matrix(x)
xtxi <- solve(t(x) %*% x)  #same as summary(g)$cov.unscaled
#the prediction interval for the single future response
bm <- sqrt(1+x0 %*% xtxi %*% x0) *2.064 * 60.98     #130.0891
c(y0-bm,y0+bm)      #-96.16946 164.00879
#the bands for mean response CI:
bm <- sqrt(x0 %*% xtxi %*% x0) *2.064 * 60.98       #32.89005
c(y0-bm,y0+bm)      # 1.029614 66.809721
#In the single future response, negative values should be always possible. These impossibe values can be avoided 
#by transforming the response, say taking logs or by using a probability model more appropriate to the response

#There is a more direct method for computing the CI:
#the prediction interval for the single future response
predict(g,data.frame(Area=0.08,Elevation=93,Nearest=6.0,Scruz=12, Adjacent=0.34),se=T,interval="prediction")  #names are necessary
#the bands for mean response CI:
predict(g,data.frame(Area=0.08,Elevation=93,Nearest=6.0,Scruz=12, Adjacent=0.34),se=T,interval="confidence")
#The width of the mean response interval can then be calculated by multiplying the se for the fit by the
#appropriate t-critical value:
15.934*2.064   #32.888 --> bm

#(5)Orthogonality
#Suppose we can partition X in two, X = [X1|X2] such that X1'X2 = 0. So now
   #Y=X*beta + X1 * beta1 + X2 * beta2 + epsilon and X'X = [X1'X1, 0; 0, X2'X2]
   #then, beta1_hat = (X1'X1)^(-1) * (X1') * y; beta2_hat = (X2'X2)^(-1) * (X2') * y
#Now if we wish to test H0 : beta1 = 0, it should be noted that RSS_Omega/df = sigma_Omega^2 will 
   #be different depending on whether X2 is included in the model. #Orthogonality is a desirable property.

data(odor)
odor  # temp = (Fahrenheit-80)/40
x <- as.matrix(cbind(1,odor[,-1]))
xtx <- t(x) %*% x  #The matrix is diagonal.
#What would happen if temp was measured in the original Fahrenheit scale?
#The matrix would still be diagonal but the entry corresponding to temp would change.

g <- lm(odor ~ temp + gas + pack, data=odor)
summary(g,cor=T)
ga <- lm(odor ~ gas + pack, data=odor)
summary(ga)
###: The coefficients themselves do not change but the residual standard error does change slightly which 
   #causes small changes in the standard errors of the coefficients t-statistics and p-values, but 
   #nowhere near enough to change our qualitative conclusions.

g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g,cor=T)
#Drop pop15 from the model:
gu <- update(g, . ~ . - pop15)  #update will update and (by default) re-fit a model.
summary(gu)
#update(object, formula., ..., evaluate = TRUE)
#Pay particular attention to pop75. The effect has now become positive whereas it was negative.
   #Granted, in neither case is it significant, but it is not uncommon in other datasets for such sign 
   #changes to occur and for them to be significant.

#(6)Identifiability
#where X is an n  p matrix. If X TX is singular and cannot be inverted, then there will be infinitely many
   #solutions to the normal equations and beta_hat is at least partially unidentifiable.
#Unidentifiability will occur when X is not of full rank - when its columns are linearly dependent
   #multilinear, supersaturated.

#Suppose we create a new variable for the savings dataset - the percentage of people between 15 and 75:
pa <- 100-savings$pop15-savings$pop75
g <- lm(sr ~ pa + pop15 + pop75 + dpi + ddpi, data=savings)
summary(g)
#pop75 is not defined because of singularities
x <- as.matrix(cbind(1,pa,savings[,-1]))
dimnames(x) <- list(row.names(savings),c("int","pa","p15","p75", "dpi","ddpi")) #dimnames() is a list
#If we didn't know which linear combination was causing the trouble, how would we find out? An eigen
   #decomposition of X TX can help:
e <- eigen(t(x) %*% x)
signif(e$values,3)   #3 means only keep 3 digits in scientific expression
#Only the last eigenvalue is zero, indicating one linear combination is the problem. 
#So we look at the last column of the eigenvector
signif(e$vectors,3)
#From last column of the matrix, we see that 100-pa-p15-p75=0 is the offending combination.

#Lack of identifiability is obviously a problem but it is usually easy to identify and work around. More
   #problematic are cases where we are close to unidentifiability.
pae <- pa +0.001*(runif(50)-0.5)
ge <- lm(sr ~ pae+pop15+pop75+dpi+ddpi,savings)
summary(ge)
#Notice the now all parameters can be estimated but the standard errors are very large because we cannot
   #estimate them in a stable way.

#We hope that epsilon ~ N(0,sigma^2) but
  #1. Errors may be heterogeneous (unequal variance).
  #2. Errors may be correlated.
  #3. Errors may not be normally distributed.
#The last defect is less serious than the first two because even if the errors are not normal, the beta_hat 
  #will tend to normality due to the power of the central limit theorem. 


g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
g2 <- lm(sr ~ pop75 + dpi + ddpi, data=savings)
g3 <- lm(sr ~ pop75 + ddpi, data=savings)
g4 <- lm(sr ~ pop75, data=savings)
#Prediction is more stable than parameter estimation. 
x0 <- data.frame(pop15=32,pop75=3,dpi=700,ddpi=3)
predict(g,x0)
predict(g2,x0)
predict(g3,x0)
predict(g4,x0)
#Interpretation of beta: suppose we observe x1 + 1 and the same other given predictor values then the 
                       #predicted response is increased by beta.
#Notice that I have been careful to not to say that we have taken a specific individual and increased 
   #their x1 by 1, rather we have observed a new individual with predictor x1 + 1. To put it another way, 
   #people with yellow fingers tend to be smokers but making someone's fingers yellow won't make them more 
   #likely to smoke.

###Prediction is conceptually simpler since interpretation is not an issue but you do need to worry about extrapolation.
##Quantitative extrapolation: Is the new x0 within the range of validity of the model. Is it close to
                            #the range of the original data? If not, the prediction may be unrealistic. 
range(savings$pop75)  #0.56 4.70
grid <- seq(0,10,0.1)
p <- predict(g4,data.frame(pop75=grid),se=T)
cv <- qt(0.975,48)
matplot(grid,cbind(p$fit,p$fit-cv*p$se,p$fit+cv*p$se),lty=c(1,2,2), type="l",xlab="pop75",ylab="Saving")
rug(savings$pop75)      #A "rug" shows the location of the observed values of pop75
#The confidence bands in the figure become wider as we move away from the range of the data.
#A model may fit well in the range of the data, but outside of that range, the predictions may be very bad.

##Qualitative extrapolation: If the model was built in the past and is to be used for future predictions, 
  #we must make a difficult judgment as to whether conditions have remained constant enough for this to work.


#4.Errors in Predictors(x) (error in Y is allowed for with epsilon)
#suppose: y_i = eta_i + epsilon_i, x_i = zeta_i + delta_i, where epsilon and delta are independent.
#Suppose the true underlying relationship is: eta_i = beta_0 + beta_1 * zeta_i, but we only see (x_i,y_i).
#Putting it together, we get y_i = beta_0 + beta_1 * zeta_i + (epsilon_i - beta_1 * delta_i)
#let sigma_zeta^2 = [sum(zeta_i - zeta_bar)^2]/n,  sigma_{zeta,delta} = cov(zeta, delta)
   #where zeta are the true values of X

#Now beta1_hat = [sum(x_i-x_bar)*y]/[sum(x_i-x_bar)^2]
   #E[beta1_hat] = beta_1 * (sigma_zeta^2+sigma_{zeta,delta})/(sigma_zeta^2+sigma_{zeta,delta}+sigma_delta^2)
   #If there is no relation between zeta and delta, this simplifies to:
     #E[beta1_hat] = beta_1*sigma_zeta^2/(sigma_zeta^2+sigma_delta^2) = beta_1* 1/(1+sigma_delta^2/sigma_zeta^2)

#So in general beta_1 will be biased (regardless of the sample size and typically towards zero). 
#If sigma_delta^2 is small relative to sigma_zeta^2 then the problem can be ignored. 
#In other words, if the variability in the errors of observation of X are small relative to the range 
   #of X then we need not be concerned.
#If not, it's a serious problem and other methods such as fitting using orthogonal rather than vertical 
   #distance in the least squares fit should be considered.

#For prediction, measurement error in the x's is not such a problem since the same error will apply to the
   #new x0 and the model used will be the right one.
#True values of the regression coeffs are 0 and 1 respectively.
set.seed(1234)
x <- 10*runif(50)
y <- x+rnorm(50)
gx <- lm(y ~ x)
summary(gx)       #intercept is not significant
#Compare the results - notice how the slope has decreased. 
z <- x + rnorm(50)
gz <- lm(y ~ z)
summary(gz)
#add even more noise:
z2 <- x+5*rnorm(50)
gz2 <- lm(y ~ z2)
summary(gz2)
#compare:
matplot(cbind(x,z,z2),y,xlab="x",ylab="y")
abline(gx,lty=1)
abline(gz,lty=2)
abline(gz2,lty=5)
legend("bottomright",legend = c("gx","gz","gz2"),lty=c(1,2,5))
#Original x shown with "1", with small error as "2" and with large error as "3".

bc <- numeric(1000)
for(i in 1:1000){
        y <- x + rnorm(50)
        z <- x + 5*rnorm(50)
        g <- lm(y ~ z)
        bc[i] <- g$coef[2]
}
summary(bc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01285 0.18330 0.22110 0.22090 0.26100 0.38700 
#Given that the variance of a standard uniform random variable is 1/12, sigma_delta^2=25 
  #and sigma_zeta^2=100/12, we'd expect the mean of beta to be 0.25.


#5.Generalized Least Squares -- errors have non-constant variance or are correlated
#(1)THE GENERAL CASE -- assume errors are AR(1)
#previously, we assume that var(epsilon) = sigma^2 * I. 
#Instead, we assume var(epsilon) = simga^2 * SIGMA. --> SIGMA is the covariance matrix.
#simga^2 is unknown, SIGMA is known.

#then Generalized least squares minimizes (y-X*beta)'*SIGMA^(-1) * (y-X*beta)
#therefore, beta_hat = (X'SIGMA^(-1)X)^(-1) * X'*SIGMA^(-1)*y

#write SIGMA = S*S', where S is a triangular matrix using the Choleski Decomposition
#then minimize (y-X*beta)'* (S*S')^(-1) * (y-X*beta)=(S^(-1)*y-S^(-1)*X*beta)'*I*(S^(-1)*y-S^(-1)*X*beta)
#So GLS is like regressing S^(-1)*X on S^(-1)*y
   #--> where y = X*beta + epsilon, S^(-1)*y = S^(-1)*X*beta + S^(-1)*epsilon; 
                                   #--> y_new = X_new*beta + epsilon_new
   #--> var(epsilon') = var(S^(-1)*epsilon)=S^(-1)*var(epsilon)*S'^(-1) 
                     #= S^(-1)*sigma^2*S*S'*S'^(-1) = sigma^2 * I
   #--> var(beta_hat)=(X'*SIGMA^(-1)*X)^(-1) * sigma^2 
#the practical problem is that SIGMA may not be known.
data(longley)
head(longley)
str(longley)
g <- lm(Employed ~ GNP + Population, data=longley) #here the errors are assumed to be uncorrelated.
summary(g,cor=T)
#notice the correlation of Coefficients are all close to 1

#In data collected over time such as this, successive errors could be correlated. 
#assume that the errors take a simple autoregressive form:
   #epsilon_{i+1} = rho * epsilon_i + delta_i,  where delta_i ~ N(0,tau^2)
#under this assumption, SIGMA_ij = rho^|i-j|

#We can estimate this correlation 'rho' by:
cor(g$res[-1],g$res[-16])   #0.3104092
cor(residuals(g)[-1],residuals(g)[-16])
#We now construct the SIGMA matrix and compute the GLS estimate of beta along with its standard errors.
x <- model.matrix(g)
SIGMA <- diag(16)
SIGMA <- 0.31041^abs(row(SIGMA)-col(SIGMA))  #important!!!! How to create an AR matrix!
SigI <- solve(SIGMA)
xtxi <- solve(t(x) %*% SigI %*% x)
beta <- xtxi %*% t(x) %*% SigI %*% longley$Empl
beta
#Compare with the model output above where the errors are assumed to be uncorrelated.
#Another way to get the same result is to regress S^(-1)*y on S^(-1)*x as we demonstrate here:
sm <- chol(Sigma)
smi <- solve(t(sm))
sx <- smi %*% x                 #S^(-1)*X*beta
sy <- smi %*% longley$Empl      #S^(-1)*y
g_refit<-lm(sy ~ sx-1)  
g_refit$coef
g_refit$res
#Our initial estimate of rho is 0.31 but once we fit our GLS model we'd need to re-estimate it
#The process would be iterated until convergence. This is cumbersome. A more convenient approach
#may be found in the nlme package, which contains a GLS fitting function.
library(nlme)
g <- gls(Employed ~ GNP + Population, correlation=corAR1(form= ~Year), data=longley)
#corAR1(value, form, fixed)
   #value: the value of the lag 1 autocorrelation, which must be between -1 and 1. Defaults to 0 (no autocorrelation).
   #form: a one sided formula of the form ~ t, or ~ t | g, specifying a time covariate t and, optionally, a grouping factor g.
          # A covariate for this correlation structure must be integer valued.
          # observations with different grouping levels are assumed to be uncorrelated.
          #Defaults to ~ 1, which corresponds to using the order of the observations in the data as a covariate, and no groups.
                  #here, form=~Year is the same as form=~1
   #fixed: an optional logical value indicating whether the coefficients should be allowed to 
                  #vary in the optimization, or kept fixed at their initial value. 
summary(g)  #rho = 0.6441692
intervals(g)  #no applicable method for 'intervals' applied to an object of class "lm"


#(2)Weighted Least Squares
#Sometimes the errors are uncorrelated, but have unequal variance where the form of the 
   #inequality is known. --> SIGMA is diagonal.
#we can write SIGMA = diag(1/w_1, 1/w_2, ..., 1/w_n)
#so S = diag(sqrt(1/w_1),sqrt(1/w_2),...,sqrt(1/w_n)), so where the w_i are called the weights 
#and so we can regress sqrt(w_i*x_i) on sqrt(w_i*y_i)
#therefore, Cases with low variability should get a high weight, high variability a low weight. 

#examples of such situation:
#Errors proportional to a predictor: var(epsilon_i) ~ x_i suggests w_i = x_i^(-1) 
#Yi are the averages of n_i observations then var(y_i)=var(epsilon_i) = sigma^2/n_i suggests w_i=n_i.

#fitting the regression with weights
data(strongx)
strongx
#The cross-section(crossx) variable is believed to be linearly related to the inverse of the energy (energy - has already been inverted)
#sd is the standard deviation of the response(y_i, that is, crossx).
g <- lm(crossx ~ energy, strongx, weights=sd^-2)   #directly add weights option in lm()
summary(g)
#fitting the regression without weights
gu <- lm(crossx ~ energy, strongx)
summary(gu)
#The two fits can be compared
plot(crossx ~ energy, data=strongx)
abline(g)
abline(gu,lty=2)



#(3)Iteratively Reweighted Least Squares
#we may model SIGMA using a small number of parameters(predictors):
   # var(epsilon_i) = sigma^2 * SIGMA = gamma_0 + gamma_1 * x1

#The IRWLS fitting Algorithm is (don't have to remember, we have rlm function):
   # Start with w_i = 1
   # Use least squares to estimate beta.
   # Use the residuals to estimate gamma, perhaps by regressing epsilon^2 on x.
   # Recompute the weights and goto 2.  (Continue until convergence.)

library(foreign)
cdata <- read.dta("http://www.ats.ucla.edu/stat/data/crime.dta")
summary(cdata)
summary(ols <- lm(crime ~ poverty + single, data = cdata))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)  #las: numeric in {0,1,2,3}; the style of axis labels.
                    #0: always parallel to the axis [default], 
                    #1: always horizontal    #2: always perpendicular to the axis,
                    #3: always vertical.
par(opar)  #will return to default par setting (1*1 mfrow)
cdata[c(9, 25, 51), 1:2]
d1 <- cooks.distance(ols)
library(MASS)
r <- stdres(ols)  #The standardized residuals. !!important!!
a <- cbind(cdata, d1, r)
a[d1 > 4/51, ]

#Now we will look at the residuals. We will generate a new variable called absr1, which is the
   #absolute value of the residuals
rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]
#Now let's run our first robust regression. Robust regression is done by iterated re-weighted least squares (IRLS). 
#There are several weighting functions that can be used for IRLS. 
#We are going to first use the Huber weights in this example
#         rlm(x, y, weights, ..., w = rep(1, nrow(x)),
#             init = "ls", psi = psi.huber,
#             scale.est = c("MAD", "Huber", "proposal 2"), k2 = 1.345,
#             method = c("M", "MM"), wt.method = c("inv.var", "case"),
#             maxit = 20, acc = 1e-4, test.vec = "resid", lqs.control = NULL)
summary(rr.huber <- rlm(crime ~ poverty + single, data = cdata))
#In Huber weighting, observations with small residuals get a weight of 1 and the larger the 
   #residual, the smaller the weight. 
#With bisquare weighting, all cases with a non-zero residual get down-weighted at least 
   #a little.
hweights <- data.frame(state = cdata$state, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]
#We can see that roughly, as the absolute residual goes down, the weight goes up. 
        #In other words, cases with a large residuals tend to be down-weighted. This output shows 
        #us that the observation for Mississippi will be down-weighted the most. Florida will also 
        #be substantially down-weighted. All observations not shown above have a weight of 1. 
        #In OLS regression, all cases have a weight of 1.

rr.bisquare <- rlm(crime ~ poverty + single, data=cdata, psi = psi.bisquare)
    #Psi functions are supplied for the Huber, Hampel and Tukey bisquare proposals as 
        #psi.huber, psi.hampel and psi.bisquare
summary(rr.bisquare)
biweights <- data.frame(state = cdata$state, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]
#We can see that the weight given to Mississippi is dramatically lower using the bisquare 
#weighting function than the Huber weighting function and the parameter estimates from 
#these two different weighting methods differ. 

# When comparing the results of a regular OLS regression and a robust regression, if the 
# results are very different, you will most likely want to use the results from the robust 
# regression. Large differences suggest that the model parameters are being highly influenced 
# by outliers.

#Huber weights can have difficulties with severe outliers, 
    #and bisquare weights can have difficulties converging or may yield multiple solutions.

#An alternative approach is to model the variance and jointly estimate the regression and 
#weighting parameters using likelihood based method. This can be implemented in R using the 
#gls() function in the nlme library.
#last time we use gls() is in 5(1) when we use correlation=corAR1(form= ~Year)

# AR(1) errors within each Mare
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
# variance increases as a power of the absolute fitted values
fm2 <- update(fm1, weights = varPower())


#6. Testing for Lack of Fit
#How can we tell if a model fits the data? 
    #If the model is correct then sigma_hat^2 should be an unbiased estimate of sigma^2.
#If a lack of fit is found, then a new model is needed.

#Criteria: If we have a model which is not complex enough to fit the data or simply takes 
      #the wrong form, then sigma_hat^2 will overestimate sigma^2
      #Alternatively, if our model is too complex and overfits the data, 
      #then sigma_hat^2 will be an underestimate.
#This suggests a possible testing procedure - we should compare sigma_hat^2 to sigma^2.

#(1) sigma^2 is known -- chisq test
   #because sigma_hat^2/sigma^2 ~ Chisq_{n-p}/(n-p)
   #so there is a lack of fit if (n-p)*sigma_hat^2/sigma^2 > chisq_{n-p,1-alpha}
#Still use strongx data as we we know the variance almost exactly because each response value 
   #is the average of a large number of observations
#Because of the way the weights are defined, w_i=1/var(y_i), the known variance is implicitly 
   #equal to one. There is nothing special about one - we could define w_i=99/var(y_i) and
   #the variance would be implicitly 99. 
data(strongx)
g <- lm(crossx ~ energy, weights=sd^-2, strongx)
summary(g)
plot(strongx$energy,strongx$crossx,xlab="Energy",ylab="Crossection")
abline(g$coef)  #abline(g) also works
chistat<-1.66^2*8   #test stat: Residual standard error: 1.66 on 8 degrees of freedom  
                 #because setting known sigma^2 as 1, so chisqtest = (n-p)*sigma_hat^2
  #if the known sigma^2 is a different number, then chisqtest = (n-p)*sigma_hat^2/(n-p)*sigma^2
1-pchisq(chistat,8)  #0.0048332
#We conclude that there is a lack of fit. Just because R2 is large does not mean that 
#you can not do better. Add a quadratic term to the model and test again:
g2 <- lm(crossx ~ energy + I(energy^2), weights=sd^-2, strongx)
#g2 <- lm(crossx ~ energy + energy^2, weights=sd^-2, strongx)   #if no I(), will ignore energy^2
summary(g2)
tstat2<-0.679^2*7
1-pchisq(tstat2,7)   #This time we cannot detect a lack of fit. 
x <- seq(0.05,0.35,by=0.01)
lines(x,g2$coef[1]+g2$coef[2]*x+g2$coef[3]*x^2,lty=2)  #lty=2: a dotted line on the plot


#(2) sigma^2 is unknown -- estimate one   --  F-statistic: F_{n-p-df_pe,df_pe} 
#we can partition the RSS into that due to lack of fit(between subject variability) 
   #and that due to the pure error(within subject variability or the measurement error)

###ANOVA of LACK OF FIT TABLE
  #RSS: df = n-p; SS = RSS
  #lack of fit: df = n-p-df_pe; SS= RSS-SS_pe;  MS = (RSS-SS_pe)/(n-p-df_pe); F = Ratio of MS
  #pure error: df_pe; SS = SS_pe; MS=SS_pe/df_pe
#if there is no replicates(duplicate x values), then there is no pure error. 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!when there is no replicate!!!!!!!!!!!!!!!!!!!!!!!#
#When there are no replicates(then no with-in subject variance), it may be possible to group the 
#responses for similar x (use cut() function) but this is not straightforward. It is also possible 
#to detect lack of fit by less formal, graphical methods.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!if there is replicates!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#Compute the F-statistic = [(RSS-SS_pe)/(n-p-df_pe)] / (SS_pe/df_pe)
   #and compare to F_{n-p-df_pe,df_pe} and reject if the statistic is too large.

#The "pure error" estimate of sigma^2 is given by SS_pe/df_pe, that is, MS
          #where SS_pe = sum_distinctx (sum_givenx((y_i-y_bar)^2))  
              # df_pe = sum_distinctx(#replicates - 1)
#sum_givenx((y_i-y_bar)^2) means within-subject variance.

#If you fit a model that assigns one parameter to each group of observations with fixed x 
   #then the sigma^2 from this model will be the pure error sigma^2.  -- group x by factoring   #This model is just the one-way anova model

#Another way of looking at this is a comparison between the model of interest and a 
   #saturated model that assigns a parameter to each unique combination of the predictors.
data(corrosion)
corrosion
g <- lm(loss ~ Fe, data=corrosion)
summary(g)   #Residual standard error: 3.058
plot(corrosion$Fe,corrosion$loss,xlab="Iron content",ylab="Weight loss")
abline(g$coef)
#We have an R2 of 97% and an apparently good fit to the data. 
#We now fit a model that reserves a parameter for each group of data with the same value of x
#This is accomplished by declaring the predictor to be a factor. 
unique(corrosion$Fe)  #6 groups: 0.01 0.48 0.71 0.95 1.19 1.44 1.96
ga <- lm(loss ~ factor(Fe), data=corrosion)  # -- group x by factoring, so that get rid of the between-group variance
               #all errors would be the pure error!!!!!
#The fitted values are the means in each group - put these on the plot:
points(corrosion$Fe,ga$fit,pch=18)

#We can now compare the two models in the usual way
anova(g,ga)    #compare the pure error(SS_pe/df_pe) with RSS 3.058.
#notice the table is not the above ANOVA of LACK OF FIT TABLE, we only want to get the pure error data from the table.

#The low p-value indicates that we must conclude that there is a lack of fit. 
#The reason is that the "pure error" sqrt(11.782/6) = 1.4 is substantially less than the 
    #regression standard error of 3.058. We might investigate models other than a straight 
    #line although no obvious alternative is suggested by the plot.

#Before considering other models:
  #I would first find out whether the replicates are genuine - perhaps the low pure error SD 
     #can be explained by some correlation in the measurements. 
     #When there are replicates, it is impossible to get a perfect fit.In these cases, one 
     #should realize that the maximum R2 that may be attained might be substantially less than 
     #100% and so perceptions about what a good value for R2 should be downgraded appropriately.
  #Another possible explanation is unmeasured third variable is causing the lack of fit.

#These methods are good for detecting lack of fit, but if the null hypothesis is accepted, 
   #we cannot conclude that we have the true model. After all, it may be that we just did not 
   #have enough data to detect the inadequacies of the model. All we can say is that the model 
   #is not contradicted by the data.


#A more general question is how good a fit do you really want? By increasing the complexity 
#of the model, it is possible to fit the data more closely. By using as many parameters as data 
#points, we can fit the data exactly. Very little is achieved by doing this since we learn 
#nothing beyond the data itself and any predictions made using such a model will tend to have 
#very high variance. 

#For example, we can fit the mean responses for the example above exactly using a sixth order polynomial:
gp <- lm(loss ~ Fe+I(Fe^2)+I(Fe^3)+I(Fe^4)+I(Fe^5)+I(Fe^6),corrosion)
plot(loss ~ Fe, data=corrosion,ylim=c(60,130))
points(corrosion$Fe,ga$fit,pch=18)
grid <- seq(0,2,len=50)
lines(grid,predict(gp,data.frame(Fe=grid)))
summary(gp)$r.squared  #0.99653
#The fit of this model is excellent, but it is clearly riduculous. --> overfitting
#This illustrates the need not to become too focused on measures of fit like R^2.



#7. Diagnostics - influential points, assumption of residuals.

#(1) Residuals and Leverage
#y_hat = X*(X'X)^(-1)*X'*y = H*y, where H is the hat-matrix, which is only depends on X

#Residual(epsilon_hat) is different from error:
   #epsilon_hat = y-y_hat = (I-H)*y = (I-H)*X*beta + (I-H)*epsilon = (I-H)*epsilon
   #var(epsilon_hat) = var(I-H) * epsilon = (I-H)*sigma^2 (assuming var(epsilon) = sigma^2*I)
#h_i = H_ii are called leverages. The h_i depends only on X.
#var(epsilon_hat_i) = sigma^2*(1-h_i), so that a large leverage for h_i will make 
   #var(epsilon_hat_i) small, in other words the fit will be "forced" to be close to y_i. 
# leverage points have residuals with less variability than residuals from non-leverage points
# a leverage point is characterized by fitted value close to the observed target value, its 
   #residual is likely to be closer to zero.(but leverage point could be considered x-axis outlier as 
        #normal outlier is considered y-axis outlier) (a large leverage point is considered good 
        #leverage point but c is considered bad leverage point.)

#Facts: sum_{i}(h_i) = p; h_i >= 1/n for any i because there must be more than 1 predictors
#An average value for h_i is p/n and a "rule of thumb" is that leverages of more than 2p/n 
   #should be looked at more closely.
#Large values of h_i are due to extreme values in X.
#var(y_hat) = var(H*y) = H*sigma^2 so var(y_hat_i) = h_i * sigma_hat^2  (sigma_hat is the RSS)

data(savings)
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
plot(g$res,ylab="Residuals",main="Index plot of residuals")
sort(g$res)[c(1,50)]
countries <- row.names(savings)
identify(1:50,g$res,countries)  #Double click on the left mouse button next to the points you 
                                #are interested in to identify them, and press Esc to show them.
#identify(x, y = NULL, labels = seq_along(x), pos = FALSE, n = length(x), plot = TRUE, atpen = FALSE, offset = 0.5, tolerance = 0.25, ...)

#Now look at the leverage: We first extract the X-matrix here using model.matrix() and then 
#compute and plot the leverages (also called "hat" values)
x <- model.matrix(g)
lev <- hat(x)       #get hat values H[i,i] (leverages)
plot(lev,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*5/50)   #rule of thumb
sum(lev)           #sum_{i}(h_i) = p

names(lev) <- countries
lev[lev > 0.2]     ##rule of thumb cutoff
#Alternatively, we can do it interactively like this
identify(1:50,lev,countries)


#(2)Studentized Residuals - a studentized residual is dividing residual by an estimate of its standard deviation. 
       #This is an important technique in the detection of outliers.

#(internally) studentized residuals: the ith case is included.
    #var(epsilon_hat_i) = sigma^2*(1-h_i) --> r_i = epsilon_hat_i/(sigma_hat*sqrt(1-h_i))

#If the model assumptions are correct var(r_i)=1 and corr(r_i,r_j) tends to be small.
#(see 7.8)Studentized residuals are sometimes preferred in residual plots as they have been standardized to have equal variance. 

#Note that studentization can only correct for the natural non-constant variance in residuals when the
#errors have constant variance. If there is some underlying heteroscedascity in the errors(epsilon), 
#studentization cannot correct for it.
#studentized residuals for the savings data:
gs <- summary(g)
gs$sigma
stud <- g$res/(gs$sigma * sqrt(1-lev))
plot(stud,ylab="Studentized Residuals",main="Studentized Residuals")

#(3) An outlier Test -- jackknife residuals -- t dist.
#jackknife (or externally studentized or crossvalidated) residuals -- the ith case is exclused.
   #t_i = epsilon_i_hat/[sigma_(i)_hat * sqrt(1-h_i)] = r_i * sqrt[(n-p-1)/(n-p-r_i^2)]  #(i) means leaving out the ith case
   #t_i ~ t_(n-p-1);  if p value too small, then case i is an outlier
# Bonferroni correction: 
   #P(all tests accept) = 1 - P(At least one rejects) >= 1 - sum_{i}P(Test i rejects) = 1 - n*alpha
jack <- rstudent(g)
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
jack[abs(jack)==max(abs(jack))]    #pick up the largest jackknife residual.
#The largest residual of 2.85 is pretty big for a standard normal scale but is it an outlier? 
#Compute the Bonferroni critical value:
abs(qt(.05/(50*2),44))  #3.525801 > 2.85. So it's not significant.

#notes:
  # Two or more outliers next to each other can hide each other.
  # An outlier in one model may not be an outlier in another when the variables have been changed or
                # transformed. You will usually need to reinvestigate the question of outliers 
                # when you change the model.
  # for large datasets, we need only worry about clusters of outliers. 

#Here is an example of a dataset with multiple outliers.
#Data are available on the log of the surface temperature and the log of the light intensity 
    #of 47 stars in the star cluster CYG OB1, which is in the direction of Cygnus.
data(star)
head(star)
plot(star$temp,star$light,xlab="log(Temperature)", ylab="log(Light Intensity)")
ga <- lm(light ~ temp, data=star)
gu <- lm(light ~ temp, data=star, subset=(temp>3.6))  #subset argument!!!!
abline(ga)  #apparently, the line is distorted by the outliers upperleft
abline(gu$coef,lty=2)

range(rstudent(ga))  #Are there any outliers in the data? The outlier test does not reveal any.
abs(qt(.05/(47*2),47-1-1))   #3.499247 is outside (-2.102756  2.780525)
#This illustrates the problem of multiple outliers. We can visualize the problems here, but for higher
#dimensional data this is much more difficult.

#(4)Influential Observations - at least either an outlier or have large leverage
#measures of influence: 
   #Change in the coefficients: beta_hat - beta_(i)_hat
   #Change in the fit: X'(beta_hat - beta_(i)_hat) = y_hat - y_(i)_hat
  ###Cook Statistics. D_i = [(beta_hat - beta_(i)_hat)'(X'X)(beta_hat - beta_(i)_hat)] / (p*sigma_hat^2)
                        #= [y_hat - y_(i)_hat]'[y_hat - y_(i)_hat] / (p*sigma_hat^2)
                        #= (1/p) * r_i^2 * h_i/(1-h_i)
                       #The first term, r_i^2, is the residual effect and the second is the leverage.
                            #The combination of the two leads to influence. 
cook <- cooks.distance(g)
plot(cook,ylab="Cooks distances")
identify(1:50,cook,countries)

gl <- lm(sr ~ pop15+pop75+dpi+ddpi,savings,subset=(cook < max(cook)))   #subset argument!!!
summary(gl)
summary(g)
#What changed? The coefficient for ddpi changed by about 50%. We don't like our estimates to be so
#sensitive to the presence of just one country. 

#It would be rather tedious to do this for each country but there's a quicker way:
ginf <- lm.influence(g)
  #results of lm.influence:
   #hat: a vector containing the diagonal of the 'hat' matrix.
   #coefficients: a matrix whose i-th row contains the change in the estimated coefficients which 
                 #results when the i-th case is dropped from the regression. 
   #sigma: a vector whose i-th element contains the estimate of the residual standard deviation 
          #obtained when the i-th case is dropped from the regression. 
   #wt.res: a vector of weighted (or for class glm rather deviance) residuals.
ginf$coef   #seems we need to plot it to see it clearly.
plot(ginf$coef[,2],ginf$coef[,3],xlab="Change in pop15 coef", ylab="Change in pop75 coef")
identify(ginf$coef[,2],ginf$coef[,3],countries)
gj <- lm(sr ~ pop15+pop75+dpi+ddpi,savings,subset=!(countries %in% c("Japan","Libya","Ireland")))
summary(gj)
summary(g)
#Notice that the ddpi term is no longer significant and that the the R2 value has decreased a lot.

#(5) Residual Plots --> check the assumptions of the model, 
#The most important diagnostic plot: Plot epsilon_hat(residual) against y_hat. 
#You should also plot epsilon_hat against x_i (for predictors that are both in and out of the model)
#Things to look for are heteroscedascity and nonlinearity

g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals") #check both non-constant variance and nonlinearity
abline(h=0)
plot(g$fit,abs(g$res),xlab="Fitted",ylab="|Residuals|")  #check for non-constant variance only
#A quick way to check non-constant variance is this regression:
summary(lm(abs(g$res) ~ g$fit))
#This test is not quite right as some weighting should be used and the degrees of freedom should be
#adjusted but there doesn't seem to be a clear problem with non-constant variance.
gr <- rlm(sr ~ pop15+pop75+dpi+ddpi,savings)
plot(gr$fit,gr$res,xlab="Fitted_psi.huber",ylab="Residuals")
gr_ha <- rlm(sr ~ pop15+pop75+dpi+ddpi,savings,psi=psi.hampel)
plot(gr_ha$fit,gr_ha$res,xlab="Fitted_psi.hampel",ylab="Residuals")
gr_bi <- rlm(sr ~ pop15+pop75+dpi+ddpi,savings,psi=psi.bisquare)
plot(gr_bi$fit,gr_bi$res,xlab="Fitted_psi.bisquare",ylab="Residuals")


#artificial example:
par(mfrow=c(3,3))
for(i in 1:9) plot(1:50,rnorm(50))   #Constant Variance
for(i in 1:9) plot(1:50,(1:50)*rnorm(50))   #Strong non-constant variance
for(i in 1:9) plot(1:50,sqrt((1:50))*rnorm(50))   #Mild non-constant variance
for(i in 1:9) plot(1:50,cos((1:50)*pi/25)+rnorm(50))   #Non-linearity
par(mfrow=c(1,1))
#we could see that it's often hard to judge whether an apparent feature is real or just random variation.
#Repeated generation of plots under a model where there is no violation of the assumption
   #that the diagnostic plot is designed to check is helpful in making this judgement.

#epsilon_hat against x_i
plot(savings$pop15,g$res,xlab="pop15",ylab="Residuals")
plot(savings$pop75,g$res,xlab="pop75",ylab="Residuals")
plot(savings$dpi,g$res,xlab="dpi",ylab="Residuals")
#from the first plot we can see two groups. Let's compare and test the variances of these two groups
#Given two independent samples from normal distributions, we can test for equal variance using 
#the test statistic of the ratio of the two variance. 
#The null distribution is a F with degrees of freedom given by the two samples.
var(g$res[savings$pop15 > 35])/var(g$res[savings$pop15 <35])
table(savings$pop15 > 35)
1-pf(2.7851,22,26)    #A significant difference is seen



#(6) Deal with Non-Constant Variance
#two approaches to dealing with non-constant variance: 
   #Weighted least squares is appropriate when the form of the non-constant variance is either 
       #known exactly or there is some known parametric form. 
   #Alternatively, one can transform y to h(y) where h() is chosen so that var(h(y)) is constant.
       #how to choose h(y):
       #consider h(y)=h(Ey)+(y-Ey)h'(Ey) + ...
               #var(h(y)) = h'(Ey)^2 * var(y) + ...
       #--> For var(h(y)) to be constant we need:  h'(Ey) ~ (var(y))^(-0.5), 
             #solve the ODE: h(y) = integrate(dy/sqrt(var(y))) = integrate(dy/SDy)
       #For example if var(y)=var(epsilon) ~ (Ey)^2 then h(y)=log(y) is suggested 
             #while if var(epsilon) ~ E(y), then h(y) = sqrt(y). Graphically one tends to see SDy rather then var y.
       #Personal comment: we could regress var(epsilon) on (Ey)^2 or E(y). (data will from different sampling).
#A formal test may be good at detecting a particular kind of non-constant variance but have no 
#power to detect another. Residual plots are more versatile because unanticipated problems may be spotted.

       #Sometimes y_i<=0 for some i in which case the transformations may choke. 
             #You can try log(y+delta) for some small delta but this makes interpretation difficult.

#Consider the residual-fitted plot for the Galapagos data:
gg <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
plot(gg$fit,gg$res,xlab="Fitted",ylab="Residuals", main="Untransformed Response")
#We guess that a square root transformation will give us constant variance:
gs <- lm(sqrt(Species) ~ Area+ Elevation+ Scruz+ Nearest+ Adjacent, gala)
plot(gs$fit,gs$res,xlab="Fitted",ylab="Residuals",main="Square root Response")
#We see in the second plot that the variance is now constant. Our guess at a variance stabilizing 
#transformation worked out here, but had it not, we could always have tried something else.

#The poisson distribution is a good model for counts and that distribution has the property that 
  #the mean is equal to the variance thus suggesting the "square root transformation". It might be even 
  #better to go with a poisson regression rather than the normal-based regression we are using here.

#There are more formal tests for non-constant variance - for example one could start by regressing
  #|epsion_hat| on y or x_i but there is a problem in specifying the alternative hypothesis for such a test.



#(7) Deal with Non-Linearity 
#(recap: nonlinearity means residual shows an nonlinear pattern against fitted value, because the 
    #linear part has been explained by the linear combination of predictors, the nonlinear part
    #would be left in the residual.)
#Lack of fit tests can be used when there is replication which doesn't happen too often, but even 
#if you do have it, the tests don't tell you how to improve the model.

#How do we check if the systematic part (Ey = X*beta) of the model is correct?
        #1. Plots of epsilon_hat against y_hat and x_i
        #2. Plots of y against each xi.
#but what about the effect of other x on the y vs. xi plot?
#two methods:
#(a) Partial Regression, or called Added Variable, plots can help isolate the effect of xi on y.
    #1. Regress y on all x except xi, get residuals delta_hat. This represents y with the other X-effect taken out.
    #2. Regress xi on all x except xi, get residuals gamma_hat. This represents xi with the other X-effect taken out.
    #3. Plot delta_hat against gamma_hat.
         #so the slope of a line fitted to the plot must be beta_i_hat

#(b) Partial residual plots: plot (epsilon_hat + beta_i_hat * x_i) against x_i. -- only plot, no regression
    #Again the slope on the plot will be beta_j_hat and the interpretation is the same. 
#To see where this comes from, look at the response with the predicted effect of the other X removed:
    #x_i*beta_i_hat + epsilon_hat = y_hat + epsilon_hat - sum_{j!=i}(x_j*beta_j_hat) 
                                 #= y - sum_{j!=i}(x_j*beta_j_hat) 
    #therefore, the response with the predicted effect of the other X removed

####Rule: Partial residual plots are reckoned to be better for non-linearity detection while added variable 
#plots are better for outlier/influential detection.

#e.g.
#First we construct a partial regression (added variable) plot for pop15:
d <- lm(sr ~ pop75 + dpi + ddpi,savings)$res
m <- lm(pop15 ~ pop75 + dpi + ddpi,savings)$res
plot(m,d,xlab="pop15 with the other X-effect taken out",ylab="savings with the other X-effect taken out",main="Partial Regression")
#The slope of a line fitted to the plot is beta_i_hat.
lm(d ~ m)$coef    #slope: -4.611931e-01
#Compare the slope of the plot to the original regression and show the line on the plot
g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
g$coef      #slope of pop15: -0.4611931471  same with above.
abline(0,lm(d ~ m)$coef[2])
abline(0,g$coef['pop15'])   #same as above.
# In the partial regression plot,w e are looking at the marginal relationship between the response 
# and the predictor after the effect of the other predictors has been removed. Multiple regression 
# is difficult because we cannot visualize the full relationship because of the high dimensionality 
        #(the poins plot couldn't be draw with more than one predictors)
# The partial regression plot allows us to focus on the relationship between one predictor and the 
# response, much as in simple regression.

#A partial residual plot is easier to do:
plot(savings$pop15,g$res+g$coef['pop15']*savings$pop15,xlab="pop'n under 15", ylab="Saving(adjusted)",main="Partial Residual")
abline(0,g$coef['pop15'])
#or more directly:
prplot(g,1)

#compare added variable plots and Partial residual plots 
con<-par(mfrow=c(1,2))
#added variable plots are better for outlier/influential detection.
plot(m,d,xlab="pop15 with the other X-effect taken out",ylab="savings with the other X-effect taken out",main="Partial Regression")
abline(0,g$coef['pop15'])
#Partial residual plots are reckoned to be better for non-linearity detection 
plot(savings$pop15,g$res+g$coef['pop15']*savings$pop15,xlab="pop'n under 15", ylab="Saving(adjusted)",main="Partial Residual")
abline(0,g$coef['pop15'])
par(con) 

#From the partial regression, we see there might be a different relationship in the two groups.
    #(conclusion is the similar as shown in part 'epsilon_hat against x_i')
g1 <- lm(sr ~ pop15+pop75+dpi+ddpi,savings,subset=(pop15 > 35))
g2 <- lm(sr ~ pop15+pop75+dpi+ddpi,savings,subset=(pop15 < 35))
summary(g1)   #g1 is almost invalid.
summary(g2)


#Higher dimensional plots can also be useful for detecting structure that cannot be seen in two 
#dimensions. These are interactive in nature so you need to try them to see how they work. 
#Two ideas are:
    #1. Spinning - 3D plots where color, point size and rotation are used to give illusion of a third dimension.
    #2. Brushing - Two or more plots are linked so that point which are brushed in one plot are highlighted in another.
#These tools look good but it's not clear whether they actually are useful in practice. 
install.packages("D:\\study\\DataScience\\packages\\xgobi_1.2-15.tar.gz", repos=NULL, type="source")
install.packages("D:\\study\\DataScience\\packages\\rggobi_2.1.20.tar.gz", repos=NULL, type="source")
library(xgobi)
xgobi(savings)
library(rggobi)
ggobi(savings)


#(8) Assessing Normality
#The residuals can be assessed for normality using a Q-Q plot. 
#1. Sort the residuals: epsilon_1_hat <= epsilon_2_hat <= ... <=epsilon_n_hat
#2. Compute u_i = PHI(i/(n+1))^(-1)
#3. Plot epsilon_i_hat against u_i
   #If the residuals are normally distributed an approximately straight-line relationship will be observed.
qqnorm(g$res,ylab="Raw Residuals")  #qqnorm: produces a normal QQ plot of the values in y.
qqline(g$res) #qqline adds a line to a "theoretical", by default normal, quantile-quantile plot which passes through the probs quantile.

#We can plot the jackknife (externally) studentized residuals:
qqnorm(rstudent(g),ylab="Studentized residuals"); abline(0,1) 
   #Because these residuals have been normalized, they should lie along a 45 degree line

#Histograms and boxplots are not as sensitive for checking normality
hist(g$res,10)
boxplot(g$res,main="Boxplot of savings residuals")

#artificial e.g.:
#We can get an idea of the variation to be expected in QQ-plots in the following experiment. 
oldpar<-par(mfrow=c(3,3))
for(i in 1:9) {qqnorm(rnorm(50));abline(0,1)}
for(i in 1:9) qqnorm(exp(rnorm(50)))  #an example of a skewed distribution
for(i in 1:9) qqnorm(rcauchy(50))  #an example of a long-tailed (platykurtic) distribution
for(i in 1:9) {qqnorm(runif(50));abline(0,1)}   #Uniform - an example of a short-tailed (leptokurtic) distribution
                            #y limit is different from rnorm(50), although seems similar.
par(oldpar)

#It's not always easy to diagnose the problem in QQ plots.
#The consequences of non-normality are
#1. that the least squares estimates may not be optimal - they will still be BLUE but other 
    #robust estimators may be more effective.
#2. that the tests and confidence intervals are invalid. However, it has been shown that only 
    #really longtailed distributions cause a problem. Mild non-normality can safely be ignored 
    #and the larger the sample size the less troublesome the non-normality.
#3. For short-tailed distributions, the consequences of non-normality are not serious and can 
    #reasonably be ignored.

#There are formal tests for normality such as the Kolmogorov-Smirnov test but these are not as 
#flexible as the Q-Q plot. The p-value is not very helpful as an indicator of what action to take. 
#After all, with a large dataset, even mild deviations from non-normality may be detected, but 
#there would be little reason to abandon least squares because the effects of non-normality are 
#mitigated by large sample sizes. For smaller sample sizes, formal tests lack power.

#Extra: qqplot produces a QQ plot of two datasets.
y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)  #y-axis is sample quantiles. x-axis is theoretical quantiles
qqplot(y, rt(300, df = 5));   #x-axis is y; y-axis is rt(300, df = 5)
## Q-Q plot for Chi^2 data against true theoretical distribution:
qqplot(qchisq(ppoints(500), df = 3), y, main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
qqline(y, distribution = function(p) qchisq(p, df = 3), prob = c(0.1, 0.6), col = 2)
mtext("qqline(*, dist = qchisq(., df=3), prob = c(0.1, 0.6))")


#(9)Half-normal plots  --  used to distinguish influential points
#A half-normal distribution is the distribution of the |X| with X having a normal distribution.

#They could be used for |epsilon_hat| but are more typically useful for diagnostic quantities 
     #like the leverages or the Cook Statistics. ()

#The idea is to plot the data against the positive normal quantiles:
   #1.sort the data: x_1 <= x_2 <= ... <= x_n    #x is |epsilon_hat| or leverages or Cook statistics.
   #2. Compute u_i = PHI((n+i)/(2n+1))^(-1)
   #3. Plot x_i against u_i

#the half-normal plot on the leverages and Cook statistics for the savings data:
halfnorm<-function(x, nlab = 2, labs = as.character(1:length(x)), ylab = "Sorted Data", ...) {
        x <- abs(x)
        labord <- order(x)
        x <- sort(x)
        i <- order(x)
        n <- length(x)
        ui <- qnorm((n + 1:n)/(2 * n + 1))
        plot(ui, x[i], xlab = "Half-normal quantiles", ylab = ylab, 
             ylim = c(0, max(x)), type = "n", ...)
        if (nlab < n) points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
        text(ui[(n - nlab + 1):n], x[i][(n - nlab + 1):n], labs[labord][(n -nlab + 1):n])
}

halfnorm(lm.influence(g)$hat,labs=countries,ylab="Leverages")
halfnorm(cooks.distance(g),labs=countries,ylab="Cook Statistics")


#(10) Correlated Errors
#check the uncorrelated assumption.
   #1. Plot epsilon_hat(residual) against time.
   #2. Use formal tests like the Durbin-Watson or the run test.
#If you do have correlated errors, you can use GLS. This does require that you know SIGMA or more 
   #usually that you can estimate it with iteration. 
data(airquality)
airquality
pairs(airquality,panel=panel.smooth)
g <- lm(Ozone ~ Solar.R + Wind + Temp,airquality)
summary(g)
#residual plot:
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals",main="Untransformed Response")
#Notice there are only 107 degrees corresponding to the 111 complete observations
#We see some non-constant variance and nonlinearity and so we try transforming the response:
gl <- lm(log(Ozone) ~ Solar.R + Wind + Temp,airquality)
#residual plot:
plot(gl$fit,gl$res,xlab="Fitted",ylab="Residuals",main="Logged Response")
#Suppose we are now otherwise satisfied with this model and want to check for serial correlation.
#The missing values in the data were not used in the construction of the model but this also 
#breaks up the sequential pattern in the data. I get round this by reintroducing missing values 
#into the residuals corresponding to the omitted cases.
res <- rep(NA,153)
res[as.numeric(row.names(na.omit(airquality)))] <- gl$res
plot(res,ylab="Residuals",main="Index plot of residuals")
#plot successive residuals:
plot(res[-153],res[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
#Do you see any problem? Let's check AR(1) for the residual
summary(lm(res[-1] ~ -1+res[-153]))  #We omitted the intercept term because the residuals have mean zero.
#We see that there is no significant correlation.

#You can plot more than just successive pairs if you suspect a more complex dependence. For spatial
#data, more complex checks are required.


###recap of impute library to fill in missing values by knn to that row
source("http://bioconductor.org/biocLite.R")
biocLite("impute")
library("impute")
set.seed(678910)
for(i in 1:40){
        #flip a coin
        coinFlip1<-rbinom(1,size=1,prob=0.5)
        coinFlip2<-rbinom(1,size=1,prob=0.5) #set.seed(678910) also applies to this line    
        #if coin is head, add a common pattern to that row
        if(coinFlip1){
                dataMatrix[i,]<-dataMatrix[i,]+rep(c(0,5),each=5)  #0 0 0 0 0 5 5 5 5 5
        }
        if(coinFlip2){
                dataMatrix[i,]<-dataMatrix[i,]+rep(c(0,5),5) #0 5 0 5 0 5 0 5 0 5
        }        
}
hh<-hclust(dist(dataMatrix))
dataMatrixOrdered<-dataMatrix[hh$order,]
dataMatrix2<-dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA  ##Randomly insert some missing data
svd1<-svd(scale(dataMatrix2))   #Doesn't work: infinite or missing values in 'x'
dataMatrix2<-impute.knn(dataMatrix2)$data  #impute(assign) missing data points, take a missing values in a row and impute it by knn to that row
svd1<-svd(scale(dataMatrixOrdered))  #svd on the pre-ordered matrix would have better effect.
svd2<-svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)
###end of recap
















