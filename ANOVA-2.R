setwd("D:\\study\\DataScience")
library(faraway)

#8.Transformation
#(1) Transforming the response - log,Box-Cox,logit,Fisher's z
#In log(y) = beta0 + beta1*x + epsilon --> y=exp(beta0+beta1 * x)*exp(epsilon)
   #the errors enter multiplicatively and not additively as they usually do. So the use of
   #standard regression methods for the logged response model requires that we believe that the errors
   #enter multiplicatively in the original scale.
#Notice that if we believe the proper model for y to be y=exp(beta0+beta1 * x) + epsilon, then we cannot 
   #linearize this model and non-linear regression methods would need to be applied.
#As a practical matter, we often do not know how the errors enter the model, additively, multiplicatively
#or otherwise. The usual approach is to try different transforms and then check the residuals to see
#whether they satisfy the conditions required for linear regression.

#When you use a log transformation on the response, the regression coefficients have a particular 
#interpretation: An increase of one in x1 would multiply the predicted response (in the original scale) 
#by exp(beta1)

#The Box-Cox method is a popular way to determine a tranformation on the response. It is designed for 
#strictly positive responses and chooses the transformation to find the best fit to the data.
  #y --> t_lmabda(y) where the family of transformations indexed by lambda is:
  #t_lmabda(y) = (y^lambda - 1)/lambda  if lambda != 0;  t_lmabda(y) = log(y)  if lambda = 0;
  #For fixed y > 0, t_lmabda(y) is continuous in lambda. Choose lambda using maximum likelihood.
#The profile log-likelihood assuming normality of the errors is
  #L(lambda) = -n/2 * log(RSS_lambda/n) + (lambda-1)*sum(log(y_i))
       #where where RSS_lambda is the RSS when t_lmabda(y) is the response.
#You can compute lambda_hat exactly to maximize this but usually L(lambda) is just maximized over a 
#grid of values such as {-2,-1,-1/2,0,1/2,1,2}. This ensures that the chosen lambda_hat is more easily interpreted. 
   #For example, if lambda_hat = 0.46, it would be hard to explain what this new response means, 
   #but sqrt(y) would be easier.

#necessity of Box-Cox transformation: One way to check this is to form a confidence interval for lambda. 
   #A 100*(1-alpha)% confidence interval for lambda is {lambda: L(lambda)>L(lambda_hat) - 1/2 * chisq_{1,1-alpha}
#This interval can be derived by inverting the likelihood ratio test of the hypothesis that 
   #H0 : lambda = lambda0 which uses the statistic 2(L(lambda_hat)-L(lambda0)) having approximate 
   #null distribution chisq_{1}.

library(MASS)
data(savings)
g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
boxcox(g,plotit=T)     #too broad
boxcox(g,plotit=T,lambda=seq(0.5,1.5,by=0.1))
#The confidence interval for lambda runs from about 0.6 to about 1.4 (95% CI). We can see that 
#there is no good reason to transform.

#another dataset
data(gala)
g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
boxcox(g,plotit=T)
boxcox(g,lambda=seq(0.0,1.0,by=0.05),plotit=T)
#lambda is between 0.1~.5. perhaps a cube-root transformation might be best here. A square root is also 
#a possibility as this falls just within the confidence intervals.

#Note of Box-Cox method:
#(a)The Box-Cox method gets upset by outliers - if you find lambda_hat = 5 then this is probably the reason.
   #there can be little justification for actually making such an extreme transformation.
#(b)What if some y_i < 0? Sometimes adding a constant to all y can work provided that constant is small.
#(c)If max_i(y_i)/min_i(y_i) is small then the Box-Cox won't do anything because power transforms are 
   #well approximated by linear transformations over short intervals.
#(d)Should the estimation of lambda count as an extra parameter to be taken account of in the degrees of
   #freedom? This is a difficult question since lambda is not a linear parameter and its estimation is 
   #not part of the least squares fit.

###The Box-Cox method is not the only way of transforming the predictors. 
#For responses, that are proportions (or percentages), the logit transformation, log(y/(1-y) is often used, 
#while for responses that are correlations, Fisher's z transform, y=0.5log(1+y)/(1-y)) is worth considering.



#(2)Transforming the predictors - Broken Stick Regression(linear spline), Polynomials transformation
# You can take a Box-Cox style approach for each of the predictors, choosing the transformation to minimize
# the RSS. However, this takes time and furthermore the correct transformation for each predictor may depend
# on getting the others right too. Partial residuals (plot (epsilon_hat + beta_i_hat * x_i) against x_i)
# are a good way of finding suggestions for transforming the predictors (to see whether x_i is linear).

###Broken Stick Regression
#first let's try subsetted regression
#Sometimes different linear regression models need to be applied in different regions of the data. 
#e.g. in the analysis of the savings data, we observed that there were two groups in the data
g1 <- lm(sr ~ pop15, savings, subset=(pop15 < 35))
g2 <- lm(sr ~ pop15, savings, subset=(pop15 > 35))
plot(savings$pop15,savings$sr,xlab="Pop'n under 15",ylab="Savings Rate")
abline(v=35,lty=5)
segments(20,g1$coef[1]+g1$coef[2]*20,  35,g1$coef[1]+g1$coef[2]*35)
segments(48,g2$coef[1]+g2$coef[2]*48,  35,g2$coef[1]+g2$coef[2]*35)
#segments(x0, y0, x1 = x0, y1 = y0, col = par("fg"), lty = par("lty"), lwd = par("lwd"), ...) 
    #x0, y0: coordinates of points from which to draw.  
    #x1, y1: coordinates of points to which to draw. At least one must be supplied.

#A possible objection to this subsetted regression fit is that the two parts of the fit do not meet at the join.
#If we believe the fit should be continuous as the predictor varies, then this is unsatisfactory.
#broken stick regression fit could solve this problem
#Define two basis functions:
   #Bl(x) = c-x if x<c; Bl(x) = 0 otherwise
   #Br(x) = x-c if x>c; Br(x) = 0 otherwise
      #where c marks the division between the two groups. 
      #Bl and Br form a first-order spline basis with a knotpoint at c.
      #Sometimes Bl and Br are called hockey-stick functions because of their shape. 
# We can now fit a model of the form: y = beta0 + beta1*Bl(x) + beta2*Br(x) + epsilon
    #Then the two linear parts are guaranteed to meet at c. 
    #Notice that this model uses only three parameters in contrast to the four total parameters 
    #used in the subsetted regression. A parameter has been saved by insisting on the continuity of the fit at c.

lhs <- function(x) ifelse(x < 35,35-x,0)   #first-order spline basis
rhs <- function(x) ifelse(x < 35,0,x-35)
gb <- lm(sr ~ lhs(pop15) + rhs(pop15), savings)
x <- seq(20,48,by=1)
py <- gb$coef[1]+gb$coef[2]*lhs(x)+gb$coef[3]*rhs(x)
lines(x,py,lty=2)

#We can have more than one knotpoint simply by defining more pairs of basis functions with different
#knotpoints. Broken stick regression is sometimes called segmented regression.


###Polynomials - y = beta0 + beta1 * x + ... + betad * x^d + epsilon
#There are two ways to choose the order d:
   #1. Keep adding terms until the added term is not statistically significant.
   #2. Start with a large d - eliminate not statistically significant terms starting with the highest order term.
#Warning: Do not eliminate lower order terms from the model even if they are not statistically significant.

#Let's see if we can use polynomial regression on the ddpi variable in the savings data.
#First fit a linear model:
summary(lm(sr ~ ddpi,savings))
#p-value of ddpi is significant so move on to a quadratic term:
summary(lm(sr ~ ddpi+I(ddpi^2),savings))
#Again the p-value of ddpi^2 is significant so move on to a cubic term:
summary(lm(sr ~ ddpi+I(ddpi^2)+I(ddpi^3),savings))
#p-value of ddpi^3 is not significant so stick with the quadratic.

##Orthogonal polynomials
# You have to refit the model each time a term is removed and for large d there can be problem with
# numerical stability. Orthogonal polynomials get round this problem by defining
#         z1 = a1 + b1*x
#         z2 = a2 + b2*x + c2*x^2
#         z3 = a3 + b3*x + c3*x^2 + d3*x^3, etc.
#            where where the coefficients a,b,c ... are chosen so that z_i'*z_j = 0 when i!=j.
          #The z are called orthogonal polynomials.
#The poly() function constructs Orthogonal polynomials.
g <- lm(sr ~ poly(ddpi,4),savings)
summary(g)
#We can verify the orthogonality of the design matrix when using orthogonal polynomials:
x <- model.matrix(g)
dimnames(x) <- list(NULL,c("Int","power1","power2","power3","power4"))
round(t(x) %*% x,3)

#You can have more than two predictors as can be seen in this "response surface" model:
#y=beta0 + beta1*x1 + beta2*x2 + beta11*(x1)^2 + beta22*(x2)^2 + beta12*x1*x2


#(3)Regression Splines - S_{k,t}(x) = sum_{i}(alpha_i * B_{i,k}(x)) , where k is the order of spline function
#Polynomials have the advantage of smoothness but the disadvantage that each data point affects the fit globally.
#In contrast, the broken stick regression method localizes the influence of each data point to its particular segment 
    #but we do not have the same smoothness as with the polynomials.
#B-spline basis functions combines the beneficial aspects of both these methods - smoothness and local influence

#We may define a cubic B-spline basis on the interval [a,b] by the following requirements on the interior
#basis functions with knot-points at t1 ... tk.
   #(a) A given basis function is non-zero on interval defined by four successive knots and zero elsewhere.
       #This property ensures the local influence property.
   #(b) The basis function is a cubic polynomial for each sub-interval between successive knots
   #(c) The basis function is continuous and continuous in its first and second derivatives at each knot point.
       #This property ensures the smoothness of the fit.
   #(d) The basis function integrates to one over its support


#Suppose we know the true model is y=sin(2*pi*x^3)^3 + epsilon,    epsilon ~ N(0,(0.1)^2)
funky <- function(x) sin(2*pi*x^3)^3
x <- seq(0,1,by=0.01)     #101 elements
y <- funky(x) + 0.1*rnorm(101)
matplot(x,cbind(y,funky(x)),type="pl",ylab="y",pch=18,lty=1, main="True Model")
#We see how an orthogonal polynomial bases of orders 4 and 12 do in fitting this data:
g4 <- lm(y ~ poly(x,4))
g12 <- lm(y ~ poly(x,12))
matplot(x,cbind(y,g4$fit,g12$fit),type="pll",ylab="y",pch=18,lty=c(1,2),main="Orthogonal Polynomials")
   #lty: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
   #the dashed line is g4$fit, the solid line is g12$fit. So lty is in reverse order.
#We see that order 4 is a clear underfit. Order 12 is much better although the fit is too wiggly

#We now create the B-spline basis.  
#You need to have three additional knots at the start and end to get the right basis.
##The basis functions at the ends of the interval are defined a little differently to ensure continuity in
#derivatives at the edge of the interval. I have chosen to the knot locations to put more in regions of 
#greater curvature. I have used 12 basis functions for comparability to the orthogonal polynomial fit.
library(splines)
knots <- c(0,0,0,0,0.2,0.4,0.5,0.6,0.7,0.8,0.85,0.9,1,1,1,1)
bx <- splineDesign(knots,x)  #generate a matrix with length(x) rows and length(knots) - ord columns.
#splineDesign(knots, x, ord = 4, derivs, outer.ok = FALSE,sparse = FALSE)
   #x: a numeric vector of values at which to evaluate the B-spline functions or derivatives. 
        #Unless outer.ok is true, the values in x must be between 
               #knots[ord] and knots[length(knots)+1-ord].  
   #(the interval is 0~1 in current case, so we need to fill three zeros and three 1 at the both ends of knots)
   #ord:the number of coefficients in each piecewise polynomial segment, thus a cubic spline has 
        #order 4. Defaults to 4.
gs <- lm(y ~ bx)
matplot(x,bx,type="l",main="B-spline basis functions")
matplot(x,cbind(y,gs$fit),type="pl",ylab="y",pch=18,lty=1,main="Spline fit") #fit comes very close to the truth.



#(4)Modern Methods
#The methods described above are somewhat awkward to apply exhaustively and even then they may miss
#important structure because of the problem of trying to find good transformations on several variables 
#simultaneously.

#One recent approach is the additive model:
#y = beta0 + f1(x1) + f2(x2) + ... + fp(xp) + epsilon
   #where nonparametric regression techniques are used to estimated the fi's.
#Alternatively, you could implement this using the regression spline bases for each predictor variable. 

#Other techniques are ACE, AVAS, Regression Trees, MARS and neural networks.
    #neural networks is hard to interpret, so it's usually only good for predictive purposes

#strengths(models are easier to interpret) and weaknesses of regression analysis:
#For larger data sets with relatively little noise, more recently developed complex models will be able 
#to fit the data better while keeping the number of parameters under control.
#For smaller data sets or where the noise level is high (as is typically found in the social sciences), 
#more complex models are not justified and standard regression is most effective.



#2.Scale Changes, Principal Components and Collinearity

#(1)Change of Scale
#Suppose xi-->(xi + a)/b
#!!!!!!!!!!important!!!!!!!!!!!######
#Rescaling xi leaves the t and F tests and simga_hat^2 and R^2 unchanged and betai_hat --> b*betai_hat
#Rescaling y in the same way leaves the t and F tests and R^2 unchanged but simga_hat and betai_hat will rescaled by b.
#To demonstrate this, we use same old model:
g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
summary(g)
g <- lm(sr ~ pop15+pop75+I(dpi/1000)+ddpi,savings)
summary(g)
#One rather thorough approach to scaling is to convert all the variables to standard units (mean 0 and
#variance 1) using the scale() command:
scsav <- data.frame(scale(savings))
g <- lm(sr ~ ., scsav)
summary(g)
#As may be seen, the intercept is zero. This is because the regression plane always runs through the
#point of the averages which because of the centering is now at the origin.
#Such scaling has the advantage of putting all the predictors and the response on a comparable scale, 
#which makes comparisons simpler.It also allows the coefficients to be viewed as kind of partial 
#correlation - the values will always be between -1 and 1.
   #The downside of this scaling is that this might not always be easy to interpret.


#(2) Principal Components -- transform the X to orthogonality.
#Recall that if the X matrix is orthogonal then testing and interpretation are greatly simplified. One purpose
    #for principal components is to transform the X to orthogonality (another purpose is get 
    #the pincipal components, as we all knew).
#We wish to find a rotation p*p matrix U such that: Z=XU, and Z'Z = diag(lambda1,...,lambdap), 
#where lambda1>=lambda2>=...>=lambdap>=0 (ranked eigenvalues of X'X), Zero eigenvalues indicate non-identifiability
#Since Z'Z = U'X'XU, the eigenvectors of X'X are the columns of U.
#The columns of Z are called the principal components and these are orthogonal to each other. -- same as the v matrix in column-scaled  svd
    #lambdai is also the variance of Zi

#note:
   #The principal components are linear combinations of the predictors - little is gained if these are
   #not interpretable.
   #There are variations which involve not using the intercept in X TX or using the correlation matrix of
   #the predictors instead of X'X.
#eigendecomposition of X'X:
data(longley)
head(longley)
x <- as.matrix(longley[,-7])   
    #when calculating xtx in regression, x is always include 1 as the intercept.But here it is not regression
e <- eigen(t(x) %*% x)
e$values
class(e$vectors)
dimnames(e$vectors)[[1]] <- colnames(longley[,-7])
dimnames(e$vectors)[[2]] <- paste("EV",1:6)   #the second element of dimnames(e$vectors) is the column names
round(e$vec,3)   #The first eigenvector is dominated by year, but this may caused by different scale of variables
pca_unscale<-prcomp(x,center=FALSE); pca_unscale$rotation    #same with the above

#Now examining the X-matrix. What are the scales of the variables?
x  #We see that the variables have different scales. It might make more sense to standardize the predictors before
   #trying principal components. This is equivalent to doing principal components on the correlation matrix.
e <- eigen(cor(x))  #eigenvectors are same as eigen(t(scale(x))%*%scale(x)), but the eigenvalues are not.
e$values
dimnames(e$vectors) <- list(c("GNP def","GNP","Unem","Armed","Popn","Year"),paste("EV",1:6))
round(e$vec,3)
#or we could simply use prcomp function to get the same result.
pca_scale<-prcomp(x,scale=TRUE); pca_scale$rotation  

plot(e$values,type="l",xlab="EV no.")
plot(cumsum(e$values)/sum(e$values),type="l",xlab="EV no.")  #we need only consider 2 principal components.

#transforms the predictors to an orthogonal basis, which is based on the eigendecomposition for the correlation matrix
#therefore we must first standardize the data:
nx <- scale(x)   #notice that eigen(cor(x)) is same with eigen(scale(x)).
#We can now create the orthogonalized predictors - the Z = XU operation (so X and U here are both from scaled X)
enx <- nx %*% e$vec  #because e$vec is from correlation matrix of x
c(enx[,1]%*%enx[,2],enx[,1]%*%enx[,3])   #approx zero.
#then fit:    #so-called principal component regression (PCR)
g <- lm(longley$Emp ~ enx)
summary(g)  #we could just look at the first 2 or three predictors
#We can take a look at the (X'X)^(-1) matrix:
round(summary(g)$cov.unscaled,4)  #a diagonal matrix

#Principal components are really only useful if we can interpret the meaning of the new linear 
#combinations. Look back at the first eigenvector - this is roughly a linear combination of all the 
#(standardized variables) - most elements of EV1 are around 0.46.
e$vec
#Now plot each of the variables as they change over time. we could find most variables are linearly
    #related to the time. So the first component has a time trend effect.
par(mfrow=c(3,2))
for(i in 1:6) plot(longley[,6],longley[,i],xlab="Year",ylab=names(longley)[i])

#The second principal component is roughly a contrast between numbers unemployed and in the armed
#forces(Unem's coefficient is -0.59 in EV2, and Armed's is 0.798, they have largest contrast). 
#Let's try fitting a regression with those two components:
summary(lm(Employed ~ Year + I(Unemployed-Armed.Forces),longley))

#We could do more work on the other principal components.
#for the third component, we could see that it's roughly a contrast between Unem+Armed and others.
summary(lm(Employed ~ Year + I(Unemployed-Armed.Forces) + I(Unemployed+Armed.Forces-GNP.deflator-GNP-Population),longley))
#This is illustrates a typical use of principal components for regression. 


#(3)Partial Least Squares
#PCR regresses the response on the principal components of X while PLS finds the best orthogonal 
#linear combinations of X for predicting Y.

#We will consider only univariate PLS - that is to say l = 1 so that Y is scalar. This is the typical 
#multiple regression setting. We will attempt to find models of the form y_hat = beta1*T1+...+betap*Tp,
   #where Tk is a linear combination of the X's. p may smaller than number f variables in X.

#We'll assume that all variables have been centered to have mean 0 - this means that our intercept terms
#will always be zero.Here is the algorithm for determining the T's.
   #(a) Regress Y on each Xi in turn to get b_1i. 
   #(b) Form T1=sum_{i~m}(w_1i * b_1i * X_1i), where the weights w_1i sum to one.
   #(c) Regress Y on T1 and each Xi on T1. The residuals from these regressions have the effect of T1 
       #removed. Replace Y and each Xi by the residuals of each corresponding regression.
   #(d) Go back to step one updating the index.
#There are various choices for the weighting scheme:
#(a)Set w_ij = 1/m giving each predictor equal weight.
#(b)Set w_ij ~ var(Xj). This is the most common choice. The variances of the b_ij are then inversely 
   #proportional to var(Xj) which does make some sense.

#The algorithm ensures that the components Ti are uncorrelated just as the principal components are
#uncorrelated. This means that betai will not change as more components are added to or subtracted 
#from the model.
x <- as.matrix(longley[,-7])
y <- longley$Emp
cx <- sweep(x,2,apply(x,2,mean))   #center x
cy <- y - mean(y)
#Now do the PCR using a more direct method than we used above:
library(MVA)
ex <- princomp(cx)
g <- lm(cy ~ ex$scores -1)
summary(g)
#same result:
pca_scale<-prcomp(cx,scale=FALSE)  #we can't use eigen(cor(x))$vector here because cor(x) will standardize x, while here we just centralize x.
z2<-cx%*%pca_scale$rotation
g2 <- lm(cy ~ z2 -1)

#Are the principal component scores ordered in terms of their importance in predicting the response?
#Now for later comparison, we have the regression on just first PC.
g <- lm(cy ~ ex$scores[,1] -1)
summary(g)

#Now we compute the first component of the partial least squares regression:
#(a) Regress Y on each Xi in turn to get b_1i. 
#(b) Form T1=sum_{i~m}(w_1i * b_1i * X_1i), where the weights w_1i sum to one.  --> this step is hardest to understand and remember
#(c) Regress Y on T1 and each Xi on T1. The residuals from these regressions have the effect of T1 
    #removed. Replace Y and each Xi by the residuals of each corresponding regression.
#(d) Go back to step one updating the index.
#Two ways to choose the w_1i:
#(a)Set w_ij = 1/m giving each predictor equal weight.
#(b)Set w_ij ~ var(Xj). This is the most common choice. The variances of the b_ij are then inversely 
#proportional to var(Xj) which does make some sense.

b1 <- numeric(6)
for(i in 1:6){b1[i] <- crossprod(cx[,i],cy)/crossprod(cx[,i],cx[,i])}
b1       #coefficient or xi, b_1i's
ncx <- sweep(cx,2,b1,"*")
#(a)Set w_ij = 1/m giving each predictor equal weight.
t1 <- apply(ncx,1,mean)   #mean function implicitly sets the weight 1/m.

#Here we have a chosen an equal weighting for the variables. Now see how well this component predicts
#the response:
gpls1 <- lm(cy ~ t1 -1)
summary(gpls1)
#Compare this to the result above for one principal component.

#An alternative weighting scheme assigns the weights proportional to the variances of the variables:
#(b)Set w_ij ~ var(Xj). This is the most common choice. The variances of the b_ij are then inversely 
#proportional to var(Xj) which does make some sense. 
    # Explain: var(beta_hat) = (X'X)^(-1) * sigma^2

varx <- apply(cx,2,var)       #each variables' variance.
vncx <- sweep(ncx,2,varx/sum(varx),"*")   #ncx = b_i * cx_i
t1a <- apply(vncx,1,sum)     #to calculate t1, we still need to sum w1i*b1i*x1i together
gpls1a <- lm(cy ~ t1a - 1)
summary(gpls1a)

#Now we compute the second component of the PLS (based on the first component). We need to regress 
#out the effect of the first component and then use the same computational method as above.
cx2 <- matrix(0,16,6)
for(i in 1:6){cx2[,i] <- lm(cx[,i] ~ t1-1)$res}
cy2 <- lm(cy ~ t1 -1)$res
b2 <- numeric(6)
for(i in 1:6){b2[i] <- crossprod(cx2[,i],cy2)/crossprod(cx2[,i],cx2[,i])}
ncx2 <- sweep(cx2,2,b2,"*")
t2 <- apply(ncx2,1,mean)

#Notice the correlation of the components is almost zero:
cor(t1,t2)

#Now add t2 to the regression:
gpls2 <- lm(cy ~ t1+t2 -1)
summary(gpls2)
#Compare the coefficient of t1 with that above. 
summary(gpls1)   #coefficients of t1 are equal

#Now compare this fit to the two component PCR.
g <- lm(cy ~ ex$scores[,1:2] -1)
summary(g)

#The tricky part is choosing how many components are required. Crossvalidation is a possible way of
#selecting the number of components. PLS has been criticized as an algorithm that solves no 
#well-defined modeling problem. PLS has the biggest advantage over ordinary least squares and PCR 
#when there are large numbers of variables relative to the number of case. It does not even require n>=p.

#Which one is superior in explaining y?
#PCR:  attempts to find linear combinations of the predictors that explain most of the variation in these
   #predictors using just a few components. --> The purpose is dimension reduction.Because the principal 
   #components can be linear combinations of all the predictors, the number of variables used is not 
   #always reduced. Because the principal components are selected using only the X-matrix and not the 
   #response, there is no definite guarantee that the PCR will predict the response particularly well 
   #although this often happens. !!! PCR is geared more towards explanation than prediction.
#PLS: finds linear combinations of the predictors that best explain the response. It is most
   #effective when ther are large numbers of variables to be considered. If successful, the 
   #variablity of prediction is substantially reduced. !!! PLS is virtually useless for explanation purposes.


#(4) Collinearity
#If X'X is singular, i.e. some predictors are linear combinations of others, we have (exact) 
   #collinearity and there is no unique least squares estimate of beta. This causes serious problems 
   #with the estimation of beta and associated quantities as well as the interpretation.

#Collinearity can be detected in several ways:
   #(a)Examination of the correlation matrix of the predictors will reveal large pairwise collinearities.
   #(b)A regression of xi on all other predictors gives Ri^2. Repeat for all predictors. Ri^2 close to one 
      #indicates a problem - the offending linear combination may be found.
   #(c)Examine the eigenvalues of X'X - small eigenvalues indicate a problem. The condition number is
      #defined as k=sqrt(lambda1/lambdap). k is called the condition number. where k>=30 is considered large.

#Collinearity makes imprecise estimates of beta - the signs of the coefficients may be misleading.
  #t-tests which fail to reveal significant factors, missing importance of predictors
  #Define S_{xi,xj} = sum_{i}(xij - xj_bar)^2, then var(betaj_hat)=sigma^2*(1/(1-Rj^2))/S_{xj,xj}
  #so if xj doesn't vary much (S_{xj,xj} is small), the variance of betaj_hat will be large. 
        ###As an aside, in PCA the variance of the first principal component is maximized and so the 
        #variance of the corresponding regression coefficient will tend to be small. 
        #Orthogonality means that Rj^2=0 which minimizes the variance. Also we can maximize S_{xj,xj} 
        #by spreading X as much as possible: The maximum is attained by placing half the points at the 
        #minimum practical value and half at the maximum. Unfortunately, this design assumes the 
        #linearity of the effect and would make it impossible to check for any curvature. So, in 
        #practice, most would put some design points in the middle of the range to allow checking
        #of the fit.

#variance inflation factor: 1/(1-Rj^2) -- only related with X
g <- lm(Employed ~ ., longley)
summary(g)
#Three of the predictors have large p-values but all are variables that might be expected to affect 
#the response. Why aren't they significant? Check the correlation matrix first 
#(a)Examination of the correlation matrix of the predictors will reveal large pairwise collinearities.
round(cor(longley[,-7]),3)
#There are several large pairwise correlations. Now we check the eigendecomposition:
x <- as.matrix(longley[,-7])
e <- eigen(t(x) %*% x)
e$val
#check the condition number
sqrt(e$val[1]/e$val)

#There is a wide range in the eigenvalues and several condition numbers are large. This means that
#problems are being caused by more than just one linear combination. Now check out the variance inflation
#factors(1/(1-R^2)). 
summary(lm(x[,1] ~ x[,-1]))$r.squared
1/(1-0.99262)   #135.5014, which is large - the VIF for orthogonal predictors is 1.

#Now we compute all the VIF's in one go:
vif(x)   #normally take <10 as the cut-off point. vif>=100 is serious collinear.
#the standard error for GNP is 42 (=sqrt(1788)) times larger than it would have been without collinearity!
#How can we get rid of this problem? One way is to throw out some of the variables.
#Notice that variables 3 and 4 do not have extremely large pairwise correlations with the other
#variables so we should keep them and focus on the others for candidates for removal:
cor(x[,-c(3,4)])
#These four variables are strongly correlated with each other - any one of them could do the job of
#representing the other. We pick year arbitrarily:
summary(lm(Employed ~ Armed.Forces + Unemployed + Year,longley))
summary(g)

#Illustrate extreme collinearity can cause problems in computing the estimates 
#look what happens when we use the direct formula for beta_hat:
x <- as.matrix(cbind(1,longley[,-7]))
solve(t(x) %*% x , t(x) %*% longley$Emp, tol = 1e-3)   #error once the tolerance is too small.
solve(t(x) %*% x) %*% t(x) %*% longley[,7]



#(5)Ridge Regression
#It is appropriate for use when the design matrix is collinear and the usual least squares estimates 
#of beta appear to be unstable. 
#Another way of looking at is to suppose we place to some upper bound on beta'*beta and then compute the
#least squares estimate of beta subject to this restriction. Use of Lagrange multipliers leads to ridge regression.

#Suppose that the predictors have been centered by their means and scaled by their standard deviations
#and that the response has been centered. then beta_hat = (X'X + lambda*I)^(-1)*X'y. 
            #The ridge constant lambda is usually selected from the range [0,1].

#lambda may be chosen by automatic methods but it is probably safest to plot the values of beta_hat 
#as a function of lambda. You should pick the smallest value of lambda that produces stable estimates of beta.
#also, we could use AIC/BIC or cross validation to choose lambda.
library(MASS)
gr <- lm.ridge(Employed ~ .,longley,lambda = seq(0,0.1,0.001))
par(mfrow=c(1,1))
matplot(gr$lambda,t(gr$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
abline(h=0,lwd=2)
#Various automatic selections for lambda are available
select(gr)
abline(v=0.00428)
#The Hoerl-Kennard (the originators of ridge regression) choice of lambda has been shown on the plot but I
#would prefer a larger value of 0.03. Then beta_hat are:
gr$coef[,gr$lam == 0.03]
#in contrast to the least squares estimates of
gr$coef[,1]
#which is more reasonable?
#Consider the change in the coefficient for GNP. For the least squares fit, the effect of GNP is
#negative on the response - number of people employed. This is counter-intuitive since we'd expect 
#the effect to be positive. The ridge estimate is positive which is more in line with what we'd expect.

#Note that these values are based on centered and scaled predictors which explains the difference from
#previous fits. 

#Ridge regression estimates of coefficients are biased. Bias is undesirable but there are other 
#considerations. The mean squared error can be decomposed in the following way:
 #E[(beta_hat - beta)^2] = (E[beta_hat - beta])^2 + E[(beta_hat - E[beta_hat])^2] = bias^2 + variance.
#Therefore sometimes a large reduction in the variance may obtained at the price of an increase in the bias. 
#If the MSE is reduced as a consequence then we may be willing to accept some bias. This is the 
#trade-off that Ridge Regression makes - a reduction in variance at the price of an increase in bias. 
#This is a common dilemma.



#10. Variable Selection
#Prior to variable selection(!!!!important!!!!!):
   #1. Identify outliers and influential points - maybe exclude them at least temporarily.
   #2. Add in any transformations of the variables that seem appropriate.

#(1)Hierarchical Models
#Some models have a natural hierarchy. For example, in polynomial models, x^2 is a higher order term 
#than x. When selecting variables, it is important to respect the hierarchy. Lower order terms should 
#not be removed from the model before higher order terms in the same variable.

#There two common situations where this situation arises:
#(a)Polynomials models. consider model y=beta0 + beta1*x + beta2 * x^2 + epsilon
     #Removal of the first order term here corresponds to the hypothesis that the predicted response 
     #is symmetric about and has an optimum at x = 0. Often this hypothesis is not meaningful and 
     #should not be considered.  Only when this hypothesis makes sense in the context of the 
     #particular problem could we justify the removal of the lower order term.
#(b)Models with interactions. consider model y = beta0 + beta1*x1 + beta2*x2 + beta11*x1^2 + beta22*x2^2 + beta12*x1*x2
    #We would not normally consider removing the x1x2 interaction term without simultaneously 
    #considering the removal of the x1^2 and x2^2 terms. A joint removal would correspond to the clearly 
    #meaningful comparison of a quadratic surface and linear one.Just removing the x1x2 term would 
    #correspond to a surface that is aligned with the coordinate axes.Any rotation of the predictor 
    #space would reintroduce the interaction term and, as with the polynomials.

#(2)Stepwise Procedures

#Backward Elimination: simplest of all variable selection procedures
        # 1. Start with all the predictors in the model.
        # 2. Remove the predictor with highest p-value greater than alpha_crit (doesn't have to be 5%).
        # 3. Refit the model and goto 2.
        # 4. Stop when all p-values are less than alpha_crit.
#If prediction performance is the goal, then a 15-20% cut-off may work best, although methods designed 
#more directly for optimal prediction should be preferred.

#(1)Forward Selection: 
#1. Start with no variables in the model.
#2. For all predictors not in the model, check their p-value if they are added to the model. Choose 
   #the one with lowest p-value less than alpha_crit.
#3. Continue until no new predictors can be added.

#(2)Stepwise Regression - a combination of backward elimination and forward selection
#Stepwise procedures are relatively cheap computationally but they do have some drawbacks.
   #Because of the "one-at-a-time" nature of adding/dropping variables, it's possible to miss the "optimal" model.
   #The p-values used should not be treated too literally. The removal of less significant predictors 
        #tends to increase the significance of the remaining predictors. This effect leads to 
        #amplify the statistical significance of the variables that stay in the model.
   #The procedures are not directly linked to final objectives of prediction or explanation and so 
        #may not really help solve the problem of interest.
   #Stepwise variable selection tends to pick models that are smaller than desirable for prediction purposes. 

data(state)
head(state.x77)
statedata <- data.frame(state.x77,row.names=state.abb,check.names=T)
g <- lm(Life.Exp ~ ., data=statedata)
summary(g)

#illustrate the backward method
g <- update(g, . ~ . - Area)
summary(g)
g <- update(g, . ~ . - Illiteracy)
summary(g)
g <- update(g, . ~ . - Income)
summary(g)
g <- update(g, . ~ . - Population)
summary(g)
#The final removal of the Population variable is a close call. We may want to consider including this
#variable if interpretation is aided. Notice that the R2 for the full model of 0.736 is reduced only slightly to
#0.713 in the final model. Thus the removal of four predictors causes only a minor reduction in fit.

#(3)Criterion-based procedures
#If there are p potential predictors, then there are 2^p possible models.Clever algorithms such as 
#the "branch-and-bound" method can avoid actually fitting all the models - only likely candidates are evaluated. 

#The Akaike Information Criterion (AIC)  AIC=-2log-likelihood) + 2p
#The Bayes Information Criterion (BIC)  BIC=-2log-likelihood) + plogn
#For linear regression models, the -2log-likelihood (known as the deviance) is nlog(RSS/n).
#BIC penalizes larger models more heavily and so will tend to prefer smaller models in comparison to AIC. 
g <- lm(Life.Exp ~ ., data=statedata)
step(g)            #aic(default, k=2)
#step(object, scope, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = NULL, steps = 1000, k = 2, ...) 
step(g,k=log(50))  #bic

#Adjusted R^2.  Recall that R^2 = 1-RSS/TSS. 
   #Adj R^2=1-[RSS/(n-p)]/[TSS/(n-1)]=1-[(n-1)/(n-p)]*(1-R^2)=1-sigma_model_hat^2/sigma_null_hat^2

#redicted Residual Sum of Squares (PRESS): sum_{i}(epsilon_(i)_hat^2), epsilon_(i)_hat are the residuals
#calculated without using case i in the fit. The model with the lowest PRESS criterion is then selected.
#This tends to pick larger models (!!!!!!!which may be desirable if prediction is the objective).

#Mallow's Cp Statistic 
#A good model should predict well so average MSE of prediction might be a good criterion:
   #sum_{i}E[(yi_hat-E[yi])^2]/sigma^2, which can be estimated by the Cp statistic:
        #Cp = RSS_p/sigma_hat^2 + 2p - n, where sigma_hat^2 is from the model with all predictors
                                     #and RSS_p(MSE_p^2) indicates the RSS from a model with p parameters.
#For the full model Cp = p exactly.
#If a p predictor model fits then E(RSS_p)=(n-p)*sigma^2 and then E(Cp) ~= p. A model with a bad
#fit will have Cp much bigger than p. 
#It is usual to plot Cp against p.We desire models with small p and Cp around or less than p.
if(!require("leaps")){install.packages("leaps")}
library(leaps)
x <- model.matrix(g)[,-1]  #x is statedata with response Life.Exp and intercept removed.
    #model.matrix creates a design (or model) matrix.  returns the model's predictors
dim(x)
y <- statedata$Life.Exp
g <- leaps(x,y)
#leaps(x=, y=, wt=rep(1, NROW(x)), int=TRUE, method=c("Cp", "adjr2", "r2"), nbest=10, names=NULL, df=NROW(x), strictly.compatible=TRUE)
#Since the algorithm returns a best model of each size, the results do not depend on a penalty model for model size: it doesn't make any difference whether you want to use AIC, BIC, CIC, DIC, ...
Cpplot(g)   #Makes a Cp plot
#The models are denoted by indices for the predictors.
#The competition is between the "456" model i.e.the Frost, HS graduation and Murder model 
                    #and the model also including Population(1456).
#Both models are on or below the Cp = p line, indicating good fits. The choice is between the smaller 
#model and the larger model which fits a little better. Some even larger models fit in the sense that 
#they are on or below the Cp = p line but we would not opt for these in the presence of smaller models 
#that fit.

#Now let's see which model the adjusted R^2 criterion selects.
adjr <- leaps(x,y,method="adjr2")
maxadjr(adjr,8)   #1456 has the largest adj R^2. The best three predictor model is in eighth place 
  #but the intervening models are not attractive since they use more predictors than the best model.

#Variable selection methods are sensitive to outliers and influential points. 
#check for high leverage points:
h <- hat(x)     #get hat values H[i,i] (leverages)
names(h) <- state.abb
rev(sort(h))
#Which state sticks out? Let's try excluding it (Alaska is the second state in the data).
l <- leaps(x[-2,],y[-2],method="adjr2")
maxadjr(l)
#We see that area now makes it into the model. 

#Transforming the predictors can also have an effect: Take a look at the variables:
par(mfrow=c(3,3))
for(i in 1:8) boxplot(state.x77[,i],main=dimnames(state.x77)[[2]][i])
#we see that Population, Illiteracy and Area are skewed - we try transforming them:
nx <- cbind(log(x[,1]),x[,2],log(x[,3]),x[,4:6],log(x[,7]))
par(mfrow=c(3,3))
apply(nx,2,boxplot)
a <- leaps(nx,y,method="adjr2")
maxadjr(a)  #This changes the "best" model again to log(Population), Frost, HS graduation and Murder



#11. Statistical Strategy and Model Uncertainty
# Thus far we have learnt various tactics
# 1. Diagnostics: Checking of assumptions: constant variance, linearity, normality, outliers, influential
#    points, serial correlation and collinearity.
# 2. Transformation: Transforming the response - Box-Cox, transforming the predictors - tests and
#    polynomial regression.
# 3. Variable selection: Stepwise and criterion based methods
# Diagnostics(EDA) -> Transformation -> Diagnostics(residual plot) -> Variable Selection -> Diagnostics 

#12. Chicago Insurance Redlining - a complete example
library(faraway)
data(eco)
plot(income ~ usborn, data=eco, xlab="Proportion US born",ylab="Mean Annual Income")  
    #the first element must be a formula when the plot function uses the "formula" input -- "data = eco", 
        #or it couldn't identify where income and usborn come from and omit the parameter data = eco.
g <- lm(income ~ usborn, eco)
summary(g)
plot(income ~ usborn, data=eco, xlab="Proportion US born",
     ylab="Mean Annual Income",xlim=c(0,1),ylim=c(15000,70000),xaxs="i")
abline(g$coef)
#We see that there is a clear statistical significant relationship between per capita annual income 
#and the proportion who are US born.  If we substitute, usborn=1 into the regression equation, we
#get 68642-46019=$22,623, while if we put usborn=0, we get $68,642. This suggests that on average,
#naturalized citizens are three times wealthier than US born citizens. In truth, information US Bureau 
#of the Census indicates that US born citizens have an average income just slightly larger than 
#naturalized citizens. What went wrong with our analysis?

#When data is collected at the group level, we may observe a correlation between two variables. The
#ecological fallacy is concluding that the same correlation holds at the individual level. The ecological 
#inference from the aggregate data to the individuals requires an assumption of constancy. Explicitly, 
#the assumption would be that the incomes of the native-born do not depend on the proportion of
#native born within the state (and similarly for naturalized citizens).This assumption is unreasonable 
#for this data because immigrants are naturally attracted to wealthier states.

#This is also relevent to the analysis of the Chicago insurance data since we have only aggregate data.
#We must keep in mind that the results for the aggregated data may not hold true at the individual level.

#We will focus on the relationship between race and the response although similar analyses might be
#done for the income variable.
data(chicago)
head(chicago)
# race: racial composition in percent minority
# fire: fires per 100 housing units
# theft: theft per 1000 population
# age: percent of housing units built before 1939
# volact: new homeowner policies plus renewals minus cancellations and non renewals per 100 housing units
# involact: new FAIR plan policies and renewals per 100 housing units
# income: median family income
ch <- data.frame(chicago[,1:4],involact=chicago[,6],income=chicago[,7]/1000)
ch
summary(ch) #a wide range in the race, some skewness in the theft and income, involact has a large number of zeroes
#Now make some graphical summaries:
par(mfrow=c(2,3))
for(i in 1:6) hist(ch[,i],main=names(ch)[i])
for(i in 1:6) boxplot(ch[,i],main=names(ch)[i])
pairs(ch)
#An examination of the data using xgobi would also be worthwhile.
# library(xgobi)
# library(rggobi)
# library(RGtk2)
# xgobi(ch)
# rggobi(ch)

#Now look at the relationship between involact and race, seems linear in pairs:
summary(lm(involact ~ race,data=ch))
#We can clearly see that homeowners in zip codes with high % minority are being denied insurance at
#higher rate than other zip codes. That is not in doubt. However, can the insurance companies claim 
#that the discrepancy is due to greater risks in some zip-codes? For example, we see that % minority 
#is correlated with the fire rate from the plots.The insurance companies could say that they were 
#denying insurance in neighborhoods where they had sustained large fire-related losses and any 
#discriminatory effect was a byproduct of (presumably) legitimate business practice. 

#suppose that if the effect of adjusting for income differences was to remove the race effect? This 
#would pose an interesting but non-statistical question. I have chosen to include the income variable 
#here just to see what happens.
#!!!!I use log(income) partly because of skewness in this variable, but also because income is better 
#considered on a multiplicative rather than additive scale. In other words, $1,000 is worth a lot more 
#to a poor person than a millionaire.
g <- lm(involact ~ race + fire + theft + age + log(income), data = ch)
g

#!!!Before we start making any conclusions, we should check the model assumptions.
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals",main="Residual-Fitted plot")
abline(h=0)
qqnorm(g$res)

gi <- lm.influence(g)   #calculate leverages
gi$coef   #could only be seen clearly in plot
for(i in 1:5) qqnorml(gi$coef[,i+1],main=names(ch)[-5][i])  #qqnorml() labels the points in a Q-Q plot 
     #case 6 and 24 stick out.
par(mfrow=c(1,1))
#Check out the jacknife residuals:
rstudent(g)
qqnorml(rstudent(g),main="Jacknife Residuals")
qt(0.05/(2*47),47-6-1)     #-3.529468.  Nothing too extreme as all abs(rstudent(g)) are smaller than 3.529468
#now look at the Cook statistics using the halfnorm() function that I wrote:
halfnorm(cooks.distance(g),main="Cook-Statistics")  #used to distinguish influential points
#plot on the leverages and Cook statistics for the savings data:

#Cases 6 and 24 stick out again. Let's take a look at these two cases:
ch[c(6,24),]
g <- lm(involact ~ race + fire + theft + age + log(income),ch,subset=(1:47)[-c(6,24)])
#theft and age are no longer significant at the 5% level.

#We now address the question of transformations - because the response has some zero values and for interpretational reasons we will not try to
#transform it.We try fitting a polynomial model with quadratic terms in each of the predictors
g2 <- lm(involact ~ race + poly(fire,2) + poly(theft,2) + poly(age,2) + poly(log(income),2), ch, subset=(1:47)[-c(6,24)])
anova(g,g2)  #Seems that we can do without the quadratic terms
#we could also draw the partial residual plots,which would also reveals no need to transform. 

#We now move on to variable selection. we are mostly interested in the dependency of involact on the 
#race variable, so let's check the collinearity of race with the other variables, which may cause beta_hat
#to vary substantially.
#Variable selection methods are sensitive to outliers and influential points. so I need to form the x and y explicitly:
g <- lm(involact ~ race + fire + theft + age + log(income),ch)
y <- ch$involact[cooks.distance(g) < 0.2]
x <- cbind(ch[,1:4],linc=log(ch[,6]))
x <- x[cooks.distance(g) < 0.2,]
library(leaps)
a <- leaps(x,y)
Cpplot(a)   #1234
#The best model seems to be this one:
g <- lm(involact ~ race + fire + theft + age, ch, subset=(1:47)[-c(6,24)])
summary(g) 
#Thus, we have verified that there is a positive relationship between involact and race while controlling for a selection of the other variables.

#How robust is the conclusion? Would other analysts have come to the same conclusion? One alternative model:
galt <- lm(involact ~ race+fire+log(income),ch,subset=(1:47)[-c(6,24)])
summary(galt)
#In this model, we see that race is not statistically significant. The previous model did fit slightly 
#better but it is important that there exists a reasonable model in which race is not significant since 
#although the evidence seems fairly strong in favor of a race effect, it is not entirely conclusive. 

#Interestingly enough, if log(income) is dropped:
galt <- lm(involact ~ race+fire,ch,subset=(1:47)[-c(6,24)])
summary(galt)
#we find race again becomes significant which raises again the question of whether income should be
#adjusted for since it makes all the difference here.

#We now return to the two left-out cases. Observe the difference in the fit when the two are re-included
#on the best model. The quantities may change but the qualitative message is the same. It is better to 
#include all points if possible, especially in a legal case like this where excluding points might lead 
#to criticism and suspicion of the results.
g <- lm(involact ~ race + fire + theft + age, data=ch)
summary(g)
#Adding back in the two points to the race+fire+log(income) model made race significant again.
#So it looks like there is some good evidence that zip codes with high minority populations are being 
#"redlined" - that is improperly denied insurance.While there is evidence that some of the relationship 
#between race and involact can be explained by the fire rate, there is still a component that cannot be 
#attributed to the other variables.

#Another issue that arises in cases of this nature is how much the data should be aggregated. For example,
#I divided the data using a zip code map of Chicago into north and south. 
data(chiczip)
g <- lm(involact ~ race + fire + theft +age, subset=(chiczip == "s"), ch)
summary(g)
g <- lm(involact ~ race + fire + theft +age, subset=(chiczip == "n"), ch)
summary(g)
#By dividing the data into smaller and smaller subsets it is possible to dilute the significance of any predictor.
#On the other hand it is important not to aggregate all data without regard to whether it is reasonable. 



#13.Robust and Resistant Regression
#When the errors are normal, least squares regression is clearly best but when the errors are nonnormal, 
#other methods may be considered.A particular concern is long-tailed error distributions. One approach 
#is to remove the largest residuals as outliers and still use least squares but this may not be effective. 
#Furthermore, the outlier test is an accept/reject procedure that is not smooth and may not be 
#statistically efficient for the estimation of beta. 


#(1)Robust regression provides an alternative.
#M-estimates choose beta to minimize sum_{i,n}[pho*(yi-xi'*beta)/sigma]
#possible choices for pho are:  (!!!don't have to know)
   #(a) pho(x) = x^2 is just least squares
   #(b) pho(x) = |x| is called least absolute deviation regression(LAD). This is also called L1 regression.
   #(c) pho(x) = (x^2)/2  if |x|<c;      pho(x) = c|x|-(c^2)/2  otherwise; 
           #is called Huber's method and is a compromise between least squares and LAD regression.
        #c can be an estimate of sigma but not the usual one which is not robust. 
        #Something ~ median |epsilon_i_hat| for example.

#Robust regression is related to weighted least squares. The normal equations tell us that X'(y-X*beta_hat)=0;
#With weights and in non-matrix form this becomes: sum_{i=1~n}[wi*x_ij*(yi - sum_{j=1~p}(x_ij*betaj))]=0
#Now differentiating the M-estimate criterion with respect to betaj and setting to zero we get:
    #sum_{i=1~n}[pho'*( (yi - sum_{j=i~p}(x_ij*betaj))/sigma )*x_ij] = 0
#Now let ui=yi-sum_{j=i~p}(x_ij*betaj) to get:
    #sum_{i=1~n}(pho(ui)'/ui) * x_ij * (yi - sum_{j=i~p}(x_ij*betaj)) = 0
#so we can make the identification of:  w(u) = pho(u)'/u
#and we find for our choices of pho above: 
        #1. LS: w(u) is constant.
        #2. LAD: w(u) = 1/|u| - note the asymptote at 0 - this makes a weighting approach difficult.
        #3. Huber: w(u) = 1 if |u|<c;  w(u) = c/|u|  otherwise

#Because the weights depend on the residuals, an iteratively reweighted least squares approach to fitting must be used. 
#We can sometimes get standard errors by var_hat(beta_hat) = sigma_hat^2*(X'*W*X)^(-1) (use a robust estimate of sigma^2 also).

#We demonstrate the methods on the Chicago insurance data. 
#Using least squares first.
data(chicago)
g_ls <- lm(involact ~ race + fire + theft + age + log(income),chicago)
summary(g_ls)
#Robust, default Huber
library(MASS)
gr <- rlm( involact ~ race + fire + theft + age + log(income), chicago)
#The R2 and F-statistics are not given because they cannot be calculated (at least not in the same way).
#Perhaps some group of observations were not being fit well and the robust regression excluded these points.


#(2) Least Trimmed Squares(LTS) - an example of a resistant regression method.
#Resistant methods are good at dealing with data where we expect there to be a certain number of 
#"bad" observations that we want to have no weight in the analysis.

#Here one minimizes sum_{i=1~q}(epsilon_(i)_hat^2) where q is some number less than n and (i) 
#indicates sorting. This method has a high breakdown point because it can tolerate a large number of 
#outliers depending on how q is chosen(small q (high breakdown point)). 
#(the breakdown point of an estimator is the proportion of incorrect observations (e.g. arbitrarily 
#large observations) an estimator can handle before giving an incorrect (e.g., arbitrarily large) result.)

if(!require(robustbase)){install.packages("robustbase")}
library(robustbase)
#remember in chicago dataset, the case 6 and case 24 are outliers. But we have to protect them becuaes
#it's a regulation research. 
g <- ltsreg(involact ~ race + fire + theft + age + log(income),chicago)
g$coef
#  (Intercept) race       fire      theft      age    log(income)
#-1.6950187 0.0037348 0.0549117 -0.0095883 0.0018549  0.1700325
g <- ltsreg(involact ~ race + fire + theft + age + log(income),chicago)
g$coef
#(Intercept)  race       fire      theft      age       log(income)
#2.2237795   0.0050697 0.0423565  -0.0084868  0.0008755  -0.2398183

#The default choice of q is [n/2]+[(p+1)/2] where [x] indicates the largest integer less than or equal
#to x. I repeated the command twice and you will notice that the results are somewhat different. This is
#because the default genetic algorithm used to compute the coefficients is non-deterministic. 
#An exhaustive search method can be used:
g <- ltsreg(involact ~ race + fire + theft + age + log(income),chicago,nsamp="exact")  #several minutes taken
#lqs(formula, data, ..., method = c("lts", "lqs", "lms", "S", "model.frame"), 
#    subset, na.action, model = TRUE, x.ret = FALSE, y.ret = FALSE, contrasts = NULL)
   #control arguments:
     #psamp:the size of each sample. Defaults to p.
     #nsamp:the number of samples or "best" (the default) or "exact" or "sample". If "sample" the number 
        # chosen is min(5*p, 3000), taken from Rousseeuw and Hubert (1997). If "best" exhaustive 
        # enumeration is done up to 5000 samples; if "exact" exhaustive enumeration will be attempted 
        # however many samples are needed.
     #adjust:should the intercept be optimized for each sample? Defaults to TRUE.
g$coef
# (Intercept)          race          fire         theft           age   log(income) 
# -1.1209359068  0.0057514708  0.0485984830 -0.0085098547  0.0007615914  0.1125154707 
g_ls$coef
# (Intercept)         race         fire        theft          age  log(income) 
# -3.573975548  0.009502223  0.039856040 -0.010294505  0.008335600  0.345761521
#The most notable difference from LS for the purposes of this data is the decrease in the race coefficient
#- if the same standard error applied then it would verge on insignificance.However, we don't have the 
#standard errors for the LTS regression coefficients.

#We now use a general method for inference which is especially useful when such theory is lacking - the Bootstrap.
#introduction of bootstrapping:
#Compare Simulation with Bootstrap. In both cases, we consider X fixed.
#Simulation:
        #repeatedly generate artificial data from the true model/ known distribution, compute the estimate 
        #each, time and gather the results to study the distribution. (we need to know the true model/distribution)
        #For the regression case, it is easiest to start with a sample from the error distribution since 
        #these are assumed to be independent and identically distributed:
#                 1. Generate epsilon from the known error distribution.
#                 2. Form y = X*beta + epsilon from the known beta.
#                 3. Compute beta_hat.
#         We repeat these three steps many times. We can estimate the sampling distribution of beta_hat 
#         using the empirical distribution of the generated beta_hat, which we can estimate as accurately 
#         as we please by simply running the simulation for long enough.This technique is useful for a 
#         theoretical investigation of the properties of a proposed new estimator.
#However, simulation is of no value for the actual data since we don't know the true error distribution 
#and we don't know the true beta.So let's see Bootstrap.
    #e.g.: 1/y = x1 + 0.57*x1^2 + 4*x1*x2 + 2.1exp(x4) + epsilon
         # x1~U(0,1), x2~N(0,1), 1/x3 ~ U(0,1), x4~N(1,1), x5~U(1,3), and epsilon ~ N(0,1)

#Bootstrap:
        #emulates the simulation procedure above except instead of sampling from the true model, it 
        #samples from the observed data itself.
#         1. Generate epsilon* by sampling with replacement from epsilon1_hat,...,epsilonn_hat.
#         2. Form y* = X*beta_hat + epsilon* 
#         3. Compute beta_hat from (X,y*)
#This number of bootstrap samples can be as small as 50 if all we want is an estimate of the variance 
#of our estimates but needs to be larger if confidence intervals are wanted.

#To implement Bootstrap, we need to be able to take a sample of residuals with replacement. 
#a random sample (with replacement) of RTS residuals is:
g$res[sample(47,rep=T)]

# We now execute the bootstrap - first we make a matrix to save the results in and then repeat the 
#bootstrap process 1000 times:
x <- model.matrix(~ race+fire+theft+age+log(income),chicago)[,-1]  #because model.matrix only extract the RHS of a formula, so we don't have to specify the response
bcoef <- matrix(0,1000,6)
for(i in 1:1000){
        newy <- g$fit + g$res[sample(47,rep=T)]
        brg <- ltsreg(x,newy,nsamp="best")
        bcoef[i,] <- brg$coef
}
#It is not convenient to use the nsamp="exact" since that would require 1000 times the 1 minutes it
#takes to make original estimate. so I compromised and used the second best option of nsamp="best".
#This likely means that our bootstrap estimates of variability will be somewhat on the high side.

#To test the null hypothesis that H0: beta_race = 0 against the alternative H1: beta_race > 0 we 
#may figure what fraction of the bootstrap sampled beta_race were less than zero.
length(bcoef[bcoef[,2]<0,2])/1000   #0.022, So our p-value is 2.2% and we reject the null at the 5% level.
quantile(bcoef[,2],c(0.025,0.975))  #95% confidence interval for this parameter: 0.0005161027~0.0122475569
#We can get a better picture of the distribution by looking at the density and marking the confidence interval:
plot(density(bcoef[,2]),xlab="Coefficient of Race",main="")
abline(v=quantile(bcoef[,2],c(0.025,0.975))) 
#We see that the distribution is approximately normal with perhaps so longish tails.
#The conclusion here would be that the race variable is significant but the effect is less than that 
#estimated by least squares(0.009502223).

#Robust estimators provide protection against long-tailed errors but they can't overcome problems with
#the choice of model and its variance structure. This is unfortunate because these problems are more
#serious than non-normal error.

#Robust methods can be used in addition to LS as a confirmatory method. You have cause to worry if
#the two estimates are far apart.



#14.Missing Data
#Finding the missing values is the best option but this is not always possible.
#(1)Several fix-up methods to use when data are missing for noninformative reasons:
#1. Delete the case with missing observations. This is OK if this only causes the loss of a relatively 
#small number of cases. 

#2. Fill-in or impute the missing values(#impute.knn(dataMatrix2)$data). Use the rest of the data to 
#predict the missing values. Simply replacing the missing value of a predictor with the average value 
#of that predictor is one easy method. Using regression on the other predictors is another possibility. 
#Some additional uncertainty is caused by the imputation which needs to be allowed for.

#3. Missing observation correlation. Consider just (xi, yi) pairs with some observations missing. The
#means and SDs of x and y can be used in the estimate even when a member of a pair is missing. An
#analogous method is available for regression problems.

#4. Maximum likelihood methods can be used assuming the multivariate normality of the data. The EM
#algorithm is often used here. We will not explain the details but the idea is essentially to treat 
#missing values as nuisance parameters.

#Suppose some of the values in the Chicago Insurance dataset were missing. I randomly declared some
#the observations missing in this modified dataset. 
data(chmiss)
chmiss   #20 missing observations denoted by NA here
#What happens if we try to fit the model?
g <- lm(involact ~ .,chmiss)
summary(g) #You can see there are now only 21 degrees of freedom - almost half the data is lost. 

#We can fill in the missing values by their variable means as in:
cmeans <- apply(chmiss,2,mean,na.rm=T)
cmeans
mchm <- chmiss
for(i in c(1,2,3,4,6)) mchm[is.na(chmiss[,i]),i] <- cmeans[i]
g <- lm(involact ~ ., data=mchm)
summary(g)
# The regression coefficients are now all closer to zero. The situation is analogous to the error in 
# variables case. The bias introduced by the fill-in method can be substantial and may not be compensated 
# by the attendant reduction in variance.

#We can also use regression methods (calld conditional-mean imputation) to predict the missing values of 
#the covariates. Let's try to fill-in the missing race values:
gr <- lm(race ~ fire+theft+age+income,chmiss)
chmiss[is.na(chmiss$race),]
predict(gr,chmiss[is.na(chmiss$race),])
#predict.lm(object, newdata, se.fit = FALSE, scale = NULL, df = Inf, interval = c("none", "confidence", "prediction"),
#level = 0.95, type = c("response", "terms"), terms = NULL, na.action = na.pass, pred.var = res.var/weights, weights = 1, ...)

#Race value couldn't be negative. So obviously we would need to put more work into
#the regression models used to fill-in the missing values. One trick that can be applied when the 
#response is bounded between 0 and 1 is the logit transformation -- y->log(y/(1-y)).

logit <- function(x) log(x/(1-x))
ilogit <- function(x) exp(x)/(1+exp(x))
#We now fit the model with a logit-transformed response and then back-transform the predicted values 
#remembering to convert our percentages to proportions and vice versa at the appropriate times:
gr <- lm(logit(race/100) ~ fire+theft+age+income,chmiss)
ilogit(predict(gr,chmiss[is.na(chmiss$race),]))*100
#    60646      60651      60616      60617 
#0.4190909 14.7320193 84.2653995 21.3121261 
#We can see how our predicted values compare to the actual values:
data(chicago)
chicago$race[is.na(chmiss$race)] 
# 1.0 13.4 62.3 36.4
#So our first two predictions are good but the other two are somewhat wide of the mark.

#Like the mean fill-in method, regression fill-in(conditional-mean imputation) will also introduce a 
#bias towards zero in the coefficients while tending to reduce the variance also. The success of the 
#regression method depends somewhat on the collinearity of the predictors - the filled-in values will be more accurate the more 
#collinear the predictors are.

#From book <Applied Regression Analysis and Generalized Linear Models>: A problem with conditional-mean 
#imputation is that the imputed observations tend to be less variable than real data because they lack
#residual variation. This problem could be addressed by adding a randomly sampled residual to each 
#fill-in value. Another problem is that we have failed to account for uncertainty in the estimation of 
#the regression coefficients used to obtain the imputed values. -- this problem leads naturally to 
#Bayesian Multiple imputation of missing values.


#For situations where there is a substantial proportion of missing data, I recommend that you investigate
#more sophisticated methods, likely using the EM algorithm. Multiple imputation is another possibility. 
#The fill-in methods described above will be fine when only a few cases need to be filled but will become 
#less reliable as the proportion of missing cases increases.










