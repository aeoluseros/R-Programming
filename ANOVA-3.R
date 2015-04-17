setwd("D:\\study\\DataScience")
library(faraway)

#15. Analysis of Covariance
#Analysis of Covariance refers to regression problems where there is a mixture of quantitative and 
#qualitative predictors.

#Suppose we are interested in the effect of a medication on cholesterol level - we might have two groups
#- one of which receives the medication and the other which does not.However, we could not treat this as a
#simple two sample problem if we knew that the two groups differed with respect to age and this would affect
#the cholesterol level. Analysis of covariance is a method for adjusting the groups for the age difference 
#and then seeing the effect of the medication.

#Three kinds of model could be constructed and compared: 
   #(a) the original model with normal variables
   #(a) the model incorporating dummy varialbes
   #(b) the model also including the interaction between dummy variables and normal variabes

#(1)A two-level example
#The data for this example consist of x= nave height and y = total length in feet for English medieval
#cathedrals. Some are in the Romanesque (r) style and others are in the Gothic (g) style. Some cathedrals
#have parts in both styles and are listed twice. We wish to investigate how the length is related to height 
#for the two styles. 
data(cathedral)
cathedral
str(cathedral)
lapply(split(cathedral,cathedral$style),summary)   #split divides the data in the vector x into the groups defined by f. 
plot(cathedral$x,cathedral$y,type="n",xlab="Nave height",ylab="Length")
text(cathedral$x,cathedral$y,as.character(cathedral$s))

g <- lm(y ~ x+style+x*style, data=cathedral)  # y ~ x:style is equivalent.
summary(g)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  241.833    336.471   0.719    0.480
# x              3.138      4.506   0.696    0.494
# styleg      -204.722    347.207  -0.590    0.562
# x:styleg       1.669      4.641   0.360    0.723

#Because style is non-numeric, R automatically treats it as a qualitative variables and sets up a coding -
#but which coding? 
model.matrix(g) #So we can see that the coding is 1 and 0. #Gothic cathedrals are treated as
                #the reference level because "g" comes before "r" in the alphabet. 

#x*style is not significant, We see that the model can be simplified to
g <- lm(y ~ x+style, cathedral)
summary(g)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  124.690     82.922   1.504   0.1469    
# x              4.712      1.058   4.452   0.0002 ***
# styleg       -80.393     32.306  -2.488   0.0209 * 

#Put the two parallel regression on the plot:
abline(44.30,4.71)     #124.690 - 80.393 = 44.30->intercept; 4.71->slope
abline(124.69,4.71,lty=2)   #124.69 -> intercept ; 4.71->slope

#We can change the reference level to "r"
cathedral$style <- relevel(cathedral$style,ref="r")
g <- lm(y ~ x+style, cathedral)
summary(g)

#(2)Coding qualitative predictors - For a k-level predictor, k-1 dummy variables
#Consider a 4 level factor that will be coded using 3 dummy variables.
#Treatment coding(default)
#         Dummy coding
#           1 2 3
#         1 0 0 0
# levels  2 1 0 0
#         3 0 1 0
#         4 0 0 1

#Helmert Coding,This coding is not so nice for interpretation. It is the default choice in S-PLUS.
#         Dummy coding
#             1  2  3
#         1  -1 -1 -1
# levels  2   1 -1 -1
#         3   0  2 -1
#         4   0  0  3

###If there are equal numbers of observations in each level (a balanced design) then the dummy variables 
#will be orthogonal to the each other and the intercept.

#(3)A Three-level example
#e.g..:The genetic determination of differences in intelligence: A study of monozygotic twins reared apart
#The data consist of IQ scores for identical twins, one raised by foster parents, the other by the natural 
#parents. We also know the social class of natural parents (high, middle or low). 
data(twins)
twins
plot(twins$B,twins$F,type="n",xlab="Biological IQ",ylab="Foster IQ")
text(twins$B,twins$F,substring(as.character(twins$S),1,1))
#We are interested in predicting the IQ of the twin with foster parents from the IQ of the twin with the
#natural parents and the social class of natural parents. 
#The most general model we'll consider is the seperate lines model:
g <- lm(Foster ~ Biological*Social, twins)
summary(g)
model.matrix(g)
#Now see if the model can be simplified to the parallel lines model:
gr <- lm(Foster ~ Biological+Social, twins)  
summary(gr)
anova(gr,g)    #Yes g can be simplied to gr

#sequential testing -- it includes anova(gr,g)
anova(g)

#We see that a further reduction to a single line model is possible:
gr <- lm(Foster ~ Biological, twins)        
abline(gr$coef)
summary(gr)
#The icing on the cake would be a further simplification of this model to the line y=x (the IQ's are equal).
#The model has no parameters at all so it has RSS =sum_{i}(yi-xi)^2 and degrees of freedom equal to the 
#sample size. 
sum(gr$res^2)   #1493.5
sum((twins$F-twins$B)^2)   #1557
((1557-1493.5)/2)/(1493.5/25)   #0.53147
1-pf(0.53147,2,25)   #0.5942341, So the null is not rejected.



#16.ANOVA
#Predictors are now all categorical/ qualitative.
#The name ANOVA stands for Analysis of Variance is used because the original thinking was to try to 
#partition the overall variance in the response to that due to each of the factors and the error.

#Predictors are now typically called factors which have some number of levels.
#The parameters are now often called effects.
#fixed effects: factors whose levels chosen by the investigators: (a)each level is an interest, (b)
     #there is no intention of extrapolating to other levels, and (c)the same levels could be used again.
#random effects: the levels of the independent variable are not specifically chosen, but instead drawn
     #randomly from a larger population of possible levels: (a)you do wish to make inferences about the 
     #larger population of levels, (b)if repeated, you would have different levels next time.

#We shall first consider only models where the parameters are considered fixed but unknown - called 
#fixed-effects models but random-effects models are also used where parameters are taken to be random variables.

#(1) One-Way Anova
#Given a factor alpha occurring at i = 1,..,I levels, with j = 1,..,Ji observations per level. We use the 
#model: y_ij = miu + alpha_i + epsilon_ij   i = 1,...,I   j = 1,..,Ji
#As it stands not all the parameters are identifiable and some restriction is necessary:
#   1. Set miu = 0 and use I different dummy variables
#   2. Set alpha1 = 0 - this corresponds to treatment contrasts (treatment coding)
#   3. Set sum_{i}(Ji*alphai) = 0.  Then: miu_hat = y.._bar;   alphai_hat = yi._bar_hat - y.._bar_hat
                                   #where .indicates which index or indices the mean is taken over.
       #sum_{i}(Ji*alphai) --> sum of "#of each level's obs. times alpha".
#This last method is the most commonly recommended for manual calculation in older textbooks although
#it is harder to represent within in the y = X*beta + epsilon framework. 

#A side-by-side boxplot is often the most useful plot. Look for equality of variance, transformations, 
#outliers(influence is not relevant here since leverages won't differ unless the design is very unbalanced).

##Estimation and testing

#The effects can be estimated using direct formulae as above or by using the least squares approach (the
#outcome is the same). The first test of interest is whether there is a difference in the levels of the 
#factor. H0: alpha_i = 0  for all i.   Ha: at least one alpha_i is non-zero
#Use F-test. If we reject the null, we must investigate which levels differ.

#example, a set of 24 blood coagulation times. 24 animals were randomly assigned to four different diets 
#and the samples were taken in a random order.
data(coagulation)
coagulation
plot(coag ~ diet, data=coagulation)
# We are hoping not to see
# 1. Outliers - these will be apparent as separated points on the boxplots. The default is to extend the
#    whiskers of the boxplot no more than one and half times the interquartiles range from the quartiles.
#    Any points further away than this are plotted separately.
# 2. Skewness - this will be apparent from an asymmetrical form for the boxes.
# 3. Unequal variance - this will be apparent from clearly unequal box sizes. Some care is required
#    because often there is very little data be used in the construction of the boxplots and so even when 
#    the variances truly are equal in the groups, we can expect a great deal of variability.
#In this case, there are no obvious problems. For group C, there are only 4 distinct observations and one
#is somewhat separated which accounts for the slightly odd looking plot.
g <- lm(coag ~ diet, coagulation)        #parameter restriction two
summary(g)
#Group A is the reference level and has a mean of 61(intercept), groups B, C and D are 5, 7 and 
#0(coefficient) seconds larger on average.
par(mfrow=c(2,2))
plot(g)
par(mfrow=c(1,1))
#Studentized residuals(Scale-location) are sometimes preferred in residual plots as they have been 
#standardized to have equal variance. 

#Examine the design matrix to understand the coding:
model.matrix(g)
#We can fit the model without an intercept term as in
gi <- lm(coag ~ diet -1, coagulation)      #parameter restriction one
summary(gi)
#We can directly read the level means but the tests are not useful since they involve comparisons with zero.
#Note the miscalculation of R^2. Denominator in the definition of R2 has a null model with an intercept 
#in mind when the sum of squares is calculated. 

##Diagnostics
#Remember to plot the residuals/fitted values and do the QQ plot. Influential points and transforming the
#predictors are not an issue although it is reasonable to consider transforming the response if the 
#situation demands it.
qqnorm(g$res) #Because the data are integers and the fitted values turn out to integers also, some 
              #discreteness is obvious in the Q-Q plot. Of course, discrete data can't be normally 
              #distributed. However, here it is approximately normal and so we can go ahead with the 
              #inference without any qualms.
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals",main="Residual-Fitted plot")
#The discreteness in the residuals and fitted values shows up in the residual-fitted plot because we can 
#see fewer points than the sample size. This is because of overplotting of the point symbols. Sometimes 
#you have to tune the amount of noise.
plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals",main="Jittered plot")

##Multiple Comparisons
#After detecting some difference in the levels of the factor, interest centers on which levels or 
#combinations of levels are different.

#It is important to ascertain whether the comparison made were decided on before or after examining the
#data.

#If the comparisons were decided on prior to examining the data, there are three cases:
# 1. Just one comparison - use the standard t-based confidence intervals that we have used before.
# 2. Few comparisons - use the Bonferroni adjustment for the t. If there are m comparisons, use alpha/m 
#    for the critical value.
# 3. Many comparisons - Bonferroni becomes increasingly conservative as m increases. At some point
#    it is better to use the Tukey or Scheffe or related methods.

#it is best to consider all comparisons as post-data.

#If the comparisons were decided on after examining the data, you must adjust the CI to allow for the
#possibility of all comparisons of the type to be made. There are two important cases:
# 1. Pairwise comparisons only. Use the Tukey method.
# 2. All contrasts i.e. linear combinations. Use the Scheffe method.

#We consider pairwise comparisons first. A simple C.I. for alpha_i-alpha_j is:
   #alphai_hat - alphaj_hat + c(1,-1)*t_{n-I,alpha/2}* sigma_hat*sqrt(1/Ji + 1/Jj); 
                  #sigma_hat = sqrt(within_group MS) --> Residual standard error

#A test for alphai-alphaj amounts to seeing whether zero lies in this interval or not. 
        # True error rates for multiple comparisons:
        #           I                 2  3     4     5     6
        # Nominal Type I error        5% 5%    5%    5%    5%
        # Actual overall Type I error 5% 12.2% 20.3% 28.6% 36.6%
#We see that the true type I error can get quite high.Using the t-based CI for multiple comparisons is
#called least significant differences or LSD but this one is a bad trip.
#When comparisons are only made after the overall F-test shows a difference, it's called Fisher's LSD - 
#this one isn't quite so bad but the type I error will still be too big.

#Simulated e.g.:Suppose we have a factor of 6 levels with 4 observations per level:
x <- factor(rep(LETTERS[1:6],rep(4,6)))
x
#suppose the response has no relationship to the factor (i.e. the null hypothesis holds):
g <- lm(rnorm(24) ~ x)
gs <- summary(g)
g$coef
#The t-statistic for testing whether level A = level B is (alphaA_hat-alphaB_hat)/se(alphaA_hat-alphaB_hat))
#where !!!!  se(alphaA_hat-alphaB_hat))=sigma_hat * sqrt(1/4+1/4) = sigma_hat/sqrt(2)
g$coef[2]/(gs$sig/sqrt(2))      #beta/se(beta)=1.267872    #gs$sig = 0.8376312 (Residual standard error)
#This would (in absolute value) need exceed this t-critical value for significance at the 5% level:
qt(0.975,24-6)     #2.1009
#Out of all the possible pairwise comparisons, we may compute the maximum t-statistic as
range(c(0,g$coef[-1]))
rg <- range(c(0,g$coef[-1]))
(rg[2]-rg[1])*sqrt(2)/gs$sig   #1.587527 which just fails to meet significance. 

#Now let's repeat the experiment 1000 times.
res <- matrix(0,1000,2)
for(i in 1:1000){
        g <- lm(rnorm(24) ~ x)
        gs <- summary(g)
        res[i,1] <- abs(g$coef[2]*sqrt(2)/gs$sig)   #one-coefficient t-statistic
        rg <- range(c(0,g$coef[-1]))
        res[i,2] <- (rg[2]-rg[1])*sqrt(2)/gs$sig    #maximum t-statistic
}

#Now see how many of the test statistics for comparing level A and level B were significant at the 5% level:
sum(res[,1] > qt(0.975,24-6))/1000   #0.045, Just a shade under the 5% it should be. 
#Now see how many times the maximum difference is significant at the 5% level.
sum(res[,2] > 2.1)/1000   #0.315. So in cases where there is no difference between levels of the factor, 
        #about 1/3 of the time, an investigator will find a statistically significant difference between 
        #some levels of the factor. 
#Clearly there is a big danger that one might conclude there is a difference when none truly exists.
#We need to make the critical value larger so that the null is rejected only 5% of the time. Using the
#simulation results we estimate this value to be
quantile(res[,2],0.95)      #3.1627
#It turns out that this value may be calculated using the "Studentized Range distribution":
    #stRange(alphaA_hat-alphaB_hat)) = qtukey * sqrt(1/Ji + 1/Jj) = qtukey/sqrt(2)
                                    #qtukey(p, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE)
                                    #nmeans: sample size for range (same for each group).
qtukey(0.95,6,18)/sqrt(2)   #3.1780,  which is close to the simulated value.  
sum(res[,2] > 3.1780)/1000     #0.041, therefore smaller than 0.05.


#Now let's take a look at the densities of our simulated t-statistics:
dmax <- density(res[,2],from=0,to=5)
d2 <- density(res[,1],from=0,to=5)
matplot(d2$x,cbind(dmax$y,d2$y),type="l",xlab="Test statistic",ylab="Density")
abline(h=0)
abline(v=2.1,lty=2)   #
abline(v=3.178)       #3.178 is the Studentized Range distribution
#We see that the distribution of the maximum t-statistic has a much heavier tail than the distribution 
#for a prespecified difference -- the maximum in the estimated distribution does not occur at zero because
#boundary error effects in the density estimator.


#Now we return to our real data. We've found that there is a significant difference among the diets but
#which diets can be said to be different and which diets are not distinguishable. 
data(coagulation)
str(coagulation)
g <- lm(coag ~ diet, coagulation)
summary(g)    #Residual standard error: 2.366 on 20 degrees of freedom
#Let's do the calculations for the difference between diet B and diet C which is 2. 
#First we do the LSD calculation:
qt(1-.05/2,20)*2.366*sqrt(1/6+1/6) # = 2.849448
#alphai_hat - alphaj_hat + c(1,-1)*t_{n-I,alpha/2}* sigma_hat*sqrt(1/Ji + 1/Jj); 

#Suppose two comparisons were pre-planned, then critical value is now this, using the Bonferroni correction.
qt(1-.05/4,20)*2.366*sqrt(1/6+1/6)   #3.3156
c(2-3.3156,2+3.3156)

#Tukey's Honest Significant Difference (HSD) is designed for all pairwise comparisons and depends
#on the studentized range distribution. 
#Let X1,...,Xn be i.i.d. N(miu,sigma^2) and let R = max_i(Xi)-min_i(Xi) be the range.
#Then R/sigma_hat has the studentized range distribution q_{n,v} where v is the number of degrees of 
#freedom used in estimating sigma.
#The Tukey C.I.'s are !!!! alphai_hat - alphaj_hat + c(1,-1)*q{I,n-I}* sigma_hat/sqrt(2)*sqrt(1/Ji + 1/Jj);
#When the sample sizes Ji are very unequal, Tukey's HSD may be too conservative but in general they are
#narrower than those produced by Scheffe's theorem.the Tukey method tends to be more conservative than 
#most because it takes the rather pessimistic approach based on the maximum difference.

#For future reference, a more general form for the Tukey intervals is 
     #(difference) +/- (q_{l,df}/sqrt(2) * (se of difference)), where l is the number of levels of the 
#factor on which we are making multiple comparisons and d f is the degrees of freedom for the error.

#We compute the Tukey HSD bands for the diet data. First we need the critical value from the studentized
#range distribution.
qtukey(0.95,4,20)  #3.958293
#and the interval is:
(qtukey(0.95,4,20) /sqrt(2))*2.366*sqrt(1/6+1/6)   #3.831492
c(2-3.83,2+3.83)
#A convenient way to obtain all the intervals is
TukeyHSD(aov(coag ~ diet, coagulation))
#The Bonferroni based bands would have been just slightly wider:
qt(1-.05/12,20)*2.366*sqrt(1/6+1/6)   #4.0052
  #We divide by 12 here because there are 6 possible pairwise differences and we want a 
  #two-sided confidence interval: 6 * 2 = 12. 
  #With a bit of work we find that only the A-D and B-C differences are not significant.

#The Tukey method assumes the worst by focusing on the largest difference. There are other competitors
#like the Newman-Keuls, Duncan's multiple range and the Waller-Duncan procedure. 
library(agricolae)
model<-lm(coag ~ diet, data=coagulation)
comparison <- SNK.test(model,trt="diet")  #SNK.test doesn't automatically output
comparison$means
comparison2<-SNK.test(model,trt="diet",main="Newman-Keuls",group=FALSE) 

comparison_duncan<-duncan.test(model,trt="diet")
comparison_waller<-waller.test(model,trt="diet")

####Contrasts
#A contrast among the effects alpha1,...,alphaI is a linear combination sum_{i}(ci*alphai) where the ci are known and sum_{i}(ci)=0.
#1. alpha1-alpha2 is a contrast with c1=1, c2=-1 and the other ci=0. All pairwise differences are contrasts.
#2. (alpha1+alpha2)/2-(alpha3+alpha4)/2 with c1=c2=1/2 and c3=c4=-1/2 and the other ci=0. This contrast
   #is directly interpretable.

#Scheffe's theorem for multiple comparisons
#a linear combination PHI=c' * beta is estimable if there exists an a'y such that Ea'y=c'*beta.
#Contrasts are estimable but something like alphai is not because it will depend on the coding used.
#Now PHI_hat=a'*y and var(PHI_hat) = sigma^2*a'*a which can be estimated by sigma_hat^2*a'*a. 

#Suppose we let the dimension of the space of possible c be q and the rank(X)=r. (r=p if we have complete identifiability.)

#Scheffe's theorem
#A 100(1-alpha)% simultaneous confidence interval for all estimable PHI is
        #PHI_hat +/- sqrt(q*F_{q,n-r,alpha})*sqrt(var(PHI_hat)_hat)
#!!!!important, Example: Simultaneous confidence interval for the regression surface:
        #x'*beta_hat +/- sqrt(p*F_{q,n-r,alpha}) * sigma_hat * sqrt(x'(x'x)^(-1)*x)  -- larger than least square interval

#Comparison with least square interval:
#e.g., corrosion data. We compute the usual t-based pointwise bands along with the simultaneous Scheffe bands:
data(corrosion)
gf <- lm(loss ~ Fe, corrosion)
grid <- seq(0,3,by=0.1)
p <- predict(gf,data.frame(Fe=grid),se=T)
fmult <- sqrt(2*qf(0.95,2,11))         #p=2.      #2.822162
tmult <- qt(0.975,11)    #df = n-p = 13-2 = 11.   #2.200985
matplot(grid,cbind(p$fit,p$fit-fmult*p$se,p$fit+fmult*p$se,p$fit-tmult*p$se,p$fit+tmult*p$se),type="l",lty=c(1,2,2,5,5),
                         ylab="loss",xlab="Iron Content")
#Scheffe 95% simultaneous confidence bands are shown as dotted lines surrounding the least
#squares fit. The interior (dashed) lines represent the pointwise confidence intervals.
points(corrosion$Fe,corrosion$loss)


#Example: One-way anova: Consider PHI=sum_{i}(ci*alphai), a contrast which is therefore estimable. 
#We compute var(PHI_hat)_hat = sigma_hat^2 * sum_{i=1~I}(ci^2/Ji) and the SCI for PHI is then
   #sum_{i=1~I}(ci*alphai) +/- sqrt((I-1)*F_{I-1,n-I,alpha}) * sigma_hat * sqrt(sum_{i=1~I}(ci^2/Ji))
#where, I is number of levels.

#Here we apply the Scheffe method for (B+C)/2 -(A+D)/2 so that c2 = c3 = 1/2 and c1 + c4 = -1/2
sqrt(3*qf(0.95,3,20))*2.37*sqrt(1/4+1/6+1/6+1/8)/2
(5+7)/2-(0+0)/2    #no A and D. 5 B and 7 C
c(6-3.04,6+3.04)
#We see that this difference is significantly different from 0 and so we may conclude that there is 
#significant difference between the average of B and C and the average of A and D despite the fact 
#that we may have chosen to test this difference after seeing the data.


####!!!Testing for homogeneity of variance -- Levene's test. 
#Simply compute the absolute values of the residuals and use these as the response in a new one-way anova.
#A significant difference would indicate non constant variance.
#There are other tests but this one is quite insensitive to non-normality and is simple to execute.
#Applying this to the diet data, we find:
summary(lm(abs(g$res) ~ coagulation$diet))
Since the p-value is large, we conclude that there is no evidence of a non-constant variance.


#(2)Two-Way Anova
#Suppose we have two factors, alpha at I levels and beta at J levels. Let n_ij be the number of obs. 
#at level i of alpha and level j of beta and let those observations be y_ij1, y_ij2,... . A complete 
#layout has n_ij >= 1 for all i,j. The most general model that may be considered is
      #y_ijk = miu + alphai + betaj + (alpha*beta)_ij + epsilon_ijk
#A balanced layout requires that n_ij=n. Not all the parameters are identifiable but if the main effects
#alpha and beta are coded appropriately and the interaction effects coding is then derived from the 
#product of these codings, then every contrast of parameters can be estimated.

###One observation per cell
#When n_ij = 1 we would have as many observation as parameters.The parameters could be estimated but 
#no further inference would be possible.
#We can assume (alpha*beta)_ij=0 to free up degrees of freedom to make some tests and CI's.This 
#assumption can be checked graphically using an interaction plot - plot the cell means on the vertical 
#axis and the factor alpha on the horizontal. Join points with same level of beta. The role of alpha 
#and beta can be reversed. Parallel lines on the plot are a sign of a lack of interaction. 

#Tukey's non-additivity test provides another way of investigating an interaction - the model
      #y_ijk = miu + alphai + betaj + phi*(alpha*beta)_ij + epsilon_ijk
#is fit to the data and then we test if phi = 0. This is a nonlinear model and that it makes the 
#assumption that the interaction effect is multiplicative in a form which seems somewhat tenuous.

#Barring any trouble with interaction, because of the balanced design, the factors are orthogonal and 
#their significance can be tested in the usual way.

###More than one observation per cell
#When n_ij =  n i.e. the same number of observations per cell, we have orthogonality.
#With more than one observation per cell we are now free to fit and test the model:
      #y_ijk = miu + alphai + betaj + (alpha*beta)_ij + epsilon_ijk
#The interaction effect may be tested by comparison to the model:
      #y_ijk = miu + alphai + betaj + epsilon_ijk
#and computing the usual F-test.If the interaction effect is found to be significant, do not test the 
#main effects even if they appear not to be significant. The estimation of the main effects and their 
#significance is coding dependent when interactions are included in the model. 

#If the interaction effect is found to be insignificant, then test the main effects but use RSS/df from 
#the full model in the denominator of the F-tests - this has been shown to maintain the type I error 
#better. So the F-statistic used is
     #F = [(RSS_small - RSS_large)/(df_small-df_large)] / (RSS_large/df_large)

###Interpreting the interaction effect
#A comparison of the levels of alpha will depend on the level of beta.
#recommend making plots of response v.s. factors. When the interaction is significant, the main effects 
#cannot be defined in an obvious and universal way.

#When you have a significant inteaction, you can fit a model: y_ijk = miu_ijk + epsilon_ijk
#and then treat the data as a one-way anova with IJ levels. Obviously this makes for more complex 
#comparisons but this is unavoidable when interactions exist. (as said before, If the interaction effect 
#is found to be significant, do not test the main effects even if they appear not to be significant. 
#The estimation of the main effects and their significance is coding dependent when interactions are 
#included in the model.)

#e.g.: 48 rats were allocated to 3 poisons (I,II,III) and 4 treatments (A,B,C,D). The response was 
#survival time in tens of hours.
data(rats)
#plots of response v.s. factors
par(mfrow=c(1,2))
plot(time ~ treat + poison, data=rats)
#Some evidence of skewness can be seen, especially since it appears that variance is in some way 
#related to the mean response. We now check for an interaction using graphical methods:
interaction.plot(rats$treat,rats$poison,rats$time)
interaction.plot(rats$poison,rats$treat,rats$time)
#Do these look parallel? The trouble with interaction plots is that we expect there to be some random
#variation regardless so it is difficult to distinguish true interaction from just noise. Fortunately, 
#in this case, we have replication so we can directly test for an interaction effect.

#Now fit the full model and see the significance of the factors:
g <- lm(time ~ poison*treat, rats)
anova(g)
#We see that the interaction effect is not significant but the main effects are. We check the diagnostics:
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals")
par(mfrow=c(1,1))
#Clearly there's a problem - perhaps transforming the data will help. Try logs first:
g <- lm(log(time) ~ poison*treat,rats)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="Log response")
#Not enough so try the reciprocal:
g <- lm(1/time ~ poison*treat,rats)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="Reciprocal response")
#Looks good - the reciprocal can be interpreted as the rate of dying. Better check the Q-Q plot again:
qqnorm(g$res)
#This looks better than the first Q-Q plot. We now check the ANOVA table again
anova(g)
#find the interaction is #not significant, simplify the model and examine the fit:
g <- lm(1/time ~ poison+treat, rats)
summary(g)
#Let's construct pairwise confidence intervals for the treatment factor using the Tukey method(multiple 
#comparision).Because of the balance, the CI's will all be the same width.We then compute the width of 
#the interval as from summary(g), we can see that Std. Error for treatment factors are 0.2013
#and from summary(rats), we can see it is a balanced factor.
#notice the Tukey method for two way ANOVA is different from that for one-way ANOVA
    ##alphai_hat - alphaj_hat + c(1,-1)*q{I,n-I}* sigma_hat*sqrt(1/Ji + 1/Jj);
qtukey(0.95,4,42)/sqrt(2) * 0.2013   #four kinds of treatments and 42 degree of freedom. sqrt(sum(1/Ji)) = sqrt(1/2)
# So the bands will be difference plus or minus 0.54. All the bands of the B-D do not include 0 so
# we can conclude that all these other pairs of treatments are significantly different.

#Can you distinguish between the poisons?
qtukey(0.95,3,42)/sqrt(2) * 0.1744   #0.4237037, also significantly different


###Replication
#Difference between replication and repeated measurements:
#Repeated measures involves measuring the same cases multiple times. So, if you measured the chips, 
      #then did something to them, then measured them again, etc it would be repeated measures.
#Replication involves running the same study on different subjects but identical conditions. So, 
      #if you did the study on n chips, then did it again on another n chips that would be replication.

#It's important that the observations observed in each cell are genuine replications. If this is not true, 
#then the observations will be correlated and the analysis will need to be adjusted.It is a common 
#scientific practice to repeat measurements and take the average to reduce measurement errors. These 
#repeat measurements are not independent observations. Data where the replicates are correlated can be 
#handled with repeated measures models.

#For example, imagine that the experiment above involved the reaction times of human subjects under two
#factors. We need to distinguish between an experiment that uses 48 subjects and one that uses 12 subjects
#where each subject repeats their assigned factor combination 4 times. In the latter case, the responses 
#will not be independent and a repeated measures style of analysis will be necessary.


#(3)Blocking designs
#When the experimental units are heterogenous in a known way and can be arranged into blocks where
#the intrablock variation is ideally small but the interblock variation is large, a block design can 
#be more efficient than a CRD.

#Examples:
#Suppose we want to compare 4 treatments and have 20 patients available. We might be able divide the
#patients in 5 blocks of 4 patients each where the patients in each block have some relevant similarity. 
#We would then randomly assign the treatments within each block.

#Suppose we want to test 3 crop varieties on 5 fields. Divide each field into 3 strips and randomly assign
#the crop variety.

#Note: We prefer to have block size equal to the number of treatments. If this is not done or possible, an
#incomplete block design must be used.

#We have one factor (or treatment) at t levels and one blocking variable at r levels. The model is
          #y_ij = miu + tau_i + pho_j + epsilon_ij
#The analysis is then very similar to the two-way anova with one observation per cell.We can check for
#interaction and check for a treatment effect. We can also check the block effect but this is only useful 
#for future reference. 

#Blocking is a feature of the experimental units and restricts the randomized assignment of the treatments.
#This means that we cannot regain the degrees of freedom devoted to blocking even if the blocking effect 
#(does blocking bring significant influence)turns out not to be significant. The randomization test-based 
#argument means that we must judge the magnitude of the treatment effect within the context of the 
#restricted randomization that has been used.

#We illustrate with an experiment to compare 4 treatments/process, A,B,C,D for the production of penicillin.
#The raw material, corn steep liquor, is quite variable and can only be made in blends sufficient for 4 
#runs. Thus a randomized complete block design is definitely suggested by the nature of the experimental 
#units. The data is:
data(penicillin)
penicillin
par(mfrow=c(1,2))
plot(yield ~ blend+treat,data=penicillin)
par(mfrow=c(1,1))
#Did you see any problems? Now check for interactions:
interaction.plot(penicillin$treat,penicillin$blend,penicillin$yield)
interaction.plot(penicillin$blend,penicillin$treat,penicillin$yield)
#What do you think? It is hard to tell - interaction plots are only suggestive, not definitive.
g <- lm(yield ~ treat+blend,penicillin)
anova(g)
# Response: yield
#            Df Sum Sq Mean Sq F value  Pr(>F)  
# treat      3     70  23.333  1.2389 0.33866  
# blend      4    264  66.000  3.5044 0.04075 *
# Residuals 12    226  18.833                  

# We see no significant treatment effect but the block effect is, as suspected, significant. 
#The analysis of variance table corresponds to a sequential testing of models, here corresponding to 
#the sequence:
#         y ~ 1
#         y ~ treat
#         y ~ treat + blend
# So here the p-value 0.339 corresponds to a comparison of the first two models in this list, while the 
# p-value of 0.041 corresponds to the test comparing the second two. 
# !!! Note that the denominator in both F-test is the mean square from the full model, here 18.8

#Notice that if we change the order of the terms in the ANOVA, it makes no difference because of the
#orthogonal design:
anova(lm(yield ~ blend+treat,penicillin))

#By way of comparison, see what happens if we omit the first observation in the dataset (unbalanced) - 
#this might happen in practice if this run was lost:
anova(lm(yield ~ blend+treat,penicillin[-1,]))
anova(lm(yield ~ treat+blend,penicillin[-1,]))
#Notice that now the order does matter. If we want to test for a treatment effect, we would prefer the 
#first table since in that version the blocking factor blend is already included when we test the 
#treatment factor. Since the blocking factor is an unalterable feature of the chosen design, this is 
#as it should be.

#Check the diagnostics:
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals")
qqnorm(g$res)

#And that might be the end of the story except for that worrying interaction effect possibility. 
summary(g)
alpha <- c(0,g$coef[2:4])  #alpha is 4 treatment differences(not only coefficient).
alpha
beta <- c(0,g$coef[5:8])   #beta is 5 blend differences.
beta
ab <- rep(alpha,5)*rep(beta,rep(4,5))   #length = 20
  #rep(alpha,5): 0,1,5,2,0,1,5,2,0,1,5,2,0,1,5,2
  #rep(beta,rep(4,5)): is 0,0,0,0,-9,-9,-9,-9,-7,-7,-7,-7,-4,-4,-4,-4,-10,-10,-10,-10  (evey elements is repeated 4 times)
h <- update(g,.~.+ab)
model.matrix(h)
anova(h)
#Because the p-value of the treat times block effect is .76 we accept the null hypothesis of no 
#interaction. Of course, the interaction may be of a non-multiplicative form, but there is little 
#we can do about that.

#Notes:
g_rat <- lm(1/time ~ poison*treat,rats)   #we can do this because rats has 48 records --> every factor combination has replicates
anova(g_rat)
ginter<-lm(yield ~ treat*blend,penicillin)   #we can't use this because penicillin only have 20 records, so this regression doesn't have degree of freedom.
summary(ginter)
anova(ginter)   #so this step couldn't substitue anova(h)  --> create a new variable is a better idea.

####rats####
#48 rats were allocated to 3 poisons (I,II,III) and 4 treatments (A,B,C,D). 
#        A    B    C    D
# I   0.31 0.82 0.43 0.45
#     0.45 1.10 0.45 0.71
#     0.46 0.88 0.63 0.66
#     0.43 0.72 0.76 0.62
# II  0.36 0.92 0.44 0.56
#     0.29 0.61 0.35 1.02
#     0.40 0.49 0.31 0.71
#     0.23 1.24 0.40 0.38
# III 0.22 0.30 0.23 0.30
#     0.21 0.37 0.25 0.36
#     0.18 0.38 0.24 0.31
#     0.23 0.29 0.22 0.33

####penicillin####
#These are the treatments. The raw material, corn steep liquor, is quite variable and can only be made in
#blends sufficient for 4 runs.
#          A  B  C  D
# Blend 1 89 88 97 94
# Blend 2 84 77 92 79
# Blend 3 81 87 87 85
# Blend 4 87 92 89 84
# Blend 5 79 81 80 88
#####


#We execute the Tukey non-additivity test. We can do multiple comparisons for the treatment effects,
#using the Tukey or the Scheffe method as appropriate:
    #Tukey: taui_hat - tauj_hat + [q_{1-alpha,t,(t-1)(r-1)}/sqrt(2)] * sigma_hat*sqrt(1/Ji + 1/Jj)
 #or Scheffe: sum_{i}ci&tau_i +/- sqrt((t-1)*F_{1-alpha,t-1,(t-1)(r-1)}) * sigma_hat * sqrt(sum_{i}*ci^2/r)
          #where r(=5) is the number of levels in one block, t(=4) is levels in a factor(or treatments)


#Now, just for the sake of the example, we compute the Tukey pairwise confidence intervals for the
#treatment effects:
g <- lm(yield ~ treat+blend,penicillin)
summary(g)     #Residual standard error: 4.34
qtukey(0.95,4,12) /sqrt(2)     #2.968901
#The standard errors for the differences are
4.34*sqrt(1/5+1/5)   #sigma_hat = 2.7449  #sigma_hat * sqrt(sum_{i}*ci^2/r)
#The bands are difference plus or minus this:
2.968901 * 2.7449


###Relative advantage of RCBD over CRD
#We can measure precision by considering var(tau_hat) (or equivalently sigma_hat^2). Compare the 
#sigma_hat^2 for designs with the same sample size. We define "relative efficiency" as
           #sigma_CRD_hat^2 / sigma_RCBD_hat^2
#where the quantities can be computed by fitting models with and without the blocking effect. 

#For example,suppose sigma_CRD_hat^2 =(226+264)/(12+4) = 30.6 and sigma_RCBD_hat^2 = 18.8, as it is 
#in the example above, then the relative efficiency is 1.62. 
   #The sigma_CRD_hat^2 numbers come from combining the sums of squares and degrees of freedom for 
   #the residuals and the blend in the first anova table we made for this data.
g <- lm(yield ~ treat+blend,penicillin)
anova(g)
#            Df Sum Sq Mean Sq F value  Pr(>F)  
# treat      3     70  23.333  1.2389 0.33866  
# blend      4    264  66.000  3.5044 0.04075 *
# Residuals 12    226  18.833                  

#An alternative method would be to simply fit the model yield ~ treat and read off sigma_CRD_hat^2.
g_alt <- lm(yield ~ treat,penicillin)
anova(g_alt)
#           Df Sum Sq Mean Sq F value Pr(>F)
# treat      3     70  23.333  0.7619 0.5318
# Residuals 16    490  30.625               

#The efficiency is not guaranteed to be greater than one. Only use blocking where there is some 
#heterogeneity in the experimental units. The decision to block is a matter of judgment prior to the 
#experiment. There is no guarantee that it will increase precision.


#(4)Latin Squares -- when there are two blocking variables --  three-way ANOVA without interaction.
#For example, in a field used for agricultural experiments, the level of moisture may vary across the 
#field in one direction and the fertility in another.

#In an industrial experiment, suppose we wish to compare 4 production methods (the treatment) - A, B, C, 
#and D. We have available 4 machines 1, 2, 3, and 4, and 4 operators, I, II, III, IV. A Latin square 
#design is:
                        #     1 2 3 4
                        # I   A B C D
                        # II  B D A C
                        # III C A D B
                        # IV  D C B A
        #Each treatment is assigned to each block once and only once.
        #The design and assignment of treatments and blocks should be random.
#We use the model:   y_ijk = miu + tau_i + beta_j + gamma_k + epsilon_ijk      i,j,k=1,...,t
     #compared with tw-way ANOVA: #y_ijk = miu + alphai + betaj + (alpha*beta)_ij + epsilon_ijk

#To test for a treatment effect simply fit a model without the treatment effect and compare using the 
#F-test. The Tukey pairwise CI's are
  #taul_hat - taum_hat + [q_{1-alpha,t,(t-1)(t-2)}/sqrt(2)] * sigma_hat*sqrt(1/t)

#Properties:
#The Latin square can be even more efficient than the RCBD provided that the blocking effects are sizable.
#We need to have both block sizes to be equal to the number of treatments. This may be difficult to
#achieve. Latin rectangle designs are possible by adjoining latin squares.

#The Latin square can be used for comparing 3 treatment factors (consider two factors as blockings). 
#Only t^2 runs are required compared to the t^3 required if all combinations were run. (The downside is 
#that you can't estimate the interactions if they exist). This is an example of a fractional factorial.

#The Latin square can be replicated if more runs are available.
#When there are 3 blocking variables, a Graeco-Latin square may be used but these rarely arise in practice.

#Example:
#An engineer wants to compare the qualities of raw materials from four suppliers, A, B, C, D. The raw
#material is used to produce a component whose breaking strength is measured. It takes an operator a whole
#day to make one component and there are 4 operators and 4 days on which the experiment will take place.
data(breaking)
breaking
matrix(breaking$supplier,4,4)
plot(y ~ operator + day + supplier, breaking)
#There appear to be differences in suppliers but not in the two blocking variables. No outlier, 
#skewness or unequal variance is apparent.
g <- lm(y ~ operator + day + supplier, breaking)
anova(g)  #we could see differences in suppliers are significant.

#Does it make a difference if we change the order of fitting? 
anova(lm(y ~ day + supplier + operator, breaking))   #They are the same because of the balanced design. 
#We see that there is clear supplier effect but no evidence of an effect due to day or operator.

plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
qqnorm(g$res,ylab="Residuals")
#both seem fine

summary(g)
#We see that Supplier C looks best followed by D. Is the difference significant though? Which suppliers
#in general are significantly better than others? We need the Tukey pairwise intervals to help determine 
#this. The width of the bands calculated in the usual manner:

qtukey(0.95,4,6)/sqrt(2)*55.7   #192.8173
#The width of the interval is 193 - what can we say about differences between suppliers? We can make a
#handy table of the supplier differences:
scoefs <- c(0,g$coef[8:10])
pairdiff<-outer(scoefs,scoefs,"-")
abs(pairdiff)>192.81730
#We see that the (A,B), (B,D) and (D,C) differences are not significant at the 5% level.

#If maximizing breaking strength is our aim, we would pick supplier C but if supplier D offered a better
#price we might have some cause to consider switching to D. 


#How much more (or less ) efficient is the Latin square compared to other designs? First compare to the
#completely randomized design:
gr <- lm(y ~ supplier,breaking)
(summary(gr)$sig/summary(g)$sig)^2   #efficiency = 0.839094
#We see that the Latin square is 16% less efficient than the CRD. Now compare to the blocked designs:
gr <- lm(y ~ supplier+operator,breaking)
(summary(gr)$sig/summary(g)$sig)^2   #0.9816555
gr <- lm(y ~ supplier+day,breaking)
(summary(gr)$sig/summary(g)$sig)^2   #0.8038031
#We see that the Latin square turned out to be a bad choice of design because there was very little if 
#any difference between the operators and days but we did not know that until after the experiment.



#(5)Balanced Incomplete Block design (BIBD)
#For a complete block design, the block size is equal to the number of treatments. When the block size is
#less than the number of treatments, an incomplete block design must be used. For example, in the 
#penicillin example, suppose 6 production processes (6 blends,horizontal) were to be compared but each 
#batch of material was only sufficient for four runs (four treatments,vertical).BIB's are also useful 
#for competitions where not all contestants can fit in the same race.

#In an incomplete block design, the treatments and blocks are not orthogonal. Some treatment contrasts
#will not be identifiable from certain block contrasts - this is an example of confounding.This means 
#that those treatment contrasts effectively cannot be examined. In a balanced incomplete block design, 
#all the pairwise differences are identifiable and have the same standard error. Pairwise differences 
#are more likely to be interesting than other contrasts.

#We have 4 treatments (t=4) A,B,C,D and the block size, k = 3 and there are b = 4 blocks. Therefore,
#each treatment appears r = 3 times in the design. One possible BIB design is
                                # 1 A B C
                                # 2 A B D
                                # 3 A C D
                                # 4 B C D
#Each pair of treatments appears in the same block lambda=2 times - this feature enables simple pairwise
#comparison. For a BIB design, we require
                # b >= t >= k
                # rt = bk = n
                # lambda*(t-1) = r*(k-1)
#This last relation holds because the number of pairs in a block is k*(k-1)/2 so the total number of 
#pairs must be bk*(k-1)/2. On the other hand the number of treatment pairs is t*(t-1)/2. The ratio of 
#these two quantities must be lambda.

#Since lambda has to be integer, a BIB design is not always possible even when the first two conditions are
#satisfied. For example, consider r=4, t=3,b=6,k=2 then lambda=2 which is OK but if r=4,t=4,b=8,k=2 then 
#lambda=4/3 so no BIB is possible (Something called a partially balanced incomplete block design can 
#then be used).

#The model of BIBD we fit is the same as for the RCBD:
           #y_ij = miu + tau_i + pho_j + epsilon_ij 

#Example, A nutrition specialist studied the effects of six diets, a, b, c, d, e, and f (t=6) on weight 
#gain of domestic rabbits. There were ten litters(b=10) available forming blocks of size three(k=3). 
#Each pair of diets appear in the same block twice (lambda = 2).
data(rabbit)
dim(rabbit)
rabbit
t(matrix(rabbit$treat,nrow=3)) 
plot(gain ~ block + treat, rabbit)   #block 8 is significantly lower. treat e is skewed. treat d and f have small variance
g <- lm(gain ~ block+treat,data=rabbit)
anova(g)
        #            Df Sum Sq Mean Sq F value    Pr(>F)    
        # block      9 730.39  81.154  8.0738 0.0002454 ***
        # treat      5 158.73  31.745  3.1583 0.0381655 *  
        # Residuals 15 150.77  10.052                      
#Changing the order of treatment and block:
anova(lm(gain ~ treat+block,data=rabbit))
        #            Df Sum Sq Mean Sq F value    Pr(>F)    
        # treat      5 293.38  58.676  5.8375 0.0034544 ** 
        # block      9 595.74  66.193  6.5854 0.0007602 ***
        # Residuals 15 150.77  10.052                      
#The first one is appropriate for testing the treatment effect or block effect, because we want to test 
#for a treatment effect after the blocking effect has been allowed for.

#Now check the diagnostics
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals")
qqnorm(g$res)

#Which treatments differ? We need to do pairwise comparisons. Tukey pairwise confidence intervals are
#easily constructed:
#taul_hat - taum_hat + [q_{1-alpha,t,n-b-t+1}/sqrt(2)] * sigma_hat*sqrt(2k/lambda)

#First we figure out the difference between the treatment effects:
tcoefs <- c(0,g$coef[11:15])
outer(tcoefs,tcoefs,"-")
#standard error for the pairwise comparisons:
summary(g)
#standard error for the pairwise comparison is 2.24 -- standard error of the coefficient. 
sqrt((2*3)/(2*6))*3.17   #2.241528  #This can also be obtained as sigma_hat*sqrt(2k/lambda):
qtukey(0.95,6,15)   #6 treatments, 15 degrees of freedom
#So the intervals have width
4.59/sqrt(2)*2.24   #7.2702

abs(outer(tcoefs,tcoefs,"-")) > 7.2702   #Only the e-f difference is significant.

#How much better is this blocked design than the CRD? We compute the relative efficiency:
gr <- lm(gain ~ treat,rabbit)
(summary(gr)$sig/summary(g)$sig)^2



#(6)Factorial experiments
# Suppose we have: Factors -- alpha, beta, gamma;  levels -- l_alpha, l_beta, l_gamma
# A full factorial experiment has at least one run for each combination of the levels. The number of
#combinations is l_alpha * l_beta * l_gamma,..., which could easily be very large.

#Typically, there is no replication due to cost concerns so it is necessary to assume that some higher 
#order interactions are zero in order to free up degrees of freedom for testing the lower order effects. 
#Not many phenomena require a precise combination of several factors so this is not unreasonable.


###Fractional Factorials -- not often used in finance
#Fractional factorials use only a fraction of the number of runs in a full factorial experiment. This is
#done to save the cost of the full experiment or because the experimental material is limited and only a 
#few runs can be made. It is often possible to estimate the lower order effects with just a fraction. 

#Fractional designs are expressed using the notation l^(k - p), where l is the number of levels of each 
#factor investigated, k is the number of factors investigated, and p describes the size of the fraction 
#of the full factorial used. Formally, p is the number of generators, assignments as to which effects or 
#interactions are confounded, i.e., cannot be estimated independently of each other. A design with p 
#such generators is a 1/(l^p) fraction of the full factorial design. For example, a 2^(5 - 2) design is 
#1/4 of a two level, five factor factorial design. 

#Resolution: An important property of a fractional design is its resolution or ability to separate main 
#effects and low-order interactions from one another. 
#Resolutions below III are not useful and resolutions above V are wasteful.
#Resolution IV:
#Estimate main effects unconfounded by two-factor interactions
#Estimate two-factor interaction effects, but these may be confounded with other two-factor interactions
#2^(4 - 1) with defining relation I = ABCD
#Resolution V        
#Estimate main effects unconfounded by three-factor (or less) interactions
#Estimate two-factor interaction effects unconfounded by two-factor interactions
#Estimate three-factor interaction effects, but these may be confounded with other two-factor interactions
#2^(5 - 1) with defining relation I = ABCDE

#Consider an experiment with 7 factors, each at 2 levels.
        #             mean main 2-way 3-way  4  5   6  7
        # no. of pars   1    7    21    35  35  21  7  1
#2-way: choose(7,2)
#3-way: choose(7,3)
#If we are going to assume that higher order interactions are negligible then we don't really need 
#2^7=128 runs to estimate the remaining parameters. We could run only a quarter of that, 32, and still 
#be able to estimate main and 2-way effects.  (Although, in this particular example, it is not possible 
#to estimate all the two-way interactions uniquely. This is because, in the language of experimental 
#design, there is no available resolution V design, only a resolution IV design is possible.)

###A Latin square is another example of a fractional factorial.

#In fractional factorial experiments, we try to estimate many parameters with as little data as possible.
#This means there is often not many degrees of freedom left over. We require that simga^2 be small, 
#otherwise there will be little chance of distinguishing significant effects. Fractional factorials are 
#popular in engineering applications where the experiment and materials can be tightly controlled. In 
#the social sciences and medicine, the experimental materials, often human or animal, are much less 
#homgenous and less controllable so sigma^2 tends to be larger. In such cases, fractional factorials 
#are of no value.

#Fractional factorials are popular in product design because they allow for the screening of a large number
#of factors. Factors identified in a screening experiment can then be more closely investigated.
#Example:
#Speedometer cables can be noisy because of shrinkage in the plastic casing material, so an experiment
#was conducted to find out what caused shrinkage. The engineers started with 15 different factors. 
#Response is percentage shrinkage per specimen. There were two levels of each factor.

#A full factorial would take 2^15 runs, which is highly impractical so a design with only 16 runs was
#used where the particular runs have been chosen specially so as to estimate the the mean and the 15 main
#effects. We assume that there is no interaction effect of any kind. 
#Examine the data. The + indicates the high level of a factor, the - the low level. 
data(speedo)
speedo
g <- lm(y ~ .,speedo)
summary(g)
#Why are there no degrees of freedom? Why do we have so many "NA"'s in the display? Because there
#are as many parameters as cases.
#It's important to understand the coding here, so look at the X-matrix.
model.matrix(g)  #We see that "+" is coded as 0 and "-" is coded as 1 (ASCII alphabet).

#We don't have any degrees of freedom so we can't make the usual F-tests. We need a different method.
#Suppose there were no significant effects and the errors are normally distributed. The estimated effects
#would then just be linear combinations of the errors and hence normal. We now make a normal quantile 
#plot of the main effects with the idea that outliers represent significant effects.

coef <- g$coef[-1]
i <- order(coef)
plot(qnorm(1:15/16),coef[i],type="n",xlab="Normal Quantiles",ylab="Effects")
text(qnorm(1:15/16),coef[i],names(coef)[i])
#Notice that "e" and possibly "g" are extreme.

#A half-normal plot is better for detecting extreme points. 
#This plots the sorted absolute values against PHI((n+i)/(2n+1))^(-1). Thus it compares the absolute 
#values of the data against the upper half of a normal distribution. We don't particularly care if 
#the coefficients are not normally distributed, it's just the extreme cases we want to detect. 
#Because the half-normal folds over the ends of a QQ plot it "doubles" our resolution for the detection 
#of outliers.
coef <- abs(coef)
i <- order(coef)
plot(qnorm(16:30/31),coef[i],type="n",xlab="Half-Normal Quantiles",ylab="Effects")
text(qnorm(16:30/31),coef[i],names(coef)[i])
#We might now conduct another experiment focusing on the effect of "e" and "g".









