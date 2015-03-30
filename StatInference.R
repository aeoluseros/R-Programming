setwd("D:\\study\\DataScience")

#1.basics
#(1)likelihood ratios: P(D|+)/P(D_c|+) = [P(+|D)/P(+|D_c)] * [P(D)/P(D_c)]
#(2)Some uses for the Poisson Distribution: 
        #-Modeling count data, 
        #-Modeling event-time or survival data (sensoring)
        #-Modeling contingency tables
        #-Approximating binomials when n is large and p is small (lambda = n*p)
pbinom(2,size=1000,prob=0.0001)
ppois(2,lambda=1000*0.0001)
#(3)law of large number
n<-1000
means<-cumsum(rnorm(n))/(1:n)  # mean prob = 0.5
plot(1:n,means)
abline(h=0)
#law of large number of coin flip, mean prob = 0.5
means<-cumsum(sample(0:1,n,replace=TRUE))/(1:n)
plot(1:n,means)  
abline(h=0.5)

#(4) central limit theorm -- distribution of average of iid variables (properly normalized) becomes
                         #that of a standard normal as the sample size increases
0.56+c(-1,1)*qnorm(0.975)*sqrt(0.56*0.44/100)
binom.test(56,100)$conf.int
#binom.test(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)
#This guarantees that the confidence level is at least conf.level, but in general does not give the shortest-length confidence intervals.
#conf.int:a confidence interval for the probability of success.
binom.test(c(682, 243), p = 3/4)$conf.int
binom.test(682, 682 + 243, p = 3/4)   # The same.

#Binomial Interval should be use Agresti/Coull interval instead of Wald Interval
#simulation
n<-20
pvals<-seq(0.1,0.9,by=0.05)
nosim<-1000
#wald interval
coverage<-sapply(pvals,function(p){
        phats<-rbinom(nosim,prob=p,size=n)/n  #generate a 1000 of 10 coin flips
        ll<-phats-qnorm(0.975)*sqrt(phats*(1-phats)/n)
        ul<-phats+qnorm(0.975)*sqrt(phats*(1-phats)/n)
        mean(ll<p & ul>p)  #proporation of times that insides the lower and upper bound
})
plot(pvals,coverage,type="l")
abline(h=0.95)
#if the true value of p is 0.95 and I do a confidence interval, the coverage is actually better
        #than 95% a little bit.
#when p value is away from 0.5, 95% confidence interval would give us coverage less than 95%. 
   #it's simply because the CTL isn't as accurate as n is not large enough (n=20)
#Quick fix 1:
n<-2000
coverage_largern<-sapply(pvals,function(p){
        phats<-rbinom(nosim,prob=p,size=n)/n  #generate a 1000 of 10 coin flips
        ll<-phats-qnorm(0.975)*sqrt(phats*(1-phats)/n)
        ul<-phats+qnorm(0.975)*sqrt(phats*(1-phats)/n)
        mean(ll<p & ul>p)  #proporation of times that insides the lower and upper bound
})
plot(pvals,coverage_largern,type="l")
abline(h=0.95)
#Quick fix 2:
#form the inverval with (X+2)/(n+4) (Add two success and failures, Agresti/Coull interval)
n<-20
coverage_AC<-sapply(pvals,function(p){
        phats<-(rbinom(nosim,prob=p,size=n)+2)/(n+4)  
        ll<-phats-qnorm(0.975)*sqrt(phats*(1-phats)/n)
        ul<-phats+qnorm(0.975)*sqrt(phats*(1-phats)/n)
        mean(ll<p & ul>p)  #proporation of times that insides the lower and upper bound
})
plot(pvals,coverage_AC,type="l")
abline(h=0.95)
#so Agresti/Coull interval should be generally used instead of Wald Interval for binomial interval

#Poisson interval
#X~Poisson(lamda*t)
#esimate lambda=X/t (X is the number of failtures)
#Variance estimate: lambda/t.
x<-5
t<-94.32
lambda<-x/t   #lambda is the expected count per unit of time
round(lambda+c(-1,1)*qnorm(0.975)*sqrt(lambda/t),3)
poisson.test(x,T=t)$conf
poisson.test(x,T=t)$p.value

lambdavals<-seq(0.005,0.1,by=0.01)
nosim<-1000
t<-100   #t is total monitoring time, so if t is larger, more events would happen, and inference would be more accurate
coverage<-sapply(lambdavals,function(lambda){
        lhats<-rpois(nosim,lambda=lambda*t)/t
        ll<-lhats-qnorm(0.975)*sqrt(lhats/t)
        ul<-lhats+qnorm(0.975)*sqrt(lhats/t)
        mean(ll<lambda & ul>lambda)
})
plot(lambdavals,coverage,type="l")  #gets really bad for small lambda values
abline(h=0.95)

#if t=1000
t<-10000
coverage<-sapply(lambdavals,function(lambda){
        lhats<-rpois(nosim,lambda=lambda*t)/t
        ll<-lhats-qnorm(0.975)*sqrt(lhats/t)
        ul<-lhats+qnorm(0.975)*sqrt(lhats/t)
        mean(ll<lambda & ul>lambda)
})
plot(lambdavals,coverage,type="l")  #gets really bad for small lambda values
abline(h=0.95)

#2.T-distribution
#(X_bar-miu)/[S/sqrt(n)] follows Gosset's distribution with n-1 degrees of freedom
#interval is X+c(-1,1)*t_(n-1) * Std(d)/sqrt(n) # wider than normal distribution
#the t interval technically assumes that the data are iid normal, though it is robust to this assumption
#Paired Observations are often analyzed using the t interval by taking differences.
   #e.g. when you measure something once and the same unit a few days later for the second measurement
        #you can use t interval to analyze this kind of data by taking differences or differences on the log scale.
#For skewed distribution, consider taking logs or using a different summary like the median.
           #or you can use other precedures, for example, creating bootstrapping confidence intervals
#Nonetheless, it doesn't make a lot of sense to use t intervals.
#For highly discrete data, like binary or poisson data, other intervals are available.

#Paired T confidence interval
#y_bar - x_bar + c(-1,1)*t_{alpha/2,n-1} * Std(d)/sqrt(n)
#data(sleep) shows the increase in hours for 10 patients on two soporific drugs. (labeled as groups)
data(sleep)
head(sleep)
dim(sleep) #20 * 3
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) 
g<-g + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g

g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); 
s <- sd(difference); 
n <- 10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)     #qt(0, n-1)=-Inf or qt(0, n-1)=Inf
        #dt(x, df, ncp, log = FALSE)
        #pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
        #qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
        #rt(n, df, ncp)
   #dt gives the density, pt gives the distribution function, qt gives the quantile function, and rt generates random deviates.

t.test(difference)      #the same as above
#t.test(x, y = NULL, alternative = c("two.sided"(default), "less", "greater"),
       #mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.95, ...)
t.test(g2, g1, paired = TRUE)  #the same as above
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)   #same as above
#t.test(formula, data, subset, na.action, ...)
    #formula: a formula of the form lhs ~ rhs where lhs is a numeric variable giving the data 
              #values and rhs a factor with two levels giving the corresponding groups.
#relevel(x, ref, ...): The levels of a factor are re-ordered so that the level specified by ref is first and the others are moved down.
  #so that change g1-g2 to g2 - g1
#I function: In function formula. There it is used to inhibit the interpretation of operators 
  #such as "+", "-", "*" and "^" as formula operators, so they are used as arithmetical operators.

#The results
rbind(
        mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
        as.vector(t.test(difference)$conf.int),      #class is numeric
        as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
        as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)

#Independent group t confidence intervals (A/B testing, randomized trial/experiment) 

#equal variance -- pooled variance
#Suppose we want to compare the mean blood pressure between two groups in a randomized trial: those 
     #who received the treatment to those who received a placebo
#We cannot use the paired t test because the groups are independent and may have different sample sizes
#Therefore, a (1-alpha)*100% confidence interval for miu_y - miu_z is:
#y_bar - x_bar + c(-1,1)*t_{alpha/2,n_x+n_y-2} * Sample_Std(p) * sqrt(1/n_x + 1/n_y)
    #where (Sample_std(p))^2 = Var(p) = {(n_x-1)*Var(x)+(n_y-1)*Var(y)}/(n_x+n_y-2)  #Pooled Variance
   
    #Because it's a randomization trial, it reasonable to assume the variance is the same in the two groups.
    #then our estimate of the variance should at some level be an average of the variance estimate from group one and the variance 
    #from group two. -- we could pool the variance.

    #if n_x = n_y, then the pooled variance is the simple average of the variance from the x group
    #and the variance from y group.

#remember, this interval is assuming a constant variance across the two groups.
#If there is some doubt, assume a different variance per group. #will be covered later.

#e.g.
#Comparing SBP for 8 oral contraceptive users versus 21 controls
#$\bar X_{OC} = 132.86$ mmHg with $s_{OC} = 15.34$ mmHg
#$\bar X_{C} = 127.44$ mmHg with $s_{C} = 18.23$ mmHg
#Pooled variance estimate
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5
#whenever you're doing an independent group interval, you always mentally think which one is the
   #first part of the subtraction. In this case, oral contraceptive users are the first part.


#Mistakenly treating the sleep data as grouped
n1 <- length(g1)
n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(g1)^2 + (n2-1) * sd(g2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
        md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,  
        t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,  #same as above
        t.test(g2, g1, paired = TRUE)$conf  #different with above
)
#pooled variance independent T confidence interval is larger than Paired T confidence interval
    #if you disregard the pairing, the interval actually contains 0.
#you could look at the plot of sleep data, it seems quite clear why paired T interval doesn't include o.
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g
#if you're comparing variation of the left-hand side to the variation of the RHS, that's a lot 
    #variability in the two groups. However, when you're matching up the subjects and looking at
    #the variability in the difference, there's a lot of that variability is explained by 
    #inter-subject variability. Therefore, the variability should not be so large as to include 0.

#ChickWeight data in R
library(datasets) 
data(ChickWeight)
dim(ChickWeight)  #578 * 4
head(ChickWeight)  #Chick and Diet are factor variables
library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
  #convert the Time variable from long format to a short format
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,gain = time21 - time0)
#mutate: Mutate a data frame by adding new or replacing existing columns.

#Plotting the raw data
g <- ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet, group = Chick))
g <- g + geom_line()  #geom_line(): Connect observations, ordered by x value.
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g
#the first diet has a lot more variation and chicks than the fourth diet. 
#by the average line, it appears that the first diet's weight gain is a little bit slower than the fourth diet.

#Weight gain by diet
g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g  #gain is the end weight minus the baseline weight by each of the diet. We could see that the assumption 
      #of equal variances appear suspect here.
   #The violin plot is similar to box plots, except that they also show the probability density of the 
   #data at different values.--> so the width of each chart could reflect the variance.

#Let's do a t interval
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

#Unequal variances  -- If there is some doubt, assume a different variance per group. 
#Assume x, y observations are iid normal, different means and different variances, then the relevant 
    #statistic does not follow a t distribution, but could be approxmiated by t distribution:
#Y_bar - X_bar + c(-1,1) * t_df * sqrt([(Sample_std(x))^2/n_x _ (Sample_std(y))^2/n_y)])
    #where df = [(Sample_std(x))^2/n_x _ (Sample_std(y))^2/n_y)]^2 / [(Sample_std(x))^2/(n_x-1) + (Sample_std(y))^2/(n_y-1))]  #don't have to remember this
#Comparing SBP for 8 oral contraceptive users versus 21 controls
#$\bar X_{OC} = 132.86$ mmHg with $s_{OC} = 15.34$ mmHg
#$\bar X_{C} = 127.44$ mmHg with $s_{C} = 18.23$ mmHg
#$df=15.04$, $t_{15.04, .975} = 2.13$
#Interval $$ 132.86 - 127.44 \pm 2.13 \left(\frac{15.34^2}{8} + \frac{18.23^2}{21} \right)^{1/2} = [-8.91, 19.75] $$
#t.test(..., var.equal = FALSE) 

#equal variance: 
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5   #-9.521097 20.361097
#unequal variance
#df = 15.04, t_{15.04,0.975} = 2.13
132.86 - 127.44 + c(-1, 1) * 2.13 * sqrt(15.34^2/8 + 18.23^2/21)   #-8.906499 19.746499

#the interval of equal and unequal is not necessarily that one is larger than another one.


#Comparing other kinds of data
#For binomial data, there's lots of ways to compare two groups
        #Relative risk, risk difference, odds ratio.
        #Chi-squared tests, normal approximations, exact tests.
#For count data, there's also Chi-squared tests and exact tests.
    #We'll leave the discussions for comparing groups of data for binary and count data until 
    #covering glms in the regression class.


#3. Hypothsis test
#(X_bar - value_against) / (s/sqrt(n)) follows a t distribution with 15 df under H0. s is standard deviation of the population
#two-sided test:
  #we want the probability of rejecting under the null to e 5%, split equally as 2.5% in the upper tail
     #and 2.5% in the lower tail.
#T test
if(!require("UsingR")){install.packages("UsingR")}
library(UsingR)   #this guy includes almost everything I need in R.
data(father.son)
dim(father.son)  #1078 * 2
t.test(father.son$sheight - father.son$fheight)
#if you fail to reject the one-sided test(p>alpha), you know that you also fail to reject the two sided(p>alpha/2)

#p-value
#binomial example:
#Suppose a friend has $8$ children, $7$ of which are girls and none are twins        	
#If each gender has an independent $50$% probability for each birth, what's the probability of getting 
#7 or more girls out of 8 births?
choose(8, 7) * .5 ^ 8 + choose(8, 8) * .5 ^ 8   #choose is the function of combination
#The binomial distribution with size = n and prob = p has density: p(x) = choose(n, x) * p^x * (1-p)^(n-x)
pbinom(6, size = 8, prob = .5, lower.tail = FALSE)  #same as above
    #lower.tail: logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].

#Poisson example:
#p(x) = lambda^x exp(-lambda)/x!
#Suppose that a hospital has an infection rate of 10 infections per 100 person/days at risk (rate of 
    #0.1) during the last monitoring period.
#Assume that an infection rate of 0.05 is an important benchmark. 
#Given the model, could the observed rate being larger than 0.05 be attributed to chance?
#H0: lambda = 0.05;  Ha: lambda > 0.05
#lambda = 0.05, then the lambda plugged in ppois function should be 0.05*100 = 5

ppois(9, 5, lower.tail = FALSE)  #p-value 0.03182806


#4. Power: the probability of rejecting the null hypothesis when it is false (specificity)
#the probability of a type II error is usually called beta. So powr = 1- beta

#Consider our previous example involving RDI
#H_0: miu = 30 versus H_a: miu > 30
#Then power is Pr((X_bar - 30)/(s/sqrt(n) > t_{1-alpha,n-1}; miu = miu_a)
   #Note that this is a function that depends on the specific value of mu_a!
   #Notice as miu_a approaches 30, the power approaches alpha

#Calculating power for Gaussian data
   #We reject if (X_bar - 30)/(sigma/sqrt(n)) > z_{1-alpha}
   #Equivalently if X_bar > 30 + Z_{1-alpha} * sigma/sqrt(n)
   #Under $H_0 : X_bar ~ N(miu_0, sigma^2 / n)
   #Under $H_a : X_bar ~ N(miu_a, sigma^2 / n)
alpha = 0.05
z = qnorm(1 - alpha)
mu0=30
mua=32
sigma=4
n=c(8,16,32,64,128)
pnorm(mu0 + z * sigma / sqrt(n), mean = mu0, sd = sigma / sqrt(n), lower.tail = FALSE)
pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n), lower.tail = FALSE) #get the upper probability

library(ggplot2)
nseq = c(8, 16, 32, 64, 128)
mua = seq(30, 35, by = 0.1)
z = qnorm(.95)
power = sapply(nseq, function(n) pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n), lower.tail = FALSE))
colnames(power) <- paste("n", nseq, sep = "")
d <- data.frame(mua, power)
library(reshape2)
d2 <- melt(d, id.vars = "mua")
names(d2) <- c("mua", "n", "power")
g <- ggplot(d2, aes(x = mua, y = power, col = n)) + geom_line(size = 2)
g    #power increases as miu_a gets larger -- we are more likely to detect a diference if the difference
         #we want to detect is very big. higher n get us higher power earilier and earilier.

#
library(manipulate)
library(ggplot2)
mu0 = 30
myplot <- function(sigma, mua, n, alpha){
        g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mu0, sd = sigma / sqrt(n)), 
                              size = 2, col = "red")
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mua, sd = sigma / sqrt(n)), 
                              size = 2, col = "blue")
        xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
        g = g + geom_vline(xintercept=xitc, size = 3)
        g
}
#manipulate: The manipulate function accepts a plotting expression and a set of controls (e.g. slider, picker, 
   #checkbox, or button) which are used to dynamically change values within the expression. When a 
   #value is changed using its corresponding control the expression is automatically re-executed and 
   #the plot is redrawn.
#slider: Create a slider control(a small gear on the topleft corner) to allow manipulation of a plot variable along a numeric range.
   #slider(min, max, initial = min, label = NULL, step = NULL, ticks = TRUE)
manipulate(
        myplot(sigma, mua, n, alpha),
        sigma = slider(1, 10, step = 1, initial = 4),
        mua = slider(30, 35, step = 1, initial = 32),
        n = slider(1, 50, step = 1, initial = 16),
        alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)
#power is nothing other than the probability of getting larger than the black line (the area between the black line and blue curve). 
   #The probability that we reject if in fact the blue curve is true.

#In power calcualtion, Unknowns are miu_a, sigma, n, beta,     and Knowns: miu_0, alpha
   #Specify any 3 of the unknowns and you can solve for the remainder
     #for example, for particular power I want, what n (how many trials) should like to have.

#Power goes up as alpha gets larger. (the black line moves left.)
#Power of a one sided test is greater than the power of the associated two sided test
#Power goes up as miu_a gets further away from miu_0
#Power goes up as n goes up

#Power doesn't need miu_a, sigma and n, instead only sqrt(n)*(miu_a-miu_0)/sigma
#The quantity (miu_a-miu_0)/sigma is called the effect size, the difference in the means in standard 
   #deviation units. Being unit free, it has some hope of interpretability across settings.

#t-test power
#The power is P((X_bar - miu_0)/(S/sqrt(n)) > t_{1-\alpha, n-1} ; miu = miu_a)  #just modify z-quantile to t-quantile
#(X_bar - miu_0)/(S/sqrt(n)) doesn't follow a t distribution is the true mean is not miu_0. 
   #the distribution is called non-central t distribution.
?power.t.test
#power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05, power = NULL, type = c("two.sample", 
      #"one.sample", "paired"), alternative = c("two.sided", "one.sided"),strict = FALSE)
   #delta: True difference in means:  this is the miu_a - miu_0.
   #miu_a/, sigma, n, beta/power: Omit one of the arguments and it solves for it

power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

#all (miu_a-miu_0)/sigma are equal.   #X_bar is the miu_a
  #Power doesn't need miu_a, sigma and n, instead only sqrt(n)*(miu_a-miu_0)/sigma
  #The quantity (miu_a-miu_0)/sigma is called the effect size, the difference in the means in standard 
  #deviation units. Being unit free, it has some hope of interpretability across settings.

power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n

#I always use power.t.test as my first attack on a power calculation. One of the main reasons is that
   #power has a lot of knobs and dials that you can turn and it's very easy to get tripped up on thinking
   #that you have better power than you have or thinking that you need a smaller sample size.
   #So, when in doubt, try to make your power calculation as simple as possible. (try to revert the question
   #that you're asking down to a t test or a binomial test.)
#you might want to move onto much more complex power calcualtions, but as a first pass you always want to
   #do the t test power the normal calcualtion power or the basic power calculation in a binomial problem.


#5. Multiple testing
#when you do more than one hypothesis test, you have to do some sort of correction to make sure that
   #you're not fooling yourself -- overuse of hypothesis testing and false positives or discoveries. 
#This lecture is about how to do those corrections. #Two key components: Error Measure, Correction

#Classifications
#http://en.wikipedia.org/wiki/False_positive_rate
   #(beta_hat=0|beta=0) = U
   ###error: (beta_hat!=0|beta=0) = V (the number of false positives (Type I error), also called "false discoveries")
   ###error: (beta_hat=0|beta!=0) = T (the number of false negatives (Type II error))
   #(beta_hat!=0|beta!=0) = S (the number of true positives (also called "true discoveries"))
  
   #set m as the total claim times.
   #times of claiming beta = 0 is m-R
   #times of claiming beta != 0 is R
   #times of beta = 0 is m_0
   #times of beta != 0 is m-m_0

#Types of errors:
   #Type I error or false positive (V) Say that the parameter does not equal zero when it does
   #Type II error or false negative (T) Say that the parameter equals zero when it doesn't
#Error Rates:
   #False positive rate - The rate at which false results (beta = 0) are called significant: E[V/m_0]
      #m_0 is total number of not significant variables; V is the total number of Type I error
      #If P-values are correctly calculated calling all P < alpha significant will control the false 
           #positive rate at level alpha on average (actually no control). (Ths controlling has a problem: Suppose that you 
           #perform 10,000 tests and you call all $P < 0.05$ significant, then The expected number of 
           #false positives is: 10,000 * 0.05 = 500 false positives. -- too many false positives)

      #While the false positive rate is mathematically equal to the type I error rate, it is viewed as a separate term because:
          #The type I error rate is often associated with the a-prior setting of the significance level.
              #As opposed to that, the false positive rate is associated with a post-prior result. the false positive rate is a parameter that is not controlled by the researcher, it cannot be identified with the significance level.
          #false positive rate is a term usually used regarding a medical test or diagnostic device 
              #(i.e. "the false positive rate of a certain diagnostic device is 1%"), while type I error is a term associated with statistical tests.

   #Family wise error rate (FWER) - The probability of at least one false positive Pr(V >= 1)
      #The Bonferroni correction(very conservative) is the oldest multiple testing correction. Basic idea:
          #Suppose you do m tests and you want to control FWER at level alpha so Pr(V>=1) < alpha
          #Calculate P-values normally
          #Set alpha_{FWER} = alpha/m
          #Call all P-values less than alpha_{FWER} significant
      #adjusted p-value approach - a direct calculation in R
          #Suppose P-values are P_1,...,P_m, You could adjust them by taking P_i_{fwer} = max(m * P_i,1) for each P-value.
          #then is if you call all P_i_{fwer} < alpha significant you will control the FWER.

   #False discovery rate (FDR) - The rate at which claims of significance are false E[V/R]
      #R is the times that we're going to claim that beta is not equal to zero. (total # of discovery)
      #the proportion of false when claiming beta != 0;
      #Controlling false discovery rate is the most popular correction when performing lots of tests 
          #say in genomics, imaging, astronomy, or other signal-processing disciplines.
      #Basic idea:
          #Suppose you do m tests and one wants to contrl FDR at level alpha so FDR is controlled (because one wants to garauntee that FDR <= alpha.
          #define V/R = 0 where R = 0
          #Calculate P-values normally, Order the P-values from smallest to largest P_1,...,P_m
          #Call any P_i <= alpha * (i/m) significant
      #same as FWER, we could also use adjusted p-value approach - a direct calculation in R
      #q-value approach:
          #you could also directly estimate q-values rather than fixing a level alpha at which to control the FDR.
          #Calculate P-values normally, Order the P-values from smallest to largest P_1,...,P_m
          #define q_k = p_k * (m/k)
          #Let q_i be the FDR-adjusted value for p_i It's value is the smallest q_k, where k>=i
      #Estimate the q-values for a given set of p-values:  #library(qvalue) - no longer available
          #qvalue(p=NULL, lambda=seq(0,0.90,0.05), pi0.method="smoother", fdr.level=NULL, robust=FALSE, 
                 #gui=FALSE, smooth.df=3, smooth.log.pi0=FALSE)


#Case study I: no true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for (i in 1:1000) {
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]  #four columns: Estimate, Std. Error, t value, Pr(>|t|)
}
# Controls false positive rate
#no control
sum(pValues < 0.05) #even in none of the 1000 simulation cases was there actually a relationship between
                    #x and y, we still get 51 or about 5% of the tests being performed called significant.
#Use controls:
# Controls FWER
sum(p.adjust(pValues, method = "bonferroni") < 0.05)
## [1] 0
# Controls FDR -- method = "BH" (same as fdr)
sum(p.adjust(pValues, method = "BH") < 0.05)
## [1] 0
p.adjust(pValues, method = "fdr") == p.adjust(pValues, method = "BH")  #TRUE

#Case study II: 50% true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for (i in 1:1000) {
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if (i <= 500) {
                y <- rnorm(20)
        } else {
                y <- rnorm(20, mean = 2 * x)
        }
        pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}
trueStatus <- rep(c("zero", "not zero"), each = 500)
table(pValues < 0.05, trueStatus)
# Controls FWER
table(p.adjust(pValues, method = "bonferroni") < 0.05, trueStatus)
# Controls FDR
table(p.adjust(pValues, method = "BH") < 0.05, trueStatus)

#P-values versus adjusted P-values
par(mfrow = c(1, 2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19)
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)
par(mfrow = c(1, 1))

#A basic Bonferroni/BH correction is usually enough
#If there is strong dependence between tests there may be problems, consider method="BY" (BH may perform strangely)


#6. Bootstrap
#The bootstrap is a tremendously useful tool for constructing confidence intervals and calculating standard errors for difficult statistics
#Sample of 50 die rolls
library(ggplot2)
library(gridExtra)
nosim <- 1000

cfunc <- function(x, n) mean(x)
g1 = ggplot(data.frame(y = rep(1/6, 6), x = 1 : 6), aes(y = y, x = x))  
g1 = g1 + geom_bar(stat = "identity", fill = "lightblue", colour = "black") 
dat <- data.frame(x = apply(matrix(sample(1 : 6, nosim * 50, replace = TRUE), nosim), 1, mean))
g2 <- ggplot(dat, aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 
grid.arrange(g1, g2, ncol = 2)  #g1 is population distribution, g2 is the distribution of mean

#imagine if you don't know whether the dice is fair or not.
#What if we only had one sample of size 50?
n = 50
B = 1000
## our data
x = sample(1 : 6, n, replace = TRUE)
## bootstrap resamples
resamples = matrix(sample(x,n * B,replace = TRUE),B, n)  #resampling from the left blue bars.
resampledMeans = apply(resamples, 1, mean)
g1 <- ggplot(as.data.frame(prop.table(table(x))), aes(x = x, y = Freq)) + geom_bar(colour = "black", fill = "lightblue", stat = "identity") 
g2 <- ggplot(data.frame(x = resampledMeans), aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 
grid.arrange(g1, g2, ncol = 2)
#Now I can't evaluate what't the behavior of average of 50 die rolls from this population because I don't
   #know what the population is and I don't know what die to roll from. Bootstrapping would tell me about
   #the distribution of averages even though I only get to observe one real true average frm the real population.

#Consider a data set
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x,n * B,replace = TRUE),B, n)
resampledMedians <- apply(resamples, 1, median)
g = ggplot(data.frame(x = resampledMedians), aes(x = x)) 
g = g + geom_density(size = 2, fill = "red")
#g = g + geom_histogram(alpha = .20, binwidth=.3, colour = "black", fill = "blue", aes(y = ..density..)) 
g = g + geom_vline(xintercept = median(x), size = 2)
g

#Nonparametric bootstrap algorithm 
#Bootstrapped confidence interval for mean or median
   #(1)first simulating complete data sets from the observed data with replacement
   #(2)Calculate the statistic for each simulated data set
   #(3)Use the simulated statistics to either define a confidence interval or take the standard 
      #deviation to calculate a standard error

#i. Sample n observations with replacement from the observed data resulting in one simulated 
    #complete data sets
#ii. Take the median of the simulated data set
#iii. Repeat these two steps B times, resulting in B simulated medians
#iv. These medians are approximately drawn from the sampling distribution of the median of n 
    #observations; therefore we can:
        #Draw a histogram of them
        #Calculate their standard deviation to estimate the standard error of the median
        #Take the 2.5th and 97.5th percentiles as a confidence interval for the median

#example
#n<-50
#x<-rnorm(50)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x,n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))
g = ggplot(data.frame(medians = medians), aes(x = medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g

#JackKnife Resampling -- especially useful for variance and bias estimation
#http://en.wikipedia.org/wiki/Jackknife_resampling
#The jackknife estimator of a parameter is found by systematically leaving out each observation from 
#a dataset and calculating the estimate and then finding the average of these calculations.

#theta_bar_jack = (1/n) * sum_{i=1~n}Xi
#variance estimation: (An estimate of the variance of an estimator can be calculated using the jackknife technique.)
   #Var(theta) = var((sum_{i=1~n}Xi)/n) = (sigma^2/n) = [(n-1)/n] * (sum_{i=1~n}(theta_bar_i - theta_bar_jack)^2)
      #where theta_bar_i is the parameter estimate based on leaving out the ith observation
             #theta_bar_jack is the jackknife estimator based on all of the samples
#Bias estimation and correction:
   #theta_bar_BiasCorrected = N * theta_bar - (N-1) * theta_bar_Jack  
       # This reduces bias by an order of magnitude from O(N^-1) to O(N^-2)


#7. Permutation test -- used for group comparisons
#e.g. comparing sparys B and C
data(InsectSprays)
g = ggplot(InsectSprays, aes(spray, count, fill = spray))
g = g + geom_boxplot()
g


#Permutation tests
#Consider the null hypothesis that the distribution of the observations from each group is the same
   #Then, the group labels are irrelevant
   #Consider a data frome with count and spray
   #Permute the spray (group) labels and Recalculate the statistic: Mean difference in counts, Geometric means, T statistic, etc.
   #Calculate the percentage of simulations where the simulated statistic was more extreme (toward the alternative) than the observed
#Variations on Permutation testing
   #rank sum test
   #Fisher's exact test (hypergeometric prob)
   #also, so-called randomization tests are exactly permutation tests, with a different motivation.
   #For matched data, one can randomize the signs; For ranks, this results is the signed rank test
   #Permutation strategies work for regression as well
#Permutation tests work very well in multivariate settings, because you can calculate sort of maiximum 
   #statistics that control FWER(family-wise error rate)

#Permutation test B v C
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))  #we could use permutation in this way!!!
observedStat
mean(permutations > observedStat)
   #from the result, we could see that for 10000 permutations, we couldn't find a reconfiguration of
       #the group labels that lead to a more extreme value of the test statistic than the observed statistic.
       #(More formally, the p value is very samll close to zero). So we reject the null hypothesis.
g = ggplot(data.frame(permutations = permutations),aes(permutations))
g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 1)
g = g + geom_vline(xintercept = observedStat, size = 2)
g  #you could see that the observed statistic is way far out in the tail of our null distribution.





