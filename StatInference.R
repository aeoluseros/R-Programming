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

#(5) T-distribution
#interval is X+c(-1,1)*t_(n-1) * S/sqrt(n) # wider than normal distribution
#the t interval technically assumes that the data are iid normal, though it is robust to this assumption
#Paired Observations are often analyzed using the t interval by taking differences.
   #e.g. when you measure something once and the same unit a few days later for the second measurement
        #you can use t interval to analyze this kind of data by taking differences or differences on the log scale.
#For skewed distribution, consider taking logs or using a different summary like the median.
           #or you can use other precedures, for example, creating bootstrapping confidence intervals
#Nonetheless, it doesn't make a lot of sense to use t intervals.
#For highly discrete data, like binary or poisson data, other intervals are available.

#Paired T confidence interval
#data(sleep) shows the increase in hours for 10 patients on two soporific drugs. (labeled as groups)
data(sleep)
head(sleep)
dim(sleep) #20 * 3
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) 
g + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)

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
#Suppose we want to compare the mean blood pressure between two groups in a randomized trial; 
#those who received the treatment to those who received a placebo
#We cannot use the paired t test because the groups are independent and may have different sample sizes




