setwd("D:\\study\\DataScience")

#EDA is used to get a sense of what's happening and what are the kinds of plots that you want to make
#Plotting and analytic graphics.

#1.Before beginning
# - Principal Analytical Graphics - Rules from Edward Tuffey
#(1) show comparisons - always ask "compared to what?"
                     #- Evidence for a hypothesis is always relative to another competing hypothesis
        #so always have a control set.
#(2) show causality, mechanism, explanation, systematic structure
        #think about what is the causal work
#(3) show multivariate data(more than 2 variables) (the real world is multivariate)
        #need to "escape flatland"
#(4) Integration of Evidence
        #Completely integrate words, numbers, images, diagrams
        #Don't let the tool drive the analysis
#(5) Describe and document the evidence with appropriate labels, scales, sources, etc.
        #a data graphic should tell a complete story that is credible
#(6) Content is king

# - why do we use graphs in data analysis?
#(1) To understand data properties
#(2) To find pattern in data
#(3) To suggest modeling strategies
#(4) To "debug" analyses
#(5) To communicate results

#2. a case on characteristics of exploratory graphs
pollution<-read.csv("./EDAdata/avgpm25.csv",colClasses=c("numeric","character","factor","numeric","numeric"))
head(pollution)

#question: we want to see counties exceed the national ambient air quality standard

#####one dimention summaries of data:
#Methods: six-number summary, Boxplots, Histograms, Density Plot, Barplot
str(pollution)
summary(pollution$pm25)
boxplot(pollution$pm25,col="blue",range=2.0)  #default range = 1.5
?boxplot  
abline(h=12)   #h: the y-value(s) for horizontal line(s).
        #abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,coef = NULL, untf = FALSE, ...)
?abline
#histogram
#Histograms can be a poor method for determining the shape of a distribution because it is so 
#strongly affected by the number of bins used.
hist(pollution$pm25,col="green",breaks=100)
?hist
rug(pollution$pm25)  #plot all of the points in your dataset along the underneath the histgram
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col="magenta",lwd=4) #Unlike boxplot, histogram doesn't have a median
                                        #so we always put in a medium bar into the plot.
#Barplot is for categorical data
barplot(table(pollution$region),col="wheat",main="number of Counties in Each Region")
#Density Plot is just plot a line on the barplot
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=12, col="red")
# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
#Kernel Density Plots
#Kernal density plots are usually a much more effective way to view the distribution of a variable.
d <- density(mtcars$mpg) # returns the density data 
plot(d) # plots the results
# Filled Density Plot
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")
#Comparing Groups VIA Kernal Density
#The sm.density.compare( ) function in the sm package allows you to superimpose the kernal 
#density plots of two or more groups.
# Compare MPG distributions for cars with # 4,6, or 8 cylinders
if(!require("sm")){install.packages("sm")}
library(sm)
# create value labels 
cyl.f <- factor(mtcars$cyl, levels= c(4,6,8),labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 
# plot densities 
sm.density.compare(mtcars$mpg, mtcars$cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")
# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)  
?locator  #Reads the position of the graphics cursor when the (first) mouse button is pressed.

#####two dimensions summary
#Methods: Multiple/overlayed 1-D plots(Lattice/ggplot2), Scatterplots, Smooth scatterplot
#Modifications: Overlayed/multiple 2-D plots(coplots), use color/size/shape to add dimensions
               #Spinning plots(move data around in three dimensions), Actual 3-D plots(not that useful)
#Multiple Boxplots
boxplot(pm25~region, data=pollution, col='red') # we could find that all the extreme states are in the west region
#Multiple histograms
par(mfrow=c(2,1),mar=c(4,4,2,1)) #mar: A numerical vector of the form c(bottom, left, top, right). The default is c(5, 4, 4, 2) + 0.1.
hist(subset(pollution,region=="west")$pm25,col="green")
#using subset is very convenient
hist(subset(pollution,region=="east")$pm25,col="green")
#scatterplots:
par(mfrow=c(1,1))
with(pollution,plot(latitude,pm25))  #pm2.5 v.s. north-south trend
#plot(pollution$latitude,pollution$pm25)  #the only difference from above is the axes labels
abline(h=12,lwd=2,lty=2)
#using color
palette()  #order of color use
palette(rainbow(6))
with(pollution,plot(latitude,pm25,col=region)) #use black and red to designate different regions
                                     #red circles are eastern counties, yellow circles are western counties
                                     #how to change color?
palette("default")
#Multiple Scatterplots
par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region="west"),plot(latitude,pm25,main="west"))
with(subset(pollution,region="east"),plot(latitude,pm25,main="east"))
par(mfrow=c(1,1))

#3. Three core plotting systems in R
#(1)The Base Plotting System: start with blank canvas and build up from there
#first plot(x,y,...) function (or similar), then use annotation functions(text, line, points, axis) to add/modify
#advantages: convenient
#drawback: cannot go back once plot has started(to adjust margin)
          #difficult to "translate" to others once a new plot has been created
with(cars,plot(speed,dist,col=dist))
#(2)The Lattice System
library(lattice)
#idea is quite different from base plotting system
#plots are created with a single function call(xyplot,bwplot,etc.)
#Most useful for conditioning types of plots(Coplot): looking at how y changes with x across levels of z
#good for putting many many plots on a screen
#construct an entire plot all at once, so you have to specify a lot of information in the call function.
#drawbacks: sometimes awkward to specify an entire plot in a single function/call
          #annotation in plot is not especially intuitive, cannot "add" to the plot once it is created
          #use of panel functions and subscripts difficult to wield and requires intense preparation
library(data.table)
state<-data.table(state.x77,region=state.region)
xyplot(Life.Exp~Income|region,data)

















