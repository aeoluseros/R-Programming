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
state<-data.frame(state.x77,region=state.region)
xyplot(Life.Exp~Income|region,data=state,layout=c(4,1)) #in data frame, dot could be used to substitute "space"
                                                        #in data table, we couldn't do the same
#(3)The ggplot2 system
#it creates a kind of language or grammar, mixing the ideas from both base and lattice plotting systems
library(ggplot2)
#automatically deals with spacings, text, titles but also allow you to annotate
head(mpg)
qplot(displ,hwy,data=mpg)
str(qplot)



#4.The Basic Plotting System
#First step: initializing a new plot with plot(x,y) or hist(x) or boxplot(x). A graphic device would be launched
#parameters of plot function are documented in ?par
library(datasets)
hist(airquality$Ozone)
#scatterplot
with(airquality,plot(Wind, Ozone))
#Boxplot
airquality<-transform(airquality,MOnth=factor(Month))
boxplot(Ozone~Month,airquality,xlab="Month",ylab="Ozone(ppb)")
#key parameters for plotting function:
        #pch: the plotting symble(default is open circle) (plot character)
        #lty: defualt is solid, can bed dashed, dotted, etc.
        #lwd: integer
        #col: palette() and colors()
        #xlab, ylab
#key parameters for par() function
        #las: the orientation of the axis labels on the plot
        #bg: the background color
        #mar: the margin size
        #oma: the outer margin size(default is 0 for all sides)
        #mfrow: # of plots per row, column. (plots are filled row-wise)
        #mfcol: # of plots per row, column. (plots are filled column-wise)
par('lty')
par("col")
par('pch')  #1 is open circle
par("bg")
par("mar")  #[1] 5.1 4.1 4.1 2.1
par("mfrow")

#Basic plot functions:
        #plot
        #lines
        #points
        #text
        #title
        #mtext: add arbitrary text to the margins (inner or outer) of the plot
        #axis: adding axis ticks/labels
with(airquality,plot(Wind,Ozone,main='Ozone and Wind in NYC'))
with(subset(airquality,Month==5),points(Wind, Ozone, col="blue"))
with(subset(airquality,Month!=5),points(Wind, Ozone, col="red"))
legend("topright",pch=1,col=c("blue","red"),legend=c("May","Other Months"))

#Add a regression line
with(airquality,plot(Wind,Ozone,main='Ozone and Wind in NYC',pch=20))
model<-lm(Ozone~Wind,airquality)
abline(model,lwd=2)

#Multiple base plot
par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(airquality,{
        plot(Wind,Ozone,main="Ozone and Wind")
        plot(Solar.R,Ozone,main="Ozone and Solar Radiation")
        plot(Temp,Ozone, main="Ozone and Temperature")
        mtext("Ozone and Weather in NYC",outer=TRUE)
})
par(mfrow=c(1,1))
#mtext: write text into the margins of a plot

#a demonstration:
x<-rnorm(100)
hist(x)
y<-rnorm(100)
plot(x,y)
z<-rnorm(100)
plot(x,z)
par(mar=c(2,2,2,2)) #I will lost my label in this setting of margin size
plot(x,y)
par(mar=c(5, 4, 4, 2) + 0.1)  #default

plot(x,y,pch=20)  #solid circle
plot(x,y,pch=18)  #solid diamond
plot(x,y,pch=2)  #triangles
plot(x,y,pch=4)  #X
plot(x,y,pch="s")  #s
example(points)  #we could see plot symbols after some example plots
par(mar=c(1,1,1,1))
pchShow(c("o","O","0"), cex = 3)
try(TestChars(sign=-1))
par(mar=c(5, 4, 4, 2) + 0.1)  

plot(x,y,pch=20)  #solid circle
title("Scatterplot")
text(-2,-2,"Label")
legend("topright",legend="Data")
legend("topright",legend="Data",pch=20)  #pch could also be used here
str(legend)
fit<-lm(y~x)
abline(fit)
abline(fit,lwd=3)
abline(fit,lwd=3,col="blue")
abline(fit,lwd=1,col="red")  #cover the above lines

plot(x,y,xlab="Weight",ylab="Height",main="ScatterPlot",pch=20)
legend("topright",legend="Data",pch=20)  #
fit<-lm(y~x)
abline(fit,lwd=3,col="red")                
z<-rpois(100,2)
par(mfrow=c(1,1))
plot(x,y,pch=20)
par("mar")
par(mar=c(2,2,1,1))
plot(x,y,pch=20)

x<-rnorm(100)
y<-x+rnorm(100)
g<-gl(2,50)  #gl(n, k, length = n*k, labels = seq_len(n), ordered = FALSE) n is # of levels, k is # of replicatins, result is of length 50*2=100
?gl
g<-gl(2,50,labels=c("Male","Female"))
str(g)
g<-sample(g,100,replace=TRUE)
plot(x,y)
#now let's try to add each category of data into the canvas
plot(x,y,type="n")  #first give a blank canvas
points(x[g=="Male"],y[g=="Male"],col="blue")
points(x[g=="Female"],y[g=="Female"],col="red",pch=19)

###grDevices package -- contains all the code implementing the various graphics devices,
    #including X11, PDF, PostScript, PNG, etc.
#Graphics Devices is something or some place where you can make a plot appear:
        #a window on you computer(screen device), a PDF file(file device), A PNG/JPEG(file device), A scalable vector graphics(SVG) file(file device)
#when you make a plot in R, it has to be sent to a specific graphics device
        #Most common place is the screen device.On Mac, the screen device is launched by quartz(), on Windows is called Windows(), on Unix/Linux is x11()
?Devices
library(grDevices)
#Two common way to create a plot:
#the most common way to create a plot:
with(faithful,plot(eruptions,waiting))
title(main="Old Faithful Geyser Data")
#Another way to create a plot:
pdf(file="myplot.pdf")  #open PDF device, will not plot on screen
with(faithful,plot(eruptions,waiting))
title(main="Old Faithful Geyser Data")
dev.off()

#Two Categories of file devices: vector and bitmap devices
#Vector Formats - most useful for line-graphics(different from natual scenes like photgraphs): 
        #pdf(used for line-type graphics,reize well, not portable; not efficient if a plot has many objects/points) 
        #svg(XML-based scalable vector graphics; support animation and interactivity, potentially used for web-based plots) 
        #win.metafile(only on Windows), postscript(older format, resize well, windows system doesn't have a postscript viewer)
#Bitmap devices - generally don't resize weill:        
        #PNG(Portable Network Graphics)-a series of pixels, good for line drawings or images with solid colors, use lossless compression. 
                #Good for plotting many many points, doesn't resize well
        #jpeg
        #tiff: Create bitmap files in TIFF format; support lossless compression
        #bmp: a native Windows bitmapped format

#Multiple Open Graphics Devices:
#you could only on one devices at a time, so the graphics devices you plot to is the active device
#use dev.cur() to see the active graphics device. Every open graphics device is assigned an integer 2.
#you change the active graphics device with dev.set(<integer>).

#Copy plots -- not an exact operation, result may not be identical to the original: 
with(faithful,plot(eruptions,waiting,main='Old Faithful Geyser Data'))
dev.copy(png,file="geyserplot.png")
dev.off()
dev.copy2pdf(file="MY.pdf", width = 7, height = 5) #don't have to close device for this one
dev.cur()


#5. lattice plotting system -- xyplot
#contains code for producing Trellis graphs, which are indep of the "base" graphics system
#including functions like xyplot, bwplot, levelplot
#another package of lattice system -- grid
#library(grid) 
#we seldom call functions from the grid package. the lattice package builds on top of grid
#Lattice Functions:
#xyplot: main function for creating scatterplots: xyplot(y~x|f*g,data)  # * means interaction
#bwplot: box and whiskers plot("boxplots)
#histogram
#stripplot
#dotplot: plot dots on "violin strings"
#splom: scatterplot matrix; like 'pairs' in base plotting system
#levelplot, contourplot: for plotting 'image' data
library(lattice)
xyplot(Ozone~Wind,data=airquality)
airquality<-transform(airquality, Month=factor(Month)) #we also need to factorize characters. This won't change the month in airquality.  
xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1))  #5,6,7,8,9 five months
class(airquality$Month)  
class(xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1)))  #trellis
#airquality$Month=factor(airquality$Month)  #next session of R would change it back to integer
#class(airquality$Month)
p<-xyplot(Ozone~Wind|Month,data=airquality,layout=c(5,1))  #could be saved as an object
dev.cur()
dev.off()
print(p)

#lattice have a "panel function' which controls what happens inside each panel of the plot
#each panel's going to represent a subset of the data, which is defined by the conditioning variable
set.seed(10)
x<-rnorm(100)
f<-rep(0:1,each=50)
y<-x+f-f*x+rnorm(100,sd=0.5)
f<-factor(f,labels=c("Group 1","Group 2"))
xyplot(y~x|f,layout=c(2,1))

#custom panel function
#functions in panel functions:
  #panel.abline(a = NULL, b = 0,h = NULL, v = NULL,reg = NULL, coef = NULL,col, col.line, lty, lwd, alpha, type,...,reference = FALSE,identifier = "abline")
  #panel.refline: panel.refline is similar to abline, but uses the "reference.line" settings for the defaults.
  #panel.curve(expr, from, to, n = 101,curve.type = "l",col, lty, lwd, type,...,identifier = "curve")
  #panel.rug:adds a rug(data points) representation of the (marginal) data to the panel, much like rug.
  #panel.average(x, y, fun = mean, horizontal = TRUE,lwd, lty, col, col.line, type,...,identifier = "linejoin")
  #panel.linejoin(x, y, fun = mean, horizontal = TRUE,lwd, lty, col, col.line, type,...,identifier = "linejoin")
  #panel.fill(col, border, ..., identifier = "fill")
  #panel.grid(h=3, v=3, col, col.line, lty, lwd, x, y, ..., identifier = "grid")
  #panel.lmline(x, y, ..., identifier = "lmline")  #panel.lmline(x, y) is equivalent to panel.abline(lm(y ~ x)).
  #panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1),n = 50, col, col.line, lwd, lty, type,..., identifier = "mathdensity")  
  #panel.superpose(x, y = NULL, subscripts, groups,panel.groups = "panel.xyplot",...,col, col.line, col.symbol,pch, cex, fill, font,fontface, fontfamily,lty, lwd, alpha,type = "p", grid = FALSE,distribute.type = FALSE)
       #subscripts: An integer vector of subscripts giving indices of the x and y values in the original data source.
  #panel.stripplot(x, y, jitter.data = FALSE,factor = 0.5, amount = NULL,horizontal = TRUE, groups = NULL,...,identifier = "stripplot")
        #Creates stripplot (one dimensional scatterplot) of x for each level of y (or vice versa, depending on the value of horizontal)

#Much of the power of Trellis Graphics comes from the ability to define customized panel functions.
xyplot(y~x|f,panel=function(x,y,...){  #The actual plotting is done by the function specified by the panel argument.
       panel.xyplot(x,y,...)  #First call the default panel function for "xyplot"
       panel.abline(h=median(y),lty=2)  #add a horizontal line a the median 
})

xyplot(y~x|f,panel=function(x,y,...){ 
        panel.xyplot(x,y,...) 
        panel.lmline(h=median(y),lty=2)
})

#another example
histogram( ~ height | voice.part, data = singer, layout = c(2, 4),
           type = "density", border = "transparent", col.line = "grey60",  #type = c("percent", "count", "density") --> we need to claim "type" here so that panel.mathdensity would be active in the panel function.
                #border: the border of bins in histogram.Either a color for the border, or a logical flag. In the latter case, the border color is black if border is TRUE, and no border is drawn if it is FALSE (the default).
           xlab = "Height (inches)",
           ylab = "Density Histogram\n with Normal Fit",  #\n: Line feed
           panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm,
                                     args = list(mean=mean(x),sd=sd(x)), ...)
                   })
#another example
histogram( ~ height | voice.part, data = singer,
           xlab = "Height (inches)", type = "density",  #type = c("percent", "count", "density")
           panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",  #panel.mathdensity plots a (usually theoretical) probability density function. 
                                     args = list(mean=mean(x),sd=sd(x)))
                                #A vectorized function that produces density values given a numeric vector named x, e.g. dnorm
                   #panel.mathdensity (dmath = dnorm, args = list(mean = 0, sd = 1), n = 50, col, col.line = reference.line$col, lwd = reference.line$lwd, lty = reference.line$lty, type, ..., identifier = "mathdensity") 
           })
#another example
bwplot(yield ~ site, barley, groups = year,
       panel = function(x, y, groups, subscripts, ...) {
               panel.grid(h = -1, v = 0) #For panel.grid, these usually specify 
                        #the number of horizontal and vertical reference lines 
                        #to be added to the plot. Alternatively, they can be 
                        #negative numbers. h=-1 and v=-1 are intended to make 
                        #the grids aligned with the axis labels.                        
               panel.stripplot(x, y, ..., jitter.data = TRUE,  #jitter.data=TRUE: jitter data to avoid overplotting
                               groups = groups, subscripts = subscripts)
               panel.superpose(x, y, ..., panel.groups = panel.average,
                               groups = groups, subscripts = subscripts)
                   #panel.superpose divides up the x (and optionally y) variable(s) by the 
                   #unique values of groups[subscripts], and plots each subset with different 
                   #graphical parameters.
       },
       auto.key = list(points = FALSE, lines = TRUE, columns = 2))  #col attribute is used for text, not for lines
        #key: A list that defines a legend to be drawn on the plot. This list is used as an argument to the draw.key function,
              #which produces a "grob" (grid object) eventually plotted by the print method for "trellis" objects. 

#another example:
bwplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
       panel = "panel.superpose",
       panel.groups = "panel.linejoin",
       xlab = "treatment",
       key = list(lines = Rows(trellis.par.get("superpose.line"),
                               c(1:7, 1)),
                  text = list(lab = as.character(unique(OrchardSprays$rowpos))),
                  columns = 4, title = "Row position"))
        #Functions used to query, display and modify graphical parameters for fine control of Trellis displays. :
                #trellis.par.set(name, value, ..., theme, warn = TRUE, strict = FALSE)
                #trellis.par.get(name = NULL)
                #show.settings(x = NULL)
#another example
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(3,2), ylab=NULL)

stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")



#6. ggplot2
#aesthetic artributes: color, shape, size of geometric objects(points,lines,bars,shapes)
#the plot may also contain statistical transformations of the data.
#stats: statistical transformations like binning, quantiles, smoothing
#scales: what scale an aesthetic map uses (example, male=red, female=blue)
        #Scales are to find how to different variables are coded, in terms of plot (make man red, make female blue).

library(ggplot2)
#the basic: qplot()  - quick plot, hides what goes on underneath, which is okay for most operations
#ggplot() is the core function and very flexible for doing things qplot() cannot do
#####data has to be organized as data frame
#factors should be labeled to be informative

#(1) qplot
#Hello World for GGplot2
str(mpg)
args(qplot)
# function (x, y = NULL, ..., data, facets = NULL, margins = FALSE, 
#           geom = "auto", stat = list(NULL), position = list(NULL), 
#           xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, 
#           xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
#           asp = NA) 
qplot(displ, hwy, data=mpg)  #solid circles as points.
plot(mpg$displ, mpg$hwy)  #the same, but more ugly. Open circles as points
#modifying aesthetics
qplot(displ, hwy, data=mpg,color=drv)  #three kinds of drives, drv are factors #legend is added automatically
class(mpg$drv)  #factor
qplot(displ, hwy, data=mpg,geom=c("point","smooth"),method='lm')  
qplot(displ, hwy, data=mpg,color=drv,geom=c("point","smooth"))  #haha, interesting
qplot(hwy, data=mpg, geom="density")
qplot(hwy, data=mpg, geom="density",color=drv)
qplot(displ, hwy, data=mpg,shape=drv)
qplot(displ, hwy, data=mpg,color=drv)
qplot(displ, hwy, data=mpg,color=drv,geom=c("point","smooth"),method='lm')  #smooth method is "lm"
#you could make an histogram with a qplot function (compared with hist in base plotting system)
qplot(hwy, data=mpg, fill=drv)  #plot would be a histogram once we only specify one variable
#Facets - like panels in lattice  -- split by groups
qplot(displ,hwy,data=mpg,facets=.~drv)  #variable on the righ-hand side of ~(tilde) determines the columns of the panels
qplot(hwy,data=mpg,facets=drv~.,binwidth=2) #variable on the left side of ~(tilde) indicates the rows of this kind of matrix here.
qplot(displ, hwy, data=mpg,color=drv,geom=c("point","smooth"),method='lm',facets=.~drv)

#(2)ggplot
g<-ggplot(mpg,aes(displ,hwy))
summary(g)
print(g) #it will say: No layers in plot. It means R doesn't know how to draw the data yet.
        #it doesn't know if you want points or if you want lines and tiles...
p<-g+geom_point()
print(p)
g+geom_point()  #auto print
g+geom_point()+geom_smooth()
g+geom_point()+geom_smooth(method="lm")
g+geom_point()+facet_grid(.~drv)+geom_smooth(method="lm") #order doesn't matter
        #notice that labels are determined by the facet_grid. So make sure the metadata is then specified appropriately.
?facet_grid
qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )

mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#Are scales shared across all facets (the default, "fixed"), or do they vary across 
#rows ("free_x"), columns ("free_y"), or both rows and columns ("free")
mt + facet_grid(. ~ cyl, scales = "free") 
mt + facet_grid(vs ~ am, scales = "free")
mt + facet_grid(vs ~ am, scales = "free_x")
mt + facet_grid(vs ~ am, scales = "free_y")
mt + facet_grid(vs ~ am, scales = "free", space="free")
mt + facet_grid(vs ~ am, scales = "free", space="free_x")
mt + facet_grid(vs ~ am, scales = "free", space="free_y")


#Annotation
#labels: xlab, ylab, labs, ggtitle
#each of the "geom" functions has options to modify
#for things that only make sense globally, use theme(): e.g.: theme(legend.position="none")
#two standard appearance themes are included:
        #theme_gray(): The default theme(gray background)
        #theme_bw(): More stark/plain
g+geom_point(color="steelblue",size=4,alpha=1/2) #alpha=1/2: use transparent points
str(geom_point)
#function (mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, ...)  
g+geom_point(aes(color=drv),size=4,alpha=1/2) #alpha=1/2: use transparent points
?aes
aes(x = mpg, y = wt) #aes creates a list of unevaluated expressions.
aes(color=drv)
g+geom_point(aes(color=drv),size=3)+labs(title="MPG DRV")+labs(x=expression("displ"[2]),y="hwy")
g+geom_point(aes(color=drv),size=3,alpha=1/2)+geom_smooth(size=4,linetype=3,method="lm",se=FALSE)+labs(title="MPG DRV")+labs(x=expression("displ"[2]),y="hwy")
g+geom_point(aes(color=drv),size=3,alpha=1/2)+geom_smooth(size=2,linetype=4,method="lm",se=TRUE)+labs(title="MPG DRV")+labs(x=expression("displ"[2]),y="hwy")
g+geom_point(aes(color=drv),size=3,alpha=1/2)+theme_bw(base_family="Times")
?theme_bw  #theme_bw(base_size = 12, base_family = "")

#examples of geom_point
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()
# Add aesthetic mappings
p + geom_point(aes(colour = qsec))
p + geom_point(aes(alpha = qsec))
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(shape = factor(cyl)))
p + geom_point(aes(size = qsec))
# Change scales
p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
p + geom_point(aes(size = qsec)) + scale_size_area()
?scale_size_area() #scale the area of points to be proportional to the value.
p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
# Set aesthetics to fixed value
p + geom_point(colour = "red", size = 3)
qplot(wt, mpg, data = mtcars, colour = I("red"), size = I(3))
# Varying alpha is useful for large datasets #no transparency when alpha=1, the smaller of alpha, the more transparent of data points
d + geom_point(alpha = 1)
d <- ggplot(diamonds, aes(carat, price))
d + geom_point(alpha = 1/10)
d + geom_point(alpha = 1/20)
d + geom_point(alpha = 1/100)
# You can create interesting shapes by layering multiple points of
# different sizes
p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = cyl))
p + aes(shape = factor(cyl)) +
        geom_point(aes(colour = factor(cyl)), size = 4) +
        geom_point(colour="grey90", size = 1.5)
p + geom_point(colour="black", size = 4.5) +
        geom_point(colour="pink", size = 4) +
        geom_point(aes(shape = factor(cyl)))
# These extra layers don't usually appear in the legend, but we can
# force their inclusion
p + geom_point(colour="black", size = 4.5, show_guide = TRUE) +
        geom_point(colour="pink", size = 4, show_guide = TRUE) +
        geom_point(aes(shape = factor(cyl)))
# Transparent points:
qplot(mpg, wt, data = mtcars, size = I(5), alpha = I(0.2))
# geom_point warns when missing values have been dropped from the data set
# and not plotted, you can turn this off by setting na.rm = TRUE
mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
qplot(wt, mpg, data = mtcars2)
qplot(wt, mpg, data = mtcars2, na.rm = TRUE)

#A note about Axis Limits - Coordinate
testdata<-data.frame(x=1:100,y=rnorm(100))
testdata[50,2]<-100   #outlier
plot(testdata$x,testdata$y,type='l',ylim=c(-3,3))  #baes plot
g<-ggplot(testdata,aes(x=x,y=y))
g+geom_line()
g+geom_line()+coord_cartesian(ylim=c(-3,3))

#if we want to condition on cts variable, we first need to use cut() function to 
 #make the cts variable categorical
set.seed(6809)
diamonds <- diamonds[sample(nrow(diamonds), 1000), ]
diamonds$cut <- factor(diamonds$cut,
                       levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))
# Repeat first example with new order
p <- ggplot(diamonds, aes(carat, ..density..)) +
        geom_histogram(binwidth = 1)
p + facet_grid(. ~ cut)

mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
mt + facet_grid(. ~ cyl, scales = "free_x", space="free") +
        scale_x_continuous(breaks = seq(10, 36, by = 2))
last_plot() + xlim(10, 15)











