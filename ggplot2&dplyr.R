setwd("D:\\study\\DataScience")

#1. dplyr
library(mgcv)
library(dplyr)
install.packages("nycflights13")
library(nycflights13)
dim(flights)
head(flights)

#(1)dplyr can work with data frames as is, but if you're dealing with large data, it's worthwhile to convert 
#them to a tbl_df: this is a wrapper around a data frame that won't accidentally print a lot of data to 
#the screen.
class(flights)
#flights_dt <- tbl_dt(flights)  #flights are already tbl_df, so we don't have to transform it.

#Dplyr aims to provide a function for each basic verb of data manipulating:        
#       filter() (and slice())
#       arrange()
#       select() (and rename())
#       distinct()
#       mutate() (and transmute())
#       summarise()
#       sample_n() and sample_frac()

#(2)Filter rows with filter()
#The first argument is the name of the data frame, and the second and subsequent are filtering expressions 
    #evaluated in the context of that data frame
filter(flights, month == 1, day == 1)
#This is equivalent to the more verbose:
flights[flights$month == 1 & flights$day == 1, ]
#filter() works similarly to subset() except that you can give it any number of filtering conditions 
#which are joined together with & (not && which is easy to do accidentally!). You can use other boolean 
#operators explicitly:
filter(flights, month == 1 | month == 2)

#(3)To select rows by position, use slice()
slice(flights, 1:10)

#(4)Arrange rows with arrange()
#arrange() works similarly to filter() except that instead of filtering or selecting rows, it reorders them.
#If you provide more than one column name, each additional column will be used to break ties in the values 
   #of preceding columns:
arrange(flights, month, year,day)    #ascending
#Use desc() to order a column in descending order
arrange(flights, desc(arr_delay))
#dplyr::arrange() works the same way as plyr::arrange(). It's a straighforward wrapper around order() that 
   #requires less typing.
#The previous code is equivalent to:
flights[order(flights$year, flights$month, flights$day), ]
flights[order(desc(flights$arr_delay)), ]

#(5)Select columns with select()
#Often you work with large datasets with many columns where only a few are actually of interest to you. 
#select() allows you to rapidly zoom in on a useful subset using operations that usually only work on 
#numeric variable positions:
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
#This function works similarly to the select argument to the base::subset()

#There are a number of helper functions you can use within select(), 
   #like starts_with(), ends_with(), matches() and contains().
#You can rename variables with select() by using named arguments:
select(flights, tail_num = tailnum)  #rename tailnum as tail_num

#But because select() drops all the variables not explicitly mentioned, it's not that useful. 
   #Instead, use rename():
rename(flights, tail_num = tailnum)

#(6)Extract distinct (unique) rows
#A common use of select() is to find out which values a set of variables takes. This is particularly useful 
   #in conjunction with the distinct() verb which only returns the unique values in a table.
distinct(select(flights, tailnum))
distinct(select(flights, origin, dest))
#This is very similar to base::unique() but should be much faster.
unique(select(flights, tailnum))

#(7)Add new columns with mutate() -- that are functions of existing columns. 
flights_mutated<-mutate(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
names(flights_mutated)
#dplyr::mutate() works the same way as plyr::mutate() and similarly to base::transform().
#The key difference between mutate() and transform() is that mutate allows you to refer to columns that 
   #you just created:
mutate(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))
#not run, Error: transform(flights,gain = arr_delay - delay,gain_per_hour = gain / (air_time / 60)) 

#If you only want to keep the new variables, use transmute():
transmute(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))

#(8)Summarise values with summarise()  
#which collapses a data frame to a single row. It's not very useful yet:
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#(9)Randomly sample rows with sample_n() and sample_frac()
#You can use sample_n() and sample_frac() to take a random sample of rows, either a fixed number for 
   #sample_n() or a fixed fraction for sample_frac().
sample_n(flights, 10)
sample_frac(flights, 0.01)
#Use replace = TRUE to perform a bootstrap sample, and optionally weight the sample with the weight argument.
sample_n(flights, 10,replace = TRUE)
# year month day dep_time dep_delay arr_time arr_delay carrier tailnum flight origin dest air_time distance
# 1  2013     7  19     1823        33     2020        28      UA  N16701   1187    EWR  CLE       63      404
# 2  2013    12   1      828        -2     1130       -13      UA  N578UA    653    EWR  LAX      335     2454
# 3  2013    11  29     1409         4     1716         4      UA  N804UA    365    EWR  SAN      349     2425
# 4  2013     6   7       NA        NA       NA        NA      B6  N627JB   1018    JFK  BOS       NA      187
# 5  2013     2  12     1752        -8     1853       -20      US  N956UW   2138    LGA  BOS       42      184
# 6  2013     8  26      638        -7      818       -50      US  N549UW    623    JFK  PHX      262     2153
# 7  2013     6  11     1036        -7     1158        -1      EV  N13956   3817    EWR  MKE      120      725
# 8  2013     7  27      813        -2      959       -29      9E  N920XJ   3538    JFK  MSP      143     1029
# 9  2013     5  14      922        -3     1158       -42      VX  N622VA    407    JFK  LAX      312     2475
# 10 2013     5  30      547        -3      835       -10      UA  N33264   1077    EWR  MIA      149     1085

#Commonalities: You may have noticed that all these functions are very similar:
  #The first argument is a data frame. The subsequent arguments describe what to do with it, and you can 
  #refer to columns in the data frame directly without using $. The result is a new data frame

#At the most basic level, you can only alter a tidy data frame in five useful ways: 
  #you can reorder the rows (arrange()), 
  #pick observations and variables of interest (filter() and select()), 
  #add new variables that are functions of existing variables (mutate()) or 
  #collapse many values to a summary (summarise()).


#The remainder of the language comes from applying the five functions to different types of data, 
   #like to grouped data, as described next.
#(10)Grouped operations
# Above functions are affected by grouping as follows:
# mutate() and filter() are most useful in conjunction with window functions (like rank(), or min(x) == x), 
#    and are described in detail in vignette("window-function").
# grouped select() is the same as ungrouped select(), excepted that retains grouping variables are always 
#    retained.
# grouped arrange() orders first by grouping variables
# sample_n() and sample_frac() sample the specified number/fraction of rows in each group.
# slice() extracts rows within each group.
# summarise() is easy to understand and very useful, and is described in more detail below.

by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
library(ggplot2)
ggplot(delay, aes(dist, delay)) + geom_point(aes(size = count), alpha = 1/2) + geom_smooth() + scale_size_area()

#You use summarise() with aggregate functions, which take a vector of values, and return a single number. 
   #There are many useful functions in base R like min(), max(), mean(), sum(), sd(), median(), and IQR(). 
  #dplyr provides a handful of others:
# n(): number of observations in the current group
# n_distinct(x): count the number of unique values in x.
# first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)], and x[n] but give you 
   #more control of the result if the value isn't present.
#e.g.: find the number of planes and the number of flights that go to each possible destination:
destinations <- group_by(flights, dest)
summarise(destinations, planes = n_distinct(tailnum), flights = n())

#When you group by multiple variables, each summary peels off one level of the grouping. 
   #That makes it easy to progressively roll-up a dataset:
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

#ungroup(x)   #removing existing grouping.

#group_size()
data("hflights", package = "hflights")
group_size(group_by(hflights, Year, Month, DayofMonth))
group_size(group_by(hflights, Dest))


#(11)Chaining
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,arr = mean(arr_delay, na.rm = TRUE),dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
#Or if you don't want to save the intermediate results, you need to wrap the function calls inside each other
filter(
        summarise(
                select(
                        group_by(flights, year, month, day),
                        arr_delay, dep_delay
                ),
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE)
        ),
        arr > 30 | dep > 30
)
#This is difficult to read because the order of the operations is from inside to out, and the arguments 
  #are a long way away from the function. To get around this problem, dplyr provides the %>% operator. 
  #x %>% f(y) turns into f(x, y) so you can use it to rewrite multiple operations so you can read from 
  #left-to-right, top-to-bottom:
flights %>%
        group_by(year, month, day) %>%
        select(arr_delay, dep_delay) %>%
        summarise(
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE)
        ) %>%
        filter(arr > 30 | dep > 30)

library(hflights)
by_dest <- hflights %>% group_by(Dest) %>% filter(n() > 100)
library(mgcv)
by_dest_gam<-by_dest %>% do(smooth = gam(ArrDelay ~ s(DepTime) + Month, data = .))
by_dest_gam[[2]]

#(12) Other data sources
# dplyr allows you to use the same verbs with a remote database. It takes care of generating the SQL for 
# you so that you can avoid the cognitive challenge of constantly swiching between languages. See the databases vignette for more details.
# 
# Compared to DBI and the database connection algorithms:
#    it hides, as much as possible, the fact that you're working with a remote database
#    you don't need to know any sql (although it helps!)
#    it abstracts over the many differences between the different DBI implementations


#(13) Multidimensional arrays / cubes
#tbl_cube() provides an experimental interface to multidimensional arrays or data cubes. 
?tbl_cube
titanic <- as.tbl_cube(Titanic)  #first group by "survived", then by "Titanic", then by "age", then "sex", then "class"
head(as.data.frame(titanic))  
head(Titanic)
dim(Titanic)  #4 2 2 2
dim(titanic)  #32  4   #the four dimension would become four columns
as.data.frame(titanic)  #need to use as.data.frame to print the content

admit <- as.tbl_cube(UCBAdmissions)
as.data.frame(admit)
UCBAdmissions  #just reverse the group order because UCBAdmissions is grouped from the first column to the last

esoph_tube<-as.tbl_cube(esoph, dim_names = 1:3) #firt group by tobgp, then by alcgp, then by agegp
head(as.data.frame(esoph_tube))  
head(esoph)  #group first by agegp, then by alcgp, then by tobgp

#(14)do
by_cyl <- group_by(mtcars, cyl)  #just group, by_cyl doesn't change the row order. but the data underlying has been separated into groups.
class(by_cyl)     #"tbl_df"
do(by_cyl, head(., 2))  #use . to refer to the current group.  #return the first two elemetns in each group, so 6 elements in total
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))  #regression in groups
models  #consists of three groups
summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(coef = coef(.$mod)))    #coef(models$mod[[1]])
models %>% do(data.frame(var = names(coef(.$mod)), coef(summary(.$mod)))) #do two actions at the same time

models <- by_cyl %>% do(mod_linear = lm(mpg ~ disp, data = .), mod_quad = lm(mpg ~ poly(disp, 2), data = .))
models
compare <- models %>% do(aov = anova(.$mod_linear, .$mod_quad))
#compare %>% summarise(p.value = aov$`  Pr(>F)`)
compare[[1]]



#2.dplyr periphery
#(1)all.equal
scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
mtcars_df <- tbl_df(mtcars)
all.equal(mtcars_df, scramble(mtcars_df))
all.equal(mtcars_df, scramble(mtcars_df), ignore_col_order = FALSE)
all.equal(mtcars_df, scramble(mtcars_df), ignore_row_order = FALSE)

x=expand.grid(x1=1:2,y1=1:4) #Create a data frame from all combinations of the supplied vectors or factors
y=expand.grid(x1=1:2,y1=1:4)
all.equal(x,y)
x=expand.grid(x1=1:2,y1=1:4)
y=expand.grid(x1=1:4,y1=1:4)
all.equal(x,y)   #result would be strange because x,y are not tbl_tb/tf
x=tbl_df(expand.grid(x=1:2,y=1:4))
y=tbl_df(expand.grid(x=1:4,y=1:4))
all.equal(x,y)  #"Rows in y but not x: 7, 3, 15, 11, 8, 12, 16, 4"

#(2)explain_sql - for ODBC database
#require("RSQLite") && has_lahman("sqlite")
#batting <- tbl(lahman_sqlite(), "Batting")
#explain_sql(nrow(batting))
#explain_sql(nrow(batting))
## nrow requires two queries the first time because it's the same as dim(x)[1], but the results are cached

#show_sql(head(batting))
#explain_sql(head(batting))

#explain(batting)
#explain(filter(batting, lgID == "NL" & yearID == 2000))
#explain(filter(batting, lgID == "NL" | yearID == 2000))

#(3)failwith
x<-rbinom(10,1,1/2)
f <- function(x) if (x == 1) stop("Shit!") else 1
#stop: stops execution of the current expression and executes an error action.
safef <- failwith("oh yeah", f)
safef(1)
safef(2)
geterrmessage()  # gives the last error message.

#(4)compute: lazy tbls

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#2.ggplot2
#a statistical graphic is a mapping from data to (a)aesthetic attributes(color,shape,size) of 
#(b)geom(points, lines, bars). May also contian (c)statistical transformations(stats) of data 
#and is drawn on a specific coordinate system(coord).(d)Faceting can be used to generate the same 
#plot for different subsets of the dataset.
library(ggplot2)
#(1) qplot
set.seed(1410)
dsmall<-diamonds[sample(nrow(diamonds),100),]
        #qplot(x, y = NULL, ..., data, facets = NULL, margins = FALSE,
        #      geom = "auto", stat = list(NULL), position = list(NULL), 
        #      xlim = c(NA,NA), ylim = c(NA, NA), log = "", main = NULL,
        #      xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA)
#Using the data argument is recommended: it's a good idea to keep related data in a single data frame.
qplot(carat, price, data = diamonds)
#The relationship looks exponential, though, so the first thing we'd like to do is to transform the variables.
qplot(log(carat), log(price), data = diamonds)  #The relationship now looks linear. 

#Arguments can also be combinations of existing variables
#if we are curious about the relationship between the volume of the diamond(approximated by x × y × z) and its weight
qplot(carat, x * y * z, data = diamonds)
#We would expect the density (weight/volume) of diamonds to be constant, and so see a linear relationship between volume and weight.


###The first big difference when using qplot instead of plot:
#With plot, it's your responsibility to convert a categorical variable in your data
   #(e.g., "apples", "bananas", "pears") into something that plot knows how to
   #use (e.g., "red", "yellow", "green"). qplot can do this for you automatically,
qplot(carat, price, data = dsmall, colour = color)  #Mapping point colour to diamond colour
qplot(carat, price, data = dsmall, shape = cut)  #Mapping point shape to cut quality
#Colour, size and shape are all examples of aesthetic attributes. For every aesthetic attribute, there 
#is a function, called a scale, which maps data values to valid values for that aesthetic.
#For example, in the above plots, the colour scale maps J to purple and F to green.

#You can also manually set the aesthetics using I(), e.g., colour = I("red") or size = I(2).

###Rule: For large datasets, semitransparent points are often useful to alleviate some of the overplotting.
qplot(carat, price, data = diamonds, alpha = I(1/10))   #I() is necessary
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))
###Rule: Different types of aesthetic attributes work better with different types of variables.
#colour and shape work well with categorical variables, while size works better with continuous variables.
#The amount of data also makes a difference: if there is a lot of data, like in the plots of diamonds, 
   #it can be hard to distinguish the different groups (so we use dsmall). An alternative solution is 
   #to use faceting.

###Plot geoms
#The following geoms enable you to investigate two-dimensional relationships
#geom = "point" draws points to produce a scatterplot. This is the default when you supply both x and y arguments to qplot().
#geom = "smooth" fits a smoother to the data and displays the smooth and its standard error
#geom = "boxplot" produces a box-and-whisker plot to summarise the distribution of a set of points
#geom = "path" and geom = "line" draw lines between the data points. A line plot is constrained to 
   #produce lines that travel from left to right, while paths can go in any direction.

#For 1d distributions, your choice of geoms is guided by the variable type:
#For continuous variables, geom = "histogram" draws a histogram(default), geom = "freqpoly" a frequency polygon, 
     #and geom = "density" creates a density plot.
#For discrete variables, geom = "bar" makes a bar chart.


###Adding a smoother to a plot
#If you have a scatterplot with many data points, it can be hard to see exactly what trend is shown 
#by the data. In this case you may want to add a smoothed line to the plot.

qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))
#If you want to turn the confidence interval off, use se = FALSE.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),se = FALSE)


###Smoothers -- method argument
#method = "loess", the default for small n, uses a smooth local regression -- for the fit at point x, 
   #the fit is made using points in a neighbourhood of x, weighted by their distance from x.
   #The wiggliness of the line is controlled by the 'span' parameter, 
         #which ranges from 0 (exceedingly wiggly) to 1 (not so wiggly).
#Loess does not work well for large datasets (it's O(n2) in memory), and so an alternative smoothing 
   #algorithm is used when n is greater than 1,000.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 1)
#method = "lm" fits a linear model. The default will fit a straight line to your data, or you can 
   #specify formula = y ~ poly(x, 2) to specify a degree 2 polynomial, or better, load the splines 
   #package and use a natural spline: formula = y ~ ns(x, 2). The second parameter is the degrees
   #of freedom: a higher number will create a wigglier curve.
library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm",formula = y ~ poly(x, 2))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ ns(x,5))

#method = "rlm" works like lm, but uses a robust fitting algorithm so that outliers don't affect the 
#fit as much. It's part of the MASS package, so remember to load that first.
library(MASS)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "rlm")

#method = "gam", formula = y ~ s(x):
#You could also load the mgcv library and use method = "gam", formula = y ~ s(x) to fit a generalised 
#additive model. This is similar to using a spline with lm, but the degree of smoothness is estimated 
#from the data.
#For large data, use the formula y ~ s(x, bs = "cs"). This is used by default when there are more than 1,000 points.
library(mgcv)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),method = "gam", formula = y ~ s(x))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),method = "gam", formula = y ~ s(x, bs = "cs"))


###Boxplots and jittered points
#When a set of data includes a categorical variable and one or more continuous variables, you will 
   #probably be interested to know how the values of the continuous variables vary with the levels of 
   #the categorical variable. Boxplots and jittered points offer two ways to do this. 

#how the distribution of price per carat varies with the colour of the diamond
qplot(color, price / carat, data = dsmall, geom = "jitter",alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",alpha = I(1 / 200))
qplot(color, price / carat, data = diamonds, geom = "boxplot")

#For jittered points, qplot offers the same control over aesthetics as it does for a 
   #normal scatterplot: size, colour and shape. 
qplot(color, price / carat, data = dsmall, geom = "jitter",alpha = I(1 / 5),color=color,shape=color,size=price / carat)
#For boxplots you can control the outline colour, the internal fill colour and the size of the lines.
qplot(color, price / carat, data = diamonds, geom = "boxplot",outline = FALSE,fill = color,outlier.colour = "green")


###Histogram and density plots
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")
#For the density plot, the adjust argument controls the degree of smoothness (high values of adjust produce smoother plots). 
qplot(carat, data = diamonds, geom = "density",adjust = 0.1)
qplot(carat, data = diamonds, geom = "density",adjust = 1)

#for Histogram, binwidth argument or break argument
#For the histogram, the binwidth argument controls the amount of smoothing by setting the bin size.
   #(Break points can also be specified explicitly, using the breaks argument.)
#It is very important to experiment with the level of smoothing. You may find that gross features 
   #of the data show up well at a large bin width, while finer features require a very narrow width.
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1,xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01,xlim = c(0,3)) 
#It is only in the plot with the smallest bin width (right) that we see the striations we noted in 
   #an earlier scatterplot, most at "nice" numbers of carats.

#To compare the distributions of different subgroups, just add an aesthetic mapping
#Mapping a categorical variable to an aesthetic will automatically split up the geom by that variable
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)
# The density plot is more appealing at first because it seems easy to read and compare the various 
# curves. However, it is more difficult to understand exactly what a density plot is showing. In addition, 
# the density plot makes some assumptions that may not be true for our data; i.e., that it is unbounded,
# continuous and smooth.

###Bar charts -- as with barchart in R
# If the data has already been tabulated or if you'd like to tabulate class members in some other
#way, such as by summing up a continuous variable, you can use the weight geom. 
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat") #a bar chart of diamond colour weighted by carat.
     #scale_y_continuous("caret") changes the y-axis's label into "caret".

###Time series with line and path plots
#Line plots join the points from left to right, while path plots join them in the order that they
   #appear in the dataset (a line plot is just a path plot of the data sorted by x value).
#Line plots usually have time on the x-axis, showing how a single variable has changed over time. 
#Path plots show how two variables have simultaneously changed over time, with time encoded in the way.
data(economics)
head(economics)
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

#Below we plot unemployment rate vs. length of unemployment and join the individual observations with a path. 
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
#we apply the colour aesthetic to the line to make it easier to see the direction of time.
qplot(unemploy / pop, uempmed, data = economics, geom = "path", colour = year(date)) + scale_size_area() 
       #When scale_size_area is used, the default behavior is to scale the area of points to be proportional to the value.
#We can see that percent unemployed(x-axis) and length of unemployment(y-axis) are highly correlated, 
   #although in recent  years the length of unemployment has been increasing relative to the unemployment 
   #rate (more tilted).

###Faceting:  
#formula: row var ~ col var. You can specify as many row and column variables as you like, keeping 
   #in mind that using more than two variables will often produce a plot so large that it is difficult 
   #to see on screen. 
# To facet on only one of columns or rows, use . as a place holder. For example, row var~ . will create 
   #a single column with multiple rows.

#sets of histograms showing the distribution of carat conditional on colour.
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

#set of histograms shows proportions
#The density plot makes it easier to compare distributions ignoring the relative abundance of diamonds within each colour.
#The ..density.. syntax is new. The y-axis of the histogram does not come from the original data, but
   #from the statistical transformation that counts the number of observations in each bin. 
   #Using ..density.. tells ggplot2 to map the density to the y-axis instead of the default use of count.
qplot(carat, ..density.., data = diamonds, facets = color ~ .,geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
#we could see that high-quality diamonds (colour D) are skewed towards small sizes, and as quality declines the distribution becomes more flat.


###Other options  -- same effect as their plot equivalents:
#xlim, ylim: set limits for the x- and y-axes, each a numeric vector of length two, 
       #e.g., xlim=c(0, 20) or ylim=c(-0.9, -0.5).
#log: a character vector indicating which (if any) axes should be logged. 
       #For example, log="x" will log the x-axis, log="xy" will log both.
#main: main title for the plot, centered in large text at the top of the plot.
   #This can be a string (e.g., main="plot title") or an expression (e.g.,main = expression(beta[1] == 1)). 
                                      #See ?plotmath for more examples of using mathematical formulae.
?plotmath 
#xlab, ylab: labels for the x- and y-axes. As with the plot title, these can be character strings or mathematical expressions.
qplot(carat, price, data = dsmall, xlab = "Price($)", ylab = "Weight(carats)", main = "Price-weight relationship")
qplot(carat, price/carat, data = dsmall,ylab = expression(frac(price,carat)),xlab = "Weight (carats)",main="Small diamonds",xlim = c(.2,1))
qplot(carat, price, data = dsmall, log = "xy")
qplot(log(carat), log(price), data = dsmall)  #same as above

###another difference in plot() and qplot
# To add further graphic elements to a plot produced in base graphics, you
# can use points(), lines() and text(). With ggplot2, you need to add
# additional layers to the existing plot, described in the next chapter.


#2.Mastering the layered grammar of graphics -- six components
#fuel economy dataset -- mpg, Question: How are engine size and fuel economy related?
head(mpg) # manufacturer, model, displ(engine displacement in litres), engine size(cyl, number of cylinders), 
          #transmission(trans), drive(drv), cty(city mpg), hwy(highway mpg), fuel economy, class
qplot(displ, hwy, data = mpg, colour = factor(cyl))








