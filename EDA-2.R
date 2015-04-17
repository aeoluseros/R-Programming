setwd("D:\\study\\DataScience")
source("http://bioconductor.org/biocLite.R")
biocLite("impute")

#1. Hierachical Clustering - essential when it comes to visualizing a high dimensional or multidimensional data.
#a common approach is agglomerative approach -- a kind of bottom-up approach
#How do we define close?
   #Distance or Similarity: 
        #Continuous - Euclidean Distance (high-dimension problems)
        #Continuous - Correlation Distance
        #Binary - Manhattan Distance (simple sum of the horizontal and vertical components)
set.seed(1234)
par(mar=c(1,1,1,1))
x<-rnorm(12, mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12, mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.08,y,labels=as.character(1:12))
dataFrame<-data.frame(x=x,y=y)
dist(dataFrame) #returns the distance matrix  #dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#######dist:
dist(dataFrame,"canberra")
dist(dataFrame,"maximum")
dist(dataFrame,"minkowski")  #The p norm, default is 2, so equal to euclidean
dist(dataFrame,"manhattan")
dist(dataFrame,"binary") # The vectors are regarded as binary bits, so non-zero elements are 'on' and zero elements are 'off'. The distance is the proportion of bits in which only one is on amongst those in which at least one is on.
datab<-matrix(rbinom(100,1,1/3),10,10)
dist(datab,"binary")    #
datab<-matrix(rbinom(100,1,1),10,10)  #still don't know what is binary distance, but not important though
dist(datab,"binary") 
#######
#clustering starts - dendrogram
distxy<-dist(dataFrame)
hClustering<-hclust(distxy,labels)  #hclust is based on distance
#hclust result: 
   #merge: an n-1 by 2 matrix. Row i of merge describes the merging of clusters at 
           #step i of the clustering. If an element j in the row is negative, then 
           #observation -j was merged at this stage.If j is positive then the merge 
           #was with the cluster formed at the (earlier) stage j of the algorithm. 
           #Thus negative entries in merge indicate agglomerations of singletons, and 
           #positive entries indicate agglomerations of non-singletons.
   #height:a set of n-1 real values (non-decreasing for ultrametric trees). The 
           #clustering height: that is, the value of the criterion associated with 
           #the clustering method for the particular agglomeration.
   #order: a vector giving the permutation of the original observations suitable for 
           #plotting, in the sense that a cluster plot using this ordering and matrix 
           #merge will not have crossings of the branches.
str(hClustering)
hClustering$merge  #negative values are in the bottom, positive ones are after merging
par(mar=c(5.1,4.1,4.1,2.1))
plot(hClustering)
#prettier dendrogram
mpcluster<-function(hClustering,lab=hClustering$labels,lab.col=rep(1,length(hClustering$labels)),hang=0.1,...){
        y<-rep(hClustering$height,2)
        x<-as.numeric(hClustering$merge)
        y<-y[which(x<0)]  #only get those singletons
        x<-x[which(x<0)]  #only get those singletons
        x<-abs(x)
        y<-y[order(x)]
        x<-x[order(x)]
        plot(hClustering,labels=FALSE,hang=hang, ...)
        text(x=x,y=y[hClustering$order]-(max(hClustering$height)*hang),labels=lab[hClustering$order],col=lab.col[hClustering$order],srt=90,adj=c(1,0.5),xpd=NA,...)
                #srt is the incline degree (90 is vertical/perpendicular). adj(y-axis,x-axis) is the adjustment of position
}
mpcluster(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

#how to merge points together - average linkage, complete linkage, etc
#average linkage: if you take two points and their new coordinate location, it's just the average of their x coordinates and y coordinates
                #it's like the center of that group of points.
#complete linkage: if you want to measure the distance between two clusters of points, 
                #then you take the farthest two points from the two clusters as the distance.
str(hclust) #function (d, method = "complete", members = NULL)  
            #method: This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

#heatmap function - runs cluster analysis on the rows of the table and on the columns of the table
#think of the columns of the as sets of observations even if they are not actually observations (the columns might be variables, but you can run cluster analysis on that too).
#so heatmap is to organize the rows and the columns of the tables so that you can visualize them(visualize groups or blocks of observations within the table using the image function).
x<-rnorm(12, mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12, mean=rep(c(1,2,1),each=4),sd=0.2)
dataFrame<-data.frame(x=x,y=y)
set.seed(143)
dataMatrix<-as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)  #heatmap also take 
#library(data.table)
#dataTable<-data.table(dataFrame)[sample(1:12),]
#heatmap(dataTable)     #dataTable is not supported because every subset of data table is data table.

#issues about hierachical clustering
   #the piciture may be unstable: 
        #change a few points
        #pick a different distance metrics
        #change the merging strategy
        #change the scale of points for one variable
   #choosing where to cut isn't always obvious
   #clustering is primarily used for EDA


#2. K-means Clustering
#summarize high dimensional data and get a sense of what patterns your data shows, what kinds of observations are similar/different to each other
#K-means clustering partitions group of observations into a fixed number of clusters.
        #each group would have a centroid, and assign things to closest centroid
        #recalculate centroids
#require: a defined distance metric, a number of clusters, an initial guess to cluster centroids.
#produce: final estimate of cluster centorids, and assignment of each point to clusters.
set.seed(1234)
par(mfrow = c(1,1),mar=c(0,0,0,0))
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.1,y,labels=as.character(1:12))  #1:12 would sequantially be assigned to (x,y)
dataFrame<-data.frame(x,y)
kmeansObj<-kmeans(dataFrame, centers=3)  #kmeans is based on original data
?kmeans
#kmeans(x, centers, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace=FALSE)
#most commonly the algorithm given by MacQueen (1967) but sometimes that given by Lloyd (1957) 
#and Forgy (1965). The Hartigan-Wong algorithm generally does a better job than either of those, 
#but trying several random starts (nstart> 1) is often recommended.
names(kmeansObj)
kmeansObj$cluster
par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)
#heatmap
par(mar=c(5.1,4.1,4.1,2.1),mfrow=c(1,2))
dataMatrix<-as.matrix(dataFrame)[sample(1:12),]
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")  #why in reverse order? nrow(dataMatrix):1. Because draw from top to bottom
?image
#image(x, y, z, zlim, xlim, ylim, col = heat.colors(12), add = FALSE, xaxs = "i", yaxs = "i", 
        #xlab, ylab,breaks, oldstyle = FALSE, useRaster, ...)
#x, y: locations of grid lines at which the values in z are measured. These must be finite, non-missing 
#and in (strictly) ascending order. By default, equally spaced values from 0 to 1 are used. If x 
#is a list, its components x$x and x$y are used for x and y, respectively. If the list has 
#component z this is used for z.
#z: a matrix containing the values to be plotted (NAs are allowed). Note that x can be used instead 
#of z for convenience.
        #therefore, if only one parameter, we call the function image(x, ...).

image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n") #yaxt="n" means no index label


#3. Dimension Reduction - PCA  and SVD
#Introduction:
set.seed(12345)
par(mfrow=c(1,1),mar=rep(2,4))
dataMatrix<-matrix(rnorm(400),nrow=40)   #40 rows, 10 columns
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])  #
heatmap(dataMatrix)

#what if we add a pattern?
set.seed(678910)
for(i in 1:40){
        #flip a coin
        coinFlip<-rbinom(1,size = 1,prob = 0.5)
        #if coin is head add a common pattern to that row
        if(coinFlip){
                dataMatrix[i,]<-dataMatrix[i,]+rep(c(0,3),each=5)
        }
}
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])  #nrow(dataMatrix):1 --> 40,39,...,2,1
#image(1:40,1:10,dataMatrix[nrow(dataMatrix):1,])
#The length of x should be equal to the nrow(z)+1 or nrow(z). In the first case x 
#specifies the boundaries between the cells: in the second case x specifies the 
#midpoints of the cells. Similar reasoning applies to y. It probably only makes sense 
#to specify the midpoints of an equally-spaced grid. If you specify just one row or 
#column and a length-one x or y, the whole user area in the corresponding direction 
#is filled. For logarithmic x or y axes the boundaries between cells must be specified.

heatmap(dataMatrix)   #two parts are easily separated and clustered
#Patterns in Rows and Columns
hh<-hclust(dist(dataMatrix))
dataMatrixOrdered<-dataMatrix[hh$order,]  #should be ordered to make sure trees are not crossed
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrix),40:1,,xlab="Row Mean",ylab="Row",pch=19)
plot(colMeans(dataMatrix),xlab="Column",ylab="Column Mean",pch=19) #columns are rather separated

#related problems
#suppose you have multivariate variables X1,...,Xn so that X1=(X11,...,X1m)
#Statistical goal: Find a new set of multivariate varibles that are uncorrelated 
#and explains as much variance as possible.
#Data Compression goal: If you put all the variables together in one matrix, find the 
#best matrix created with fewer varibes (lower rank) that explains the original data.

#SVD: X=UDV' (columns of U are orthogonal(left singular vectors), columns of V are orthogonal(right singular vectors) and D is a diagonal matrix(singular values))
#PCA: PC are equal to the right sigular values if you first scale(subtract the mean, divide by the standard deviation) the variables.
svd1<-svd(scale(dataMatrixOrdered))  #scale column by column
str(svd1) #d is 10*10 diagonal, u is 40*10, v is 10*10
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,xlab="Row",ylab="First left singluar vector",pch=19) #svd1$u[,1] first component's loading
 #you can see that first left singluar vector has a negative values for rows 40 through 18 or 17, positve values for the remainings.
 #that shows the clear separation in the means of the two sets of rows.
plot(svd1$v[,1],xlab="Column",ylab="First right singluar vector",pch=19)
#the first right singular vector shows the change in the mean between the first 5 columns and the second 5 columns
#plot: For simple scatter plots, plot.default will be used.
        #plot(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL, log = "", main = NULL, 
        #sub = NULL, xlab = NULL, ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes,
        #panel.first = NULL, panel.last = NULL, asp = NA, ...)

####U's columns is used for analyzing rows; V's columns is used for analyzing Columns(components).####


#the nice thing of SVD is that it immediately picked up on the shift in the means, both kind from the row dimension and the column dimension
        #without you have to tell it anything(totally unsupervised), it just picked up automatically.

#another components of SVD:
par(mfrow=c(1,2),mar=c(5,4,4,2))
plot(svd1$d,xlab="Column",ylab="Singular Value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="column",ylab="Prop. of Variance Explained",pch=19)

#relationship to principal components
svd1<-svd(scale(dataMatrixOrdered))
pca1<-prcomp(dataMatrixOrdered,scale=TRUE)
#prcomp's result:
   #sdev: the standard deviations of the principal components (i.e., the square roots of the 
         #eigenvalues of the covariance/correlation matrix, though the calculation is actually done 
         #with the singular values of the data matrix).
   #rotation: the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
   #x: if retx is true the value of the rotated data (the centred (and scaled if requested) data multiplied by the rotation matrix) is returned. Hence, cov(x) is the diagonal matrix diag(sdev^2).
str(pca1)
# pca1$sdev
# pca1$x
# pca1$roration
# pca1$center
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singluar Vector 1")
abline(0,1)
#we could see that pca1$rotation[,1] and svd1$v[,1] are exactly the same things.
#so PCA and SVD are essentially the same thing.

#Components of the SVD - Variance Explained
constantMatrix<-dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[1,]<-rep(c(0,1),each=5)}
svd1<-svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular Value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of Variance Exaplained",pch=19)
 #we could confirm from the last plot that 100% variance are explained by the first singular value (eigenvalue of d).

#what if we add a second pattern?
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
par(mfrow=c(1,1))
plot(hh)
dataMatrixOrdered<-dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column,",ylab="Pattern 1")
plot(rep(c(0,1),5),pch=19,xlab="Column,",ylab="Pattern 2")
#we could see there are two separate patterns buried within the data. We could use SVD to pick them up.
svd2<-svd(scale(dataMatrixOrdered))   # need to order the matrix to put similar things together first!!!!!
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1],pch=19,xlab="Column",ylab="First Right Singluar Vector")  #use V matrix
plot(svd2$v[,2],pch=19,xlab="Column",ylab="Second Right Singluar Vector")
plot(svd2$v[,3],pch=19,xlab="Column",ylab="Third Right Singluar Vector")
#we could see patterns are generally separated although not perfect because data are really confounding.
svd1<-svd(scale(dataMatrixOrdered))   # need to order the matrix to put similar things together first!!!!!
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular Value",pch=19)
plot(svd1$d^2/sum(svd2$d^2),pch=19,xlab="Column",ylab="Third Right Singluar Vector")
sum(svd1$d[1:4]^2/sum(svd2$d^2))


###Missing Values###
dataMatrix2<-dataMatrixOrdered
#Randomly insert some missing data
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
svd1<-svd(scale(dataMatrix2))   #Doesn't work: infinite or missing values in 'x'
#use library impute to deal with missing values
#source("http://bioconductor.org/biocLite.R")
#biocLite("impute")
library("impute")
dataMatrix2<-impute.knn(dataMatrix2)$data  #impute(assign) missing data points, take a missing values in a row and impute it by knn to that row
svd1<-svd(scale(dataMatrixOrdered))  #svd on the pre-ordered matrix would have better effect.
svd2<-svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

##Final Face Example
load("./EDAData/face.rda")
par(mfrow=c(1,1))
image(t(faceData)[(1:ncol(faceData)*(8/8)),1:ncol(faceData)*(6/8)])
#when rows read is increased from 1:ncol(faceData)*(6/8)) to 1:ncol(faceData)*(8/8)), the chin of the face shows up.
#therefore, the top of the row is the top of face.  --> the save in the matrix is regular.
#But the image function transpose/inverse transpose the matrix when drawing on the device.
#so we need to transpose the matrix first to get a correct image. after transpose, we find out that the image is upside down.
#therefore, the image function, after transposing the original data set, also flip the matrix left-right.

image(t(faceData)[,(nrow(faceData)*(7/8)):1])  #will draw the top 7/8. so the first row of the original matrix saves the top of the image
image(t(faceData)[,(nrow(faceData)*(8/8)):(nrow(faceData)*(6/8))]) 
image(t(faceData)[,nrow(faceData):1])  #the way of image() function to draw plot is reverse.
svd1<-svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance Exaplained")
#the first five to ten vectors almost capture pretty much all of the variation in the data set.
approx1<-svd1$u[,1]%*%t(svd1$v[,1])*svd1$d[1]
approx5<-svd1$u[,1:5]%*%diag(svd1$d[1:5])%*%t(svd1$v[,1:5])
approx10<-svd1$u[,1:10]%*%diag(svd1$d[1:10])%*%t(svd1$v[,1:10])
par(mfrow=c(1,4))
image(t(approx1)[,nrow(approx1):1],main="(approx1)")  
image(t(approx5)[,nrow(approx1):1],main="(approx5)")
image(t(approx10)[,nrow(approx1):1],main="(approx10)")
image(t(faceData)[,nrow(approx1):1],main="(Original)")

#notes:
#PC's/SV's may mix real patterns
#alternatives: Factor Analysis, Independent Component Analysis, Latent Semantic Analysis




#4. Working with Color
#The default color schemes for most plots in R are horrendous
#palette:heat.colors(),rainbow(),topo.colors()
palette(heat.colors(15))
#grDevices package has two functions: colorRamp, colorRampPalette -- help to interpolate between colors
colors() #this functions lists names of colors you can use in any plotting function
#colorRamp() take a palette of colors and return a function that takes values between 0 and 1 (e.g. see 'gray' function)
gray(0:8 / 8)
#colorRampPalette take a palette of colors and return a function that takes integer arguments 
  #and returns a function that takes integer arguments and returns a vector of colors interpolating the palette(e.g. heat.color, topo.colors)
pal<-colorRamp(c("red","blue"))
u<-pal(0) #u[1,1] is red, u[1,2] is blue, u[1,3] is green
pal(1) #blue
pal(0.5)  #half red, half blue
pal(seq(0,1,len=10))
pal2<-colorRampPalette(c("red","yellow"))
pal2(2)  #pal2 will take integer argument not numbers between 0 and 1
pal2(10)

#RColorBrewer Package -- it contains 3 types of palettes interesting palette: 
                #-Sequential(from light to dark, used for ordered data from low to high), 
                #-Diverging(light in the middle) 
                #-Qualitative(no order)
#Palette information can be used in conjunction with the colorRamp() and colorRampPalette()
library(RColorBrewer)
#brewer.pal is the only useful function in the package
cols<-brewer.pal(3,"BuGn")   #Blue and Green Palette
str(brewer.pal)  #brewer.pal(n, name)  
        #n is # of colors you want in your palette. n is usually small. You don't need that many blotches on your palette as primary colors.
        #name of palette: not easy to remember. you just need to check it in the help page of brewer.pal
?brewer.pal
cols
pal<-colorRampPalette(cols)
par(mfrow=c(1,1))
image(volcano,col=pal(20))  #use 20 colors to represent the image
#another function uses the ColorBrewer palette is the smoothScatter function which comes with R
#this function is very useful you have to plot a lot of different points
#it creates a 2D histogram. the default set of colors is the blue sequential palette
install.packages("KernSmooth")
x<-rnorm(10000)
y<-rnorm(10000)
smoothScatter(x,y)

#Some other plotting notes
#rgb function
#color transparency can be added via the alpha parameter to rgb 
#the colorspace package can be used for a different control over colors
x<-rnorm(1000)
y<-rnorm(1000)
#when there are too many data overlapping, we could use some transparency
plot(x,y,pch=19)
plot(x,y,pch=19,col=rgb(0,0,0,alpha=0.2))  



#5. Clustering Case study - use smart phone to predict human activities
#The basic idea of EDA is to produce a rough cut of the kind of analysis that you ultimately want to do.
#It give you a rough idea of what information you're going to able to extract out of your data set and 
#what kinds of your questions you're going to be able to feasibly answer and what questions might not be possible.
#with the given data set.
load("./EDAData/samsungData.rda")
head(samsungData)
names(samsungData)[1:12]
table(samsungData$activity)

#Plotting Average Acceleration for the first subject
par(mfrow=c(1,2),mar=c(5,4,1,1))
samsungData<-transform(samsungData,activity=factor(activity)) #use transform to convert an attribute into factor variable.
sub1<-subset(samsungData,subject==1)
plot(sub1[,1],col=sub1$activity,ylab=names(sub1)[1])
plot(sub1[,2],col=sub1$activity,ylab=names(sub1)[2])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity),pch=1) 
#green is standing, red is sitting, black is laying, blue is walking, gray(turquoise) is walkdown, magenta is walkup
#so we could see that the mean body acceleration is relatively uninteresting for things like sitting, standing
#but things like walking, walkdown and walkup there is much more variability

#we could cluster the data based on average acceleration
source("./EDAData/mpcluster.R")
distanceMatrix<-dist(sub1[,1:3]) #"tBodyAcc.mean...X" "tBodyAcc.mean...Y" "tBodyAcc.mean...Z"
hclustering<-hclust(distanceMatrix)
par(mfrow=c(1,1))
args(mpcluster)
#function (hClustering, lab = hClustering$labels, lab.col = rep(1,length(hClustering$labels)), hang = 0.1, ...) 
mpcluster(hclustering,lab.col=unclass(sub1$activity))  #unclass returns (a copy of) its argument with its class attribute removed.
#we can see that all the colors are kind of jumbled together at the bottom, no clear clusters from top to bottom
#so we might need to look further and extract more information out of here.
#further analysis required.(condition might improve if we use maximum acceleration)

#then we could look at maximum accerleration for the first subject
par(mfrow=c(1,2))
plot(sub1[,10],pch=19,col=sub1$activity,ylab=names(sub1)[10]) 
plot(sub1[,11],pch=19,col=sub1$activity,ylab=names(sub1)[11])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity),pch=19,cex=0.8) 
#we could see that for standing, sitting and laying, there is not a lot of interesting things going on
#walk, walkdown, walkup are interesting here.

#also, we cluster based on maximum acceleration
source("./EDAData/mpcluster.R")
distanceMatrix<-dist(sub1[,10:12])
hclustering<-hclust(distanceMatrix)
par(mfrow=c(1,1))
mpcluster(hclustering,lab.col=unclass(sub1$activity)) 
#we could see two very clear clusters on the left hand side you get the various walking activity
#on the right hand side, we get various non-moving activities: laying, standing and sitting.
#and beyond that, things are jumbled together. You see there is a lot of turquoise on the left 
#and so that's clearly one activity, but in the blue and the magenta kind of mixed together.
#So, a clustering based on maximum acceleration seems to separate out moving from non-moving, but once
#you get within those clusters, then it's a little bit hard to tell what is what, based just on maximum acceleration

#we can try SVD on the whole data set
svd1<-svd(scale(sub1[,-c(562,563)]))  #562,563: "subject"  "activity"
par(mfrow=c(1,2))
plot(svd1$u[,1],col=sub1$activity,pch=19)  #u is used to analyze rows
plot(svd1$u[,2],col=sub1$activity,pch=19)
#again you can see there is a similar type of pattern the first singular vevtor really seems to 
#separate out the moving from the non-moving. (green, red, black on the bottom, blue, turquoise, magenta on the top)
#the second singluar vector is a little bit more vague. what it's looking at seems to be separating out
#the magenta color from all the other cluters, and so I think this is the walkup.
#so it's not clear what is the different about that gets highlighted on the 2nd singular vector here.

#One of things we can do is try to find maximum contributor
#so in the second right singular vector, we can try to figure out which of these features 
#is con'tributing most to the variation between the different observations.
par(mfrow=c(1,4))
plot(svd1$v[,1],pch=19)
plot(svd1$v[,2],pch=19)  #why the second? the 2nd column is about variance
plot(svd1$v[,3],pch=19) 
plot(svd1$v[,4],pch=19)
par(mfrow=c(1,1))
#New Clustering with maximum contributer
#we need to pick out the observation with the largest variance
maxContrib<-which.max(svd1$v[,2]) #dim(sub1): 347*563 #dim(svd1$v) = 561 * 347. ; dim(svd1$u) = 347 * 347. svd1$d = 347
#which.max: Determines the index of the first maximum(TRUE value) of a numeric (or logical) vector.
#so in SVD, the # of columns could exceed # of observations

#which.max(svd1$v[,2]) returns the row number of the svd1$v[,2], picked out from 561 features(variables)

distanceMatrix<-dist(sub1[,c(10:12,maxContrib)]) #I'll cluster based on the maximum acceleration plus this extra feature
hclustering<-hclust(distanceMatrix)
mpcluster(hclustering, lab.col=unclass(sub1$activity))
#green is standing, red is sitting, black is laying, blue is walking, gray(turquoise) is walkdown, magenta is walkup
#you can see now that various activities seem to be separating out a little bit more, at least the three movement 
#activities have clearly been separated -- magenta, dark blue, and turquoise
#green, black and red are still jumbled.

names(samsungData)[maxContrib]
#so this max contributor was the body acceleration, the mean body acceleration in the frequency 
        #domain for the z direction. So this is the body acceleration for the z direction where 
        #they applied and you transform and they give you the frequency components from that.

#we can try another clustering technique here with K-means clustering
kclust<-kmeans(sub1[,-c(562,563)],centers=6,nstart=100,algorithm="Hartigan-Wong")
#you may get different answer by kmeans method, depending on how many times, starting values you've tried
#and how often you run it so whenever you start k-means it has to choose a starting point for where
#the cluster centers are often just chosen randomly in most algorithms.
#function (x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace = FALSE)  
#it's usually good to set the nstart argument to be more than one so you can start at many different starting points,
#just so you can get the optimal, or a more optimal solution.
table(kclust$cluster, sub1$activity)
#k-means have a little bit trouble separating out the laying, sitting and standing.

#what features seem to be important or classifying into that cluster. 
#cluster 1 variable Centers(laying)
par(mfrow=c(1,1))
plot(kclust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="")
#we can see that the center has a relatively high value for a high, or positive values for the
#first three features, which is kind of the mean body acceleration, and lower values for 4-10.
#cluster 4 variable Centers(walking)
plot(kclust$center[4,1:10],pch=19,ylab="Cluster Center",xlab="")
#we can see it has more interesting values for other Features, there is mean accelerations and also
#max accelerations.


#6. Air Pollution Case Study
#are air pollution levels lower now than decades before?
#In this case we look at this problem from the perspective of fine particle air pollution.
#Data from 1999 - 2012
pm0<-read.table("./EDAdata/RD_501_88101_1999-0.txt",comment.char="#",header=FALSE,sep="|",na.strings="")
dim(pm0)
head(pm0)
#because the columns are in the format of comment, we could use readLine to read it in instead of trying to read the column names in read.table
cnames<-readLines("./EDAdata/RD_501_88101_1999-0.txt",1)
#then split out column names
cnames<-strsplit(cnames,"|",fixed=TRUE)
#fixed: logical. If TRUE match split exactly, otherwise use regular expressions.
cnames
class(cnames)  #a list which contains only one list of characters
class(cnames[[1]])  #character
names(pm0)<-cnames[[1]]
head(pm0)
#some column names are not valid because they have spaces in it, such as "state code, county code"
#fix it -- make.names:
names(pm0)<-make.names(cnames[[1]])
        #make.names(names, unique = FALSE, allow_ = TRUE)
        #All invalid characters are translated to ".". A missing value is translated to "NA".
        make.names(c("a and b", "a-and-b"), unique = TRUE)
        make.names(c("a and b", "a_and_b"), unique = TRUE)
        make.names(c("a and b", "a_and_b"), unique = TRUE, allow_ = FALSE)
        make.names(c("", "X"), unique = TRUE)
        #another useful related function: make.unique: 
        #make.unique(names, sep = ".") #Makes the elements of a character vector unique by appending sequence numbers to duplicates.
        make.unique(c("a", "a", "a"))
        make.unique(c(make.unique(c("a", "a")), "a"))
        make.unique(c(make.unique(c("a", "a")), "a.1", "a"))
        rbind(data.frame(x = 1), data.frame(x = 2), data.frame(x = 3))
        rbind(rbind(data.frame(x = 1), data.frame(x = 2)), data.frame(x = 3))
head(pm0)
x0<-pm0$Sample.Value
class(x0)  
str(x0)
summary(x0)
mean(is.na(x0))  #0.1125532 --> about 11.25% are missing.
#first problem: are missing values a problem?
##
##

pm1<-read.table("./EDAdata/RD_501_88101_2012-0.txt",comment.char="#",header=FALSE,sep="|",na.strings="")
dim(pm1)
cnames_1<-readLines("./EDAdata/RD_501_88101_2012-0.txt",1)
cnames_1<-strsplit(cnames_1,split="|",fixed=TRUE)
cnames_1<-make.names(cnames_1[[1]])
names(pm1)<-cnames_1
head(pm1)
x1<-pm1$Sample.Value
str(x1)
summary(x1)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -10.00    4.00    7.63    9.14   12.00  909.00   73133 
mean(is.na(x1))
summary(x0)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.00    7.20   11.50   13.74   17.90  157.10   13216 
mean(is.na(x0))

#let's take a visual representation of the data:
boxplot(x0,x1)
#There is a lot of right skew in the pm2.5 data. so you can see that it's kind of all smooshed down near zero but then there's very large values.
#maximum for x1 is 909, for the U.S. it's too high to be possible. --  it might be an error.
#one way to fix this box plot is to take a log of these guys
boxplot(log(x0),log(x1))  #the base of log in R is 10 by default.
#why there are negative values in x1?
#PM2.5 should be positive. so let's pluck out the values that happen to be negative
#So I'll create a logical vector.
negative<-x1<0
str(negative)
sum(negative, na.rm=TRUE)
mean(negative, na.rm=TRUE)  #2% values
oriDates<-pm1$Date  #int
str(oriDates)
library(lubridate)
dates_POSIX<-ymd(oriDates)  #POSIXct
dates_Date<-as.Date(as.character(oriDates),"%Y%m%d")
str(dates_POSIX)
str(dates_Date)
as.numeric(dates_POSIX)
hist(dates_Date,"month") 
#hist(x, breaks = "Sturges",freq = NULL, probability = !freq,include.lowest = TRUE, right = TRUE,
#density = NULL, angle = 45, col = NULL, border = NULL,main = paste("Histogram of" , xname),xlim = range(breaks), ylim = NULL,
#xlab = xname, ylab,axes = TRUE, plot = TRUE, labels = FALSE,nclass = NULL, warn.unused = TRUE, ...)
hist(dates_POSIX,"month")
#we could see that most measurements are occuring in the winter and spring months
hist(dates_Date[negative],"month") 
hist(dates_POSIX[negative],"month")
#may be negative values are because the pm2.5 values are too low so measurements error occurs
#but given that just 2% of the data, I'm not going to spend too much time worrying about it at the moment.

#Exploring Change at One Monitor/location
site0<-unique(subset(pm0,State.Code==36,c(County.Code, Site.ID)))
site1<-unique(subset(pm1,State.Code==36,c(County.Code, Site.ID)))
#subset(x, subset, select, drop = FALSE, ...) 
        #x: object to be subsetted.  #subset: logical expression indicating elements or rows to keep.
        #select: expression, indicating columns to select from a data frame;
site0<-paste(site0[,1],site0[,2],sep=".")
site1<-paste(site1[,1],site1[,2],sep=".")
str(site0)  #33 sites
class(site0)  #character
str(site1)  #18 sites
#what's the intersection between these two?
both<-intersect(site0,site1)
?intersect
        #union(x, y)
        #intersect(x, y)
        #setdiff(x, y)
        #setequal(x, y)
        #is.element(el, set)
str(both)  #10 common sites
#take out records of these sites
pm0$County.Site<-with(pm0,paste(County.Code,Site.ID,sep="."))
pm1$County.Site<-with(pm1,paste(County.Code,Site.ID,sep="."))
cnt0<-subset(pm0,State.Code==36 & County.Site %in% both)
cnt1<-subset(pm1,State.Code==36 & County.Site %in% both)
#split this data frame by the monitor
split(cnt0,cnt0$County.Site) # too much data to be seen clearly, so let's count # of obs. for each site
sapply(split(cnt0,cnt0$County.Site),nrow)
sapply(split(cnt1,cnt1$County.Site),nrow)
#let's go to pick up 63.2008 to see the trend of pm2.5
pm0sub<-subset(pm0, State.Code==36&County.Code==63&Site.ID==2008)
pm1sub<-subset(pm1, State.Code==36&County.Code==63&Site.ID==2008)
dim(pm0sub)
dim(pm1sub)
#plot the data as a function of dates
dates0<-pm0sub$Date
dates1<-pm1sub$Date
x0sub<-pm0sub$Sample.Value
x1sub<-pm1sub$Sample.Value
dates0<-as.Date(as.character(dates0),"%Y%m%d")
dates1<-as.Date(as.character(dates1),"%Y%m%d")
plot(dates0,x0sub)  #from July,1999 to Dec,1999, only half year
plot(dates1,x1sub)  #from June 2012 to March,2012
#Build a panel plot
par(mfrow=c(1,2))
plot(dates0,x0sub,pch=20)
abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20)
abline(h=median(x1sub,na.rm=T))
#adjust plots
rng<-range(x0sub,x1sub,na.rm=T)
plot(dates0,x0sub,pch=20,ylim=rng)
abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20,ylim=rng)
abline(h=median(x1sub,na.rm=T))
#we could notice a huge spread of points for the 1999's data
        #therefore, for 2012, the extreme values are also coming down. 

#Let's explore change at the state level
head(pm0)
#take average value by state
mn0<-with(pm0,tapply(Sample.Value,State.Code,mean,na.rm=T))
str(mn0)
summary(mn0)
mn1<-with(pm1,tapply(Sample.Value,State.Code,mean,na.rm=T))
str(mn1)
summary(mn1)
names(mn1)
#create a data frame that has the name of the state and their average pm2.5
d0<-data.frame(state=names(mn0),mean=mn0)
d1<-data.frame(state=names(mn1),mean=mn1)
head(d0)
mrg<-merge(d0,d1,by="state")
?merge
#merge(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x",".y"),incomparables = NULL, ...)
head(mrg)
dim(mrg)
par(mfrow=c(1,1))
with(mrg,plot(rep(1999,52),mrg[,2],xlim=c(1998,2013)))  #use rep(1999,52) to make all obs. plotted on one line
with(mrg,points(rep(2012,52),mrg[,3]))
#use segments function to connect data points
segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])
#we could see most states are going down, while some are going up.






























