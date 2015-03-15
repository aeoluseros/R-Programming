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
names(kmeansObj)
kmeansObj$cluster
par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)
#heatmap
par(mar=c(2,4,0.1,0.1),mfrow=c(1,2))
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
#the first right singular vector shows the change in the mean between the first 5 columns and the second five columns
#plot: For simple scatter plots, plot.default will be used.
        #plot(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL, log = "", main = NULL, 
        #sub = NULL, xlab = NULL, ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes,
        #panel.first = NULL, panel.last = NULL, asp = NA, ...)
 #U is used for analyzing rows; V is used for analyzing Columns(components).


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
   #sdev: the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).
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
svd1<-svd(scale(dataMatrixOrdered))
svd2<-svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

##Final Face Example
load("./EDAData/face.rda")
par(mfrow=c(1,1))
image(t(faceData)[,nrow(faceData):1])
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






#5. Clustering Case study








#6. Air Pollution Case Study



