setwd("D:\\study\\DataScience")
library(datasets);
data(swiss);
require(stats);
require(graphics);
pairs(swiss, panel=panel.smooth,main="Swiss data",col=3+(swiss$Catholic>50)) #will add a smooth line to each plot
#pairs(swiss, panel=points,main="Swiss data",col=3+(swiss$Catholic>50))




