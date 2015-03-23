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