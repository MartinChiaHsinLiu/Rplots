plot.BAR<-function(NAME,YLAB,MAIN,data=data){
tmp.data<-data[data$type==NAME,]
NAME.subgroup<-levels(as.factor(tmp.data$group))
MEAN<-SD<-c()
for(i in 1:length(NAME.subgroup)){
MEAN[i]<-mean(tmp.data$value[tmp.data$group==NAME.subgroup[i]])
SD[i]<-sd(tmp.data$value[tmp.data$group==NAME.subgroup[i]])
}
mp <- barplot(MEAN,ylim=c(0,max(MEAN)+2*max(SD)), axisnames=FALSE, main=MAIN, xlab="Group", ylab=YLAB)
axis(1, labels=NAME.subgroup, at = mp)
box()
segments(mp, MEAN - SD, mp, MEAN + SD, lwd=2)
segments(mp - 0.1, MEAN - SD, mp + 0.1, MEAN - SD, lwd=2)
segments(mp - 0.1, MEAN + SD, mp + 0.1, MEAN + SD, lwd=2)
}
#test.data<-data.frame(type=rep(c("dic","test","moc"),each=10),group=rep(c("G1","G2","G3"),times=10),value=c(1:30))
#plot.BAR(NAME="dic",YLAB="test",MAIN="dic",data=test.data)


