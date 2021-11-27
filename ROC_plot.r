
x<-y<-0:1;
font = 6;
pred1 <- prediction(predictor1,indicator);
perf1 <- performance(pred1,"tpr","fpr");
perf1.auc <- performance(pred1, "auc");
perf1.auc.areas <- slot(perf1.auc, "y.values");
curve.area1 <- mean(unlist(perf1.auc.areas));
plot(perf1, col="grey82",main=MAIN.TEXT, lty=0,lwd=.1,cex=.01,font.lab=6,cex.lab=1.5,font.main=6,cex.main=1.5,bty="n");
plot(perf1,lwd=1,lty=1 ,spread.estimate="boxplot", add=TRUE);



#draw two curves on one plot
rocplot3 <- function(predictor1, predictor2=NULL ,predictor3=NULL ,NAME1,NAME2=NULL,NAME3=NULL, indicator,MAIN.TEXT) {
X11();
    x<-y<-0:1;
    font = 6;
    pred1 <- prediction(predictor1,indicator);
    perf1 <- performance(pred1,"tpr","fpr");
    perf1.auc <- performance(pred1, "auc");
    perf1.auc.areas <- slot(perf1.auc, "y.values");
    curve.area1 <- mean(unlist(perf1.auc.areas));
	if(!is.null(predictor2)){
    pred2 <- prediction(predictor2,indicator);
    perf2 <- performance(pred2,"tpr","fpr");
    perf2.auc <- performance(pred2, "auc");
    perf2.auc.areas <- slot(perf2.auc, "y.values");
    curve.area2 <- mean(unlist(perf2.auc.areas));
	}
    plot(perf1, col="grey82",main=MAIN.TEXT, lty=0,lwd=.1,cex=.01,font.lab=6,cex.lab=1.5,font.main=6,cex.main=1.5,bty="n");
    plot(perf1,lwd=1,lty=1 ,spread.estimate="boxplot", add=TRUE);
	if(!is.null(predictor2)){
    plot(perf2, col="grey82", lty=0,lwd=2, add=TRUE);
    plot(perf2,lwd=1,lty=3 ,spread.estimate="boxplot", add=TRUE);
	}
    savefont<-par(font=6);
	if(!is.null(predictor3)){
    pred3 <- prediction(predictor3,indicator);
    perf3 <- performance(pred3,"tpr","fpr");
    perf3.auc <- performance(pred3, "auc");
    perf3.auc.areas <- slot(perf3.auc, "y.values");
    curve.area3 <- mean(unlist(perf3.auc.areas));
    plot(perf3, col="grey82", lty=0,lwd=.2, add=TRUE);
#   plot(perf3,lwd=.2,lty=2 ,avg="horizontal",spread.estimate="boxplot", add=TRUE);
    plot(perf3,lwd=.2,lty=2 ,spread.estimate="boxplot", add=TRUE);
	legend("bottomright",		c("AUROC",paste(NAME1,":",round(curve.area1,digit=3)),paste(NAME2,":",round(curve.area2,digit=3)),paste(NAME3,":",round(curve.area3,digit=3))),lty=c(0,1,3,2),cex=0.8)
	par(savefont)
	}
	if(!is.null(predictor2)){	
	legend("bottomright",c("AUROC",paste(NAME1,":",round(curve.area1,digit=3)),paste(NAME2,":",round(curve.area2,digit=3))),lty=c(0,1,3),cex=0.8)
	} else if (!is.null(predictor1)){
	legend("bottomright",c("AUROC",paste(NAME1,":",round(curve.area1,digit=3))),lty=c(0,1,3),cex=0.8)
	}
	par(savefont)
	return(round(curve.area1,digit=3))
}

tiff(file=paste(getwd(),"/result/fig5.tif",sep=""),res=600,units="cm", width = 15, height = 5, pointsize = 5);
par(mfcol=c(1,3),font=6)
GSP3$ids <- MT$GSP$N!=0;
cat("There are ",sum(GSP3$ids)," genes in GSP\n");
GSN3$ids <- MT$GSN$N!=0;
cat("There are ",sum(GSN3$ids)," genes in GSN\n");
indicator<-c(rep(1,sum(GSP3$ids)), rep(0,sum(GSN3$ids)));
predictor2<-c(MT$GSP$PearsonCorCoef[GSP3$ids], MT$GSN$PearsonCorCoef[GSN3$ids]);
predictor1<-c(GSP3$coevo1[GSP3$ids], GSN3$coevo1[GSN3$ids]);
MAIN.TEXT="A.  HPRD dataset                                                                            ";
rocplot3(predictor1=predictor1, predictor2=predictor2, NAME1="Co-evolutionary divergence",NAME2="Mirror tree",NAME3="NULL", indicator=indicator,MAIN.TEXT=MAIN.TEXT);
legend("topleft",c("DeLong Test p-value = 2.2e-16"));

#ROC curve on BIOGRID dataset
TEST$GSP$ids <- TEST$GSP$MT$N!=0;
cat("There are ",sum(TEST$GSP$ids)," within pais\n");
TEST$GSN$ids <- TEST$GSN$MT$N!=0;
cat("There are ",sum(TEST$GSN$ids)," between pairs\n");
indicator<-c(rep(1,sum(TEST$GSP$ids)), rep(0,sum(TEST$GSN$ids)));
predictor1<-c(TEST$GSP$EVO$coevo1[TEST$GSP$ids], TEST$GSN$EVO$coevo1[TEST$GSN$ids]);
predictor2<-c(TEST$GSP$MT$PearsonCorCoef[TEST$GSP$ids], TEST$GSN$MT$PearsonCorCoef[TEST$GSN$ids]);
MAIN.TEXT="B.  BIOGRID dataset                                                                            ";
rocplot3(predictor1=predictor1, predictor2=predictor2, NAME1="Co-evolutionary divergence",NAME2="Mirror tree",NAME3="NULL", indicator=indicator,MAIN.TEXT=MAIN.TEXT);
legend("topleft",c("DeLong Test p-value = 2.2e-16"));
