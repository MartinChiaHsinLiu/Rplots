####install annotation dataset
/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb  -buildver hg19 1000g2015aug /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/
/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb -buildver hg19 seq /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/hg19_seq/ 

/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb -webfrom annovar -buildver hg19 icgc21 /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/
/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb -webfrom annovar -buildver hg19 cadd13 /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/
/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb -webfrom annovar -buildver hg19 clinvar_20160302 /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/


#####conduct ANNOVAR in R
#example in command line
#./table_annovar.pl LA98_allele.txt humandb/ -buildver hg19 -out LA98_allele -protocol ljb26_all,popfreq_max_20150413,phastConsElements46way,tfbsConsSites,clinvar_20140211,cosmic68 -operation f,f,r,r,f,f -csvout
#example in download dataset
#/home/qnerv/Annovar2015Jun/annotate_variation.pl -downdb 1000g2015aug /mnt/isilonX200/qnerv/Annovar2015Mar/humandb -buildver hg19

###parameter setting
#Where is your program?
ANNOVAR.PATH<-"/home/qnerv/annovar/table_annovar.pl"
ANNOVAR.PATH<-"/mnt/isilonX200/qnerv/Annovar2015Mar/table_annovar.pl"
#Where your output should be shown
OUT<-"MYANNOVAR_out"
#What kind of data you wanna extract(http://annovar.openbioinformatics.org/en/latest/user-guide/download/)
PROTOCOL<-"avsnp144,refGene,knownGene,ensGene,dgvMerged,ljb26_all,popfreq_max_20150413,phastConsElements46way,tfbsConsSites,cosmic68,cadd,gerp++gt2,1000g2015aug_all,1000g2015aug_afr,1000g2015aug_eas,1000g2015aug_eur,1000g2015aug_sas,1000g2015aug_amr"
#Type of data; g:gene-based, r:region-based, f:filter-based
OPERATION<-"f,g,g,g,r,f,f,r,r,f,f,f,f,f,f,f,f,f"
#PROTOCOL<-"refGene,1000g2015aug_all,1000g2015aug_afr,1000g2015aug_eas,1000g2015aug_eur,1000g2015aug_sas,1000g2015aug_amr"
#OPERATION<-"g,f,f,f,f,f,f"
#Where temperature data put
TMP.FILE<-paste(c("/home/qnerv/tmp/",OUT,".input"),collapse="")
#TMP.FILE<-paste(c("/mnt/isilonX200/IMPUTE2_RES/LA_DMR_out/",OUT,".input"),collapse="")
#input file format
#chrom start end ref alt comments(optional)
#example: less /mnt/isilonX200/qnerv/Annovar2015Mar/LA98_add.txt
#1       10642	10642   G       A
#1       11008	11008   C       G
#1       11012	11012   C       G
#1       13110	13110   G       A
#1       13116	13116   T       G
#Annovar.input<-read.table("/home/qnerv/test_annovar.txt",header=F)
#NO HEAD txt file!!!!
RAnnovar<-function(Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION){
#write tmp table for annovar input
write.table(Annovar.input,TMP.FILE,row.names=F,col.names=F,quote=FALSE)
#script
SH<-paste(c(ANNOVAR.PATH," ",TMP.FILE," /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/ -buildver hg19 -out ",getwd(),"/",OUT," -nastring . -protocol ",PROTOCOL," -operation ",OPERATION),collapse="")
system(SH)
res.out<-read.delim(paste(c(getwd(),"/",OUT,".hg19_multianno.txt"),collapse=""),header=T,stringsAsFactors=FALSE,sep="\t")
return(res.out)
}

#-vcfinput
RAnnovar_VCF<-function(VCF.FILE,Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION){
#script
#SH<-paste(c("nohup ",ANNOVAR.PATH," -vcfinput ",VCF.FILE," /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/ -buildver hg19 -out ",getwd(),"/",OUT," -nastring . -protocol ",PROTOCOL," -operation ",OPERATION," &"),collapse="")
SH<-paste(c(ANNOVAR.PATH," -vcfinput ",VCF.FILE," /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/ -buildver hg19 -out ",getwd(),"/",OUT," -nastring . -protocol ",PROTOCOL," -operation ",OPERATION),collapse="")
system(SH)
#res.out<-read.delim(paste(c(getwd(),"/",OUT,".hg19_multianno.txt"),collapse=""),header=T,stringsAsFactors=FALSE,sep="\t")
#return(res.out)
}

for(i in 3:1){
	VCF.FILE=paste0("/mnt/isilonX200/IMPUTE2_RES/Bipolar_1M2Mmerge_imputed2017Aug/merge/chr",i,"_impute2_biallele.vcf.gz")
	OUT=paste0("chr",i)
	RAnnovar_VCF(VCF.FILE,Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION)
}

#test run
if(FALSE){
Annovar.input<-PID855[,c(1:2,2:4)]
res.out<-RAnnovar(Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL=PROTOCOL,OPERATION=OPERATION)

Annovar.input<-read.csv("/mnt/isilonX200/qnerv/LCP/INFO.csv",header=T,stringsAsFactors=F)
Annovar.input<-Annovar.input[,c(1,2,2,3,4)]
Annovar.input[Annovar.input[,1]=="chrMT",1]<-"chrM"
Annovar.input[,1]<-gsub("chr","",Annovar.input[,1])
res.out<-RAnnovar(Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION)

Annovar.input<-tmp[,c(1:2,2:4)]
res.out<-RAnnovar(Annovar.input,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION)


}

#test run 2
if(FALSE){
setwd("/mnt/isilonX200/qnerv/1kg_vcf/ANNOVAR/")
info.1M<-DB.DOWNLOAD(input,"SELECT chr, pos FROM Bipolar.Bipolar1M_DATA")
info.2M<-DB.DOWNLOAD(input,"SELECT chr, pos FROM Bipolar.Bipolar2M_DATA")
for(i in 21:1){
	#ANNOVAR annotations
    cat(paste0("reading chr",i, " multianno.txt file...."))
    INFO <- read.csv(paste0("/mnt/isilonX200/qnerv/1kg_vcf/ANNOVAR/chr",i,".hg19_multianno.txt"), sep = '\t')
    cat("done!\n")
    INFO <- INFO[,1:5]
	PROTOCOL<-"avsnp144,refGene,ljb26_all,tfbsConsSites"
	OPERATION<-"f,g,f,f"
	res.out<-RAnnovar(Annovar.input=INFO,ANNOVAR.PATH,OUT,PROTOCOL,OPERATION)
	info.1M.chr<-cbind(info.1M[info.1M$chr==i,],"Y")
	colnames(info.1M.chr)<-c("Chr","Start","1M_Array")
	info.1M.chr<-info.1M.chr[!duplicated(info.1M.chr),]
	info.1M.chr<-as.data.frame(info.1M.chr)
	info.1M.chr$Chr<-as.numeric(info.1M.chr$Chr)
	info.1M.chr$Start<-as.numeric(info.1M.chr$Start)
	
	info.2M.chr<-cbind(info.2M[info.2M$chr==i,],"Y")
	colnames(info.2M.chr)<-c("Chr","Start","2M_Array")
	info.2M.chr<-info.2M.chr[!duplicated(info.2M.chr),]
	info.2M.chr<-as.data.frame(info.2M.chr)
	info.2M.chr$Chr<-as.numeric(info.2M.chr$Chr)
	info.2M.chr$Start<-as.numeric(info.2M.chr$Start)

	res.out<-merge(res.out,info.1M.chr,all.x = T)
	res.out<-merge(res.out,info.2M.chr,all.x = T)

	write.table(res.out,paste0("/mnt/isilonX200/qnerv/1kg_vcf/ANNOVAR/chr",i,".hg19_multianno2.txt"),row.names=F,sep="\t")
}

/home/qnerv/annovar/table_annovar.pl /home/qnerv/annovar/example/ex1.avinput /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/ -dbtype -buildver hg19 -out /home/qnerv/sz/MYANNOVAR_out -protocol ALL.sites.2015_08,AFR.sites.2015_08,AMR.sites.2015_08,EAS.sites.2015_08,EUR.sites.2015_08,SAS.sites.2015_08 -operation f,f,f,f,f,f
/home/qnerv/annovar/table_annovar.pl /home/qnerv/annovar/example/ex2.vcf /mnt/isilonX200/qnerv/Annovar2015Mar/humandb/ -buildver hg19 -out /home/qnerv/sz/MYANNOVAR_out -remove -protocol refGene,1000g2015aug_all,1000g2015aug_afr,1000g2015aug_eas,1000g2015aug_eur,snp138 -operation g,f,f,f,f,f -nastring . -vcfinput



