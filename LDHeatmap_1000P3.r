library(LDheatmap)
library(genetics)
READ.VCF<-function(IN.FILE){
test_inpput<-readLines(IN.FILE,n=200)
headindex<-max(grep("#",test_inpput))
headname<-unlist(strsplit(gsub("#","",test_inpput[headindex]),"\t"))
res<-read.delim(IN.FILE,skip=headindex,stringsAsFactors = FALSE,comment.char = "#",header=F)
colnames(res)<-unlist(headname)
return(res)
}
LDHeatmap_KGPp3<-function(CHR=1,startpos=225180827,endpos=225323168){
cat("extract region data from VCF\n")
system(paste0("/mnt/isilonX200/qnerv/NGSapp/bcftools/bcftools filter -r ",CHR,":",startpos,"-",endpos," -O v -o /home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,".vcf /mnt/isilonX200/qnerv/1kg_vcf/ALL.chr",CHR,"_biallele.vcf.gz"))
#system(paste0("/mnt/isilonX200/qnerv/NGSapp/SHAPEIT2/bin/shapeit --input-vcf /home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,".vcf --output-max /home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
#system(paste0("/mnt/isilonX200/qnerv/NGSapp/SHAPEIT2/bin/shapeit -convert --input-haps /home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf --output-vcf /home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
#legend
#tmp.info<-READ.VCF(paste0("/home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
tmp.data<-READ.VCF(paste0("/home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,".vcf"))
tmp.info<-tmp.data[,c(3,2,4,5)]
colnames(tmp.info)<-c("rs","position","0","1")
write.table(tmp.info,paste0("/home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,".legend"),row.names=F,quote=F,sep="\t")
DATADist<-as.numeric(tmp.info$position)
DATASNPNAME<-tmp.info$rs
DATASNP<-tmp.data[,10:ncol(tmp.data)]
cat("Parse data to ATCG\n")
for(i in 1:nrow(tmp.info)){
DATASNP[i,]<-gsub("0",tmp.info[i,3],DATASNP[i,])  
DATASNP[i,]<-gsub("1",tmp.info[i,4],DATASNP[i,]) 
}
DATASNP<-t(DATASNP)
DATASNP.LD<-as.data.frame(matrix(ncol=ncol(DATASNP),nrow=nrow(DATASNP)))
cat("Transform genotype object\n")
for(i in 1:ncol(DATASNP)){
DATASNP.LD[,i]<-genotype(gsub("\\|","/",DATASNP[,i]))
}
cat("Plot LD heatmap\n")
png(paste0("/home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,".png"),width = 960, height = 960, units = "px", pointsize = 24)
ll<-LDheatmap(DATASNP.LD, DATADist, LDmeasure = "r",title = "Pairwise LD in r^2", add.map = TRUE, name = "myLDgrob", add.key = TRUE,flip=TRUE,color=heat.colors(20))
dev.off()
}
#ex.
#LDHeatmap_KGPp3(CHR=5,startpos=168972089,endpos=169081127)

##parse our in-house vcf
#system("cp /mnt/isilonX200/mibNGS_illumina_HiSeq_ExonCapture/Table/VCF_Merge/Stage_I/Stage_I.merge.vcf /home/qnerv/EGFR_TBB/Stage_I.merge.vcf")
#system("bgzip /home/qnerv/EGFR_TBB/Stage_I.merge.vcf")
#system("tabix -p vcf /home/qnerv/EGFR_TBB/Stage_I.merge.vcf.gz")
#system(paste0("/mnt/isilonX200/qnerv/vcftools_0.1.12b/bin/vcftools --gzvcf /home/qnerv/EGFR_TBB/Stage_I.merge.vcf.gz --remove-indels --min-alleles 2 --max-alleles 2 --stdout --recode > /home/qnerv/EGFR_TBB/Stage_I.merge_biallele.vcf"))
#system("bgzip /home/qnerv/EGFR_TBB/Stage_I.merge_biallele.vcf")
#system("tabix -p vcf /home/qnerv/EGFR_TBB/Stage_I.merge_biallele.vcf.gz")

LDHeatmap_VCF<-function(VCF="/home/qnerv/EGFR_TBB/Stage_I.merge_biallele.vcf.gz",WORK.DIR="/home/qnerv/EGFR_TBB/",CHR="chr1",startpos=225180827,endpos=225323168,prephasing=TRUE){
#if prephasing=TRUE, it will create phased vcf for LDExplore to identify LD blocks
cat("extract region data from VCF\n")
system(paste0("/mnt/isilonX200/qnerv/NGSapp/bcftools/bcftools filter -r ",CHR,":",startpos,"-",endpos," -O v -o ",WORK.DIR,CHR,"_",startpos,"_",endpos,".vcf ",VCF))
if(prephasing){
system(paste0("/mnt/isilonX200/qnerv/NGSapp/SHAPEIT2/bin/shapeit --input-vcf ",WORK.DIR,CHR,"_",startpos,"_",endpos,".vcf --output-max ",WORK.DIR,CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
system(paste0("/mnt/isilonX200/qnerv/NGSapp/SHAPEIT2/bin/shapeit -convert --input-haps ",WORK.DIR,CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf --output-vcf ",WORK.DIR,CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
}
#legend
#tmp.info<-READ.VCF(paste0("/home/qnerv/tmp/",CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
system(paste0("/mnt/isilonX200/qnerv/vcftools_0.1.12b/bin/vcftools --vcf ",WORK.DIR,CHR,"_",startpos,"_",endpos,".vcf --extract-FORMAT-info GT --out ",WORK.DIR,CHR,"_",startpos,"_",endpos))
tmp.data<-READ.VCF(paste0(WORK.DIR,CHR,"_",startpos,"_",endpos,"_shapeit2phased.vcf"))
tmp.info<-tmp.data[,c(3,2,4,5)]
colnames(tmp.info)<-c("rs","position","0","1")
write.table(tmp.info,paste0(WORK.DIR,CHR,"_",startpos,"_",endpos,".legend"),row.names=F,quote=F,sep="\t")
DATADist<-as.numeric(tmp.info$position)
DATASNPNAME<-tmp.info$rs
DATASNP<-tmp.data[,10:ncol(tmp.data)]
#DATASNP<-read.table(paste0(WORK.DIR,CHR,"_",startpos,"_",endpos,".GT.FORMAT"),head=TRUE,stringsAsFactors=FALSE)
#DATASNP<-DATASNP[,-c(1:2)]
cat("Parse data to ATCG\n")
for(i in 1:nrow(tmp.info)){
DATASNP[i,]<-gsub("0",tmp.info[i,3],DATASNP[i,])  
DATASNP[i,]<-gsub("1",tmp.info[i,4],DATASNP[i,]) 
}
DATASNP<-t(DATASNP)
DATASNP.LD<-as.data.frame(matrix(ncol=ncol(DATASNP),nrow=nrow(DATASNP)))
cat("Transform genotype object\n")
for(i in 1:ncol(DATASNP)){
DATASNP.LD[,i]<-genotype(gsub("\\|","/",DATASNP[,i]))
}
cat("Plot LD heatmap\n")
png(paste0(WORK.DIR,CHR,"_",startpos,"_",endpos,".png"),width = 960, height = 960, units = "px", pointsize = 24)
ll<-LDheatmap(DATASNP.LD, DATADist, LDmeasure = "r",title = "Pairwise LD in r^2", add.map = TRUE, name = "myLDgrob", add.key = TRUE,flip=TRUE,color=heat.colors(20))
dev.off()
}





