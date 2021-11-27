#hypergeometric p-value
Enrichment.pvalue<-function(s,S,n,N){
#s:successes in the sampling
#S:total number of sampling
#n:successes in the pool
#N:total number of pool
return(1-phyper(s-1,n,N-S,S))
}