

set.seed(2019)
getwd()
source('fun_callback.R')
library(doSNOW)
iter=500
sample_size=500
NumberOfCluster <-8
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
ptm <- proc.time()
sample500 <-foreach(i = 1:iter, .combine = "rbind") %dopar%{
  library(quantreg)
  library(hdm)
  library(hqreg)
  library(mvtnorm)
  #Data generation process
  n = sample_size
  p = 100
  s=7 
  sigma <- matrix(c(1,0.3,0.3,1), ncol=2)
  epsilon<-rmvnorm(n=n, mean=c(0,0), sigma=sigma)
  x= matrix(rnorm(n * p), ncol = p) 
  X=matrix(pnorm(x),ncol = p)
  z<-matrix(cbind(rnorm(n,0,1),rnorm(n,0,1)),ncol = 2)
  d<-z[,1]+z[,2]+epsilon[,2]
  D<-pnorm(d)
  Z1<-z[,1]+rnorm(n,0,1)+X[,2]+X[,3]+X[,4]
  Z2<-z[,2]+rnorm(n,0,1)+X[,7]+X[,8]+X[,9]+X[,10]
  Z<-matrix(cbind(Z1,Z2),nrow = n)
  b = matrix(c(rep(5, s), rep(0, p - s)))
  X1=X[,c(1:10)]
  y=1+D+X%*%b+(epsilon[,1]*D)
  
  exact10<-gmm_quantile(y,D,X1,Z,tau = 0.1)
  nonexact10<-nongmm_quantile(y,D,X1,Z,tau = 0.1)
  fullgmm10<-gmm_quantile(y,D,X,Z,tau = 0.1)
  hdm10<-hdm_quantile(y,D,X,Z,tau = 0.1)
  exact25<-gmm_quantile(y,D,X1,Z,tau = 0.25)
  nonexact25<-nongmm_quantile(y,D,X1,Z,tau = 0.25)
  fullgmm25<-gmm_quantile(y,D,X,Z,tau = 0.25)
  hdm25<-hdm_quantile(y,D,X,Z,tau = 0.25)
  exact50<-gmm_quantile(y,D,X1,Z,tau = 0.5)
  nonexact50<-nongmm_quantile(y,D,X1,Z,tau = 0.5)
  fullgmm50<-gmm_quantile(y,D,X,Z,tau = 0.5)
  hdm50<-hdm_quantile(y,D,X,Z,tau = 0.5)
  exact75<-gmm_quantile(y,D,X1,Z,tau = 0.75)
  nonexact75<-nongmm_quantile(y,D,X1,Z,tau = 0.75)
  fullgmm75<-gmm_quantile(y,D,X,Z,tau = 0.75)
  hdm75<-hdm_quantile(y,D,X,Z,tau = 0.75)
  exact90<-gmm_quantile(y,D,X1,Z,tau = 0.9)
  nonexact90<-nongmm_quantile(y,D,X1,Z,tau = 0.9)
  fullgmm90<-gmm_quantile(y,D,X,Z,tau = 0.9)
  hdm90<-hdm_quantile(y,D,X,Z,tau = 0.9)
  
  meth<-cbind(exact10,nonexact10,fullgmm10,hdm10,exact25,nonexact25,fullgmm25,hdm25,exact50,nonexact50,fullgmm50,hdm50,exact75,nonexact75,fullgmm75,hdm75,exact90,nonexact90,fullgmm90,hdm90)
  return(meth)
}

stopCluster(cl)
proc.time() - ptm


quntile=c(0.1,0.25,0.5,0.75,0.9)
name=c(10,25,50,75,90)
item=c("exact","nonexact","fullgmm","hdm")
result=data.frame(t(c("item","rmse","mae")))
for (i in item) {
  for (j in 1:5) {
    qname=paste(i,sep="",name[j])
    mae=bias(sample500[,qname],quntile[j],iter,name="mae")
    rmse=bias(sample500[,qname],quntile[j],iter)
    it=data.frame(t(c(qname,rmse,mae)))
    result=rbind.data.frame(result,it)
    
  }
  
}
filename=paste("result_sample",sep="",sample_size)
filename=paste(filename,sep="",".csv")
write.table(result, sep = ",",col.names = FALSE,row.names = FALSE,file = filename)
  