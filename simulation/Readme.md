# **<font color=#808080>Simulation Implement</font>**
## ***<font color=#800080>環境需求(實測套件版本)</font>***
- **R version 3.4.3**
- **quantreg 5.34**
- **hdm 0.2.0**
- **hqreg 1.4**
- **mvtnorm 1.0.6**
- **doSNOW 1.0.16**
## ***<font color=#800080>1. Define fuction</font>***
## *<font color=#0000FF>1.1 IVQR  GMM without Partiailing out and with Partiailing out</font>*
**以下對應到論文Table 1衡量有做Partiailing out與沒做Partiailing out的Function，其中alpha的Grid是設在-1到3之間切41個點，實證會把alpa的Grid的區間放到function input裡面，這裡funtion output 為Grid在GMM裡的最小值(即為我們Casual parameter的估計)**

- **Function Input**
    - y ====> Outcome variable
    - D ====> Casual variable
    - X ====> Control variable
    - Z ====> Insturmental variable
    - tau ===> Quantile percentile 
- **Function Output**
    - param1 ====> Min of the GMM function in the grid( coefficient of Casual variable)
```r=
#GMM without Partiailing out Z on X
nongmm_quantile<-function(y,D,X,Z,tau){              
  
  alpha=seq(-1,3,length=41)        #grid 在-1到3之間切41個點
  gmm=rep(0,length(alpha))         #設一個0向量裝grid個個loss
  for (i in 1:length(alpha)) {     
    beta<- rq(y-(alpha[i]*D) ~ X, tau = tau)    #算control的係數
    beta=matrix(beta$coefficients,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)
    psi=t(Z)
    indicator=ifelse(e<=0,1,0)
    g=(psi%*%(tau-indicator))      #估paper (3)式，此為無Partiailing out Z on X
    invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
    gmm[i]=(t(g)%*%invsigma%*%g)   #paper(7)式，為GMM Loss
    i=i+1
  }  
  I=which.min(gmm)
  param1=alpha[I]
  return(param1)                   #return GMM Loss最小的alpha
}
```
**下面with Partiailing out Z on X的function 只差在paper(3)式，流程跟上面一樣**
```r=
#GMM with Partiailing out Z on X
gmm_quantile<-function(y,D,X,Z,tau){
  alpha=seq(-1,3,length=41)
  gmm=rep(0,length(alpha))
  
  for (i in 1:length(alpha)) {
    beta<- rq(y-(alpha[i]*D) ~ X, tau = tau) 
    beta=matrix(beta$coefficients,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)
    distribition=c(dnorm(e,mean(e),var(e)))   #此開始做(6)式，因模擬Kernal用已知分配
    distribition=diag(distribition)
    M=t(Z)%*%distribition%*%X      
    J=t(X)%*%distribition%*%X
    delta=M%*%solve(J)
    psi=t(Z)-delta%*%t(X)                     #加權完做Partiailing out Z on X 
    
    indicator=ifelse(e<=0,1,0)
    g=(psi%*%(tau-indicator))
    invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
    gmm[i]=(t(g)%*%invsigma%*%g)
    i=i+1
  }  
  I=which.min(gmm)
  param1=alpha[I]
  return(param1)                     
}
```
## *<font color=#0000FF>1.2 IVQR GMM under High-dimensional Control</font>*

**hdm_quantile對應到table 2 DML-IVQR這個function，在模擬中quantile L1-norm使用的是Huber loss的近似，實證中才會使用quantreg l1-norm，與上述Partiailing out Z on X的差別只有在Y-alpha*D對Control一個是做quantile regression一個是做 quantile L1-norm，以及加權的Z及X最回歸這裡變成Lasso**

```r=
hdm_quantile<-function(y,D,X,Z,tau){
  alpha=seq(-1,3,length=41)
  gmm=rep(0,length(alpha))
  for (i in 1:length(alpha)) {                #cv.hqreg中為quamtile l1-norm,nfold取5
    lasso=cv.hqreg(X,y-alpha[i]*D,method=c("quantile"),tau=tau,FUN = c("hqreg"),nfolds = 5,type.measure = c("mae"))
    cv.beta=as.matrix(lasso$fit$beta)
    kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
    kfold.beta=cv.beta[,kfold]                #取出最最適的penalty中的control係數
    beta=matrix(kfold.beta,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)       #用取出的係數算殘差
    distribition=c(dnorm(e,mean(e),var(e)))   #(8)式中的Kernal，因模擬Kernal用已知分配
    distribition=diag(distribition)
    distribition=sqrt(distribition)
    psi=matrix(0,nrow = length(Z[1,]),ncol=length(Z[,1]))
    for (j in 1:length(Z[1,])) {              #對加權完的Z跟X做lasso
      delta=rlasso(distribition%*%Z[,j] ~ distribition%*%X, post = FALSE)
      delta=matrix(delta$coefficients,ncol=1)
      delta=Z[,j]-cbind(1,X)%*%delta
      psi[j,]=t(delta)
      j=j+1
    }
    indicator=ifelse(e<=0,1,0)
    g=(psi%*%(tau-indicator))
    invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
    gmm[i]=(t(g)%*%invsigma%*%g)
    i=i+1
  }
  I=which.min(gmm)
  return(alpha[I])
}

```

## *<font color=#0000FF>1.3 Evalaute RMSE and MAE fuction </font>*
 
**以上，我們已經定義完模擬中最主要的function，在實證的部分中HDM fuction會在微調，這裡在模擬中加上最後一個function為衡量rmse 以及mae。**
```r=
#Calculate rmse or mae
bias <- function(x,tau,sample_size,name="RMSE"){
  true_para=1+qnorm(tau)
  if (name == "RMSE") {
    mse=sqrt(sum((x-true_para)^2)/sample_size)
  }
  else{
    mse=sum(abs(x-true_para))/sample_size
  }
  return(mse)
}
```
## ***<font color=#800080>2. Simulation Implement</font>***
## *<font color=#0000FF>2.1 Generate simulation data </font>*
**此段的code需要跟下一段的code合併再一起，在此為方便說明而將其分開。在此我們需要doSNOW這個library幫我們平行化，這裡NumberOfCluster <-8為使用8個線程(建議使用1)，此外要把每個要執行的process寫在foreach裡(類似for的概念)，lrbrary包也需要寫在for each裡，data generation參照paper中的式子，其中x有100個變數，對y有七個ｘ變數有影響，係數為5，此外x有七個變數對Z1,Z2有影響，係數為1(這裡工具變數只使用兩個)，這裡有90個x為多餘變數，D為我們要估的因果變數，參數為$\alpha$, 其中**
$$
\begin{align}
\alpha=F^{-1}_{\epsilon}(\tau)
\end{align}
$$



```r=
set.seed(2019)
getwd()
library(doSNOW)
iter=500                                                           #模擬數為500 
sample_size=500                                                    #樣本數為500
NumberOfCluster <-8                                                #使用8個線呈做平行運算
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
ptm <- proc.time()
sample500 <-foreach(i = 1:iter, .combine = "rbind") %dopar%{       #寫法類似迴圈
  library(quantreg)
  library(hdm)
  library(hqreg)
  library(mvtnorm)
  #Data generation process
  n = sample_size
  p = 100                                                          #p為100個變數
  s=7                                                              #對y有影響的x有7個
  sigma <- matrix(c(1,0.3,0.3,1), ncol=2)                          #covariance matrix
  epsilon<-rmvnorm(n=n, mean=c(0,0), sigma=sigma)                  #殘差分配為多變量常態
  x= matrix(rnorm(n * p), ncol = p) 
  X=matrix(pnorm(x),ncol = p)
  z<-matrix(cbind(rnorm(n,0,1),rnorm(n,0,1)),ncol = 2)
  d<-z[,1]+z[,2]+epsilon[,2]                                       #d為Z+殘差，且有內生性
  D<-pnorm(d)
  Z1<-z[,1]+rnorm(n,0,1)+X[,2]+X[,3]+X[,4]                         #Z受少部分X影響
  Z2<-z[,2]+rnorm(n,0,1)+X[,7]+X[,8]+X[,9]+X[,10]
  Z<-matrix(cbind(Z1,Z2),nrow = n)
  b = matrix(c(rep(5, s), rep(0, p - s)))
  X1=X[,c(1:10)]
  y=1+D+X%*%b+(epsilon[,1]*D)                                      #生成Y,其中90個X為無關變數
```
**以下變數名稱為對應到論文中的表名稱**
+ <font color=#FF6600>exact =======>Table 1 po-GMM及Table 2 exact-GMM</font>
+ <font color=#FF6600>nonexact=====>Table 1 GMM</font>
+ <font color=#FF6600>fullgmm =====>Table 2 GMM</font>
+ <font color=#FF6600>hdm =======> Table 2    DML-IVQR</font>


```r=
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

```

**meth輸出為各個估計下的點估計值，[點此meth在sample 500下的結果](https://github.com/FieldTien/DML-QR/blob/master/simulation/result_montecarlo500.csv)**

## *<font color=#0000FF>2.2 Evaluate RMSE and MAE </font>*

```r=
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
```

**輸出csv file為各個估計下的點RMSE及MAE，[點此為sample 500下的結果](https://github.com/FieldTien/DML-QR/blob/master/simulation/result_sample500.csv)，這裡對應到論文中的Table 1以及Table 2的結果**