#HDM 
hdm_quantile<-function(y,D,X,Z,tau){
  alpha=seq(-1,3,length=41)
  gmm=rep(0,length(alpha))
  for (i in 1:length(alpha)) {
    lasso=cv.hqreg(X,y-alpha[i]*D,method=c("quantile"),tau=tau,FUN = c("hqreg"),nfolds = 5,type.measure = c("mae"))
    cv.beta=as.matrix(lasso$fit$beta)
    kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
    kfold.beta=cv.beta[,kfold]
    beta=matrix(kfold.beta,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)
    distribition=c(dnorm(e,mean(e),var(e)))
    distribition=diag(distribition)
    distribition=sqrt(distribition)
    psi=matrix(0,nrow = length(Z[1,]),ncol=length(Z[,1]))
    for (j in 1:length(Z[1,])) {
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

#GMM with Partiailing out Z on X
gmm_quantile<-function(y,D,X,Z,tau){
  
  alpha=seq(-1,3,length=41)
  
  gmm=rep(0,length(alpha))
  
  for (i in 1:length(alpha)) {
    beta<- rq(y-(alpha[i]*D) ~ X, tau = tau) 
    beta=matrix(beta$coefficients,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)
    distribition=c(dnorm(e,mean(e),var(e)))
    distribition=diag(distribition)
    M=t(Z)%*%distribition%*%X
    J=t(X)%*%distribition%*%X
    delta=M%*%solve(J)
    psi=t(Z)-delta%*%t(X)
    
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

#GMM without Partiailing out Z on X
nongmm_quantile<-function(y,D,X,Z,tau){
  
  alpha=seq(-1,3,length=41)
  
  gmm=rep(0,length(alpha))
  
  for (i in 1:length(alpha)) {
    beta<- rq(y-(alpha[i]*D) ~ X, tau = tau) 
    beta=matrix(beta$coefficients,nrow = 1)
    e=y-alpha[i]*D-cbind(1,X)%*%t(beta)
    psi=t(Z)
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