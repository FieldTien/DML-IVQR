hdm_parallel<-function(y,D,X,Z,tau,alphaint,core){
  cl <- makeCluster(core)
  registerDoSNOW(cl)
  A=foreach(i = alphaint, .combine = "rbind") %dopar%{
    library(hqreg)
    library(quantreg)
    library(hdm)
    lasso=cv.hqreg(X,y-i*D,method=c("quantile"),tau=tau,FUN = c("hqreg"),nfolds = 5,type.measure = c("mae"))
    cv.beta=as.matrix(lasso$fit$beta)
    kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
    kfold.beta=cv.beta[,kfold]
    beta=matrix(kfold.beta,nrow = 1)
    e=y-i*D-cbind(1,X)%*%t(beta)
    hh=sd(e)*(4/3/length(e))^(1/5)
    distribition=akj(e,z=e,h = hh)$dens
    distribition=diag(distribition)
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
    gmm=(t(g)%*%invsigma%*%g)
    return(gmm)
  } 
  stopCluster(cl)
  I=which.min(A)
  param1=alphaint[I]
  result=list(param1,A)
  return(result)
}


gmm_parallel<-function(y,D,X,Z,tau,alphaint,core){
  cl <- makeCluster(core)
  registerDoSNOW(cl)
  A=foreach(i = alphaint, .combine = "rbind") %dopar%{
    
    library(quantreg)
    beta<- rq.fit(X, y-i*D, tau = tau, method="br")
    e=beta$residuals
    hh=sd(e)*(4/3/length(e))^(1/5)
    distribition=akj(e,z=e,h = hh)$dens
    distribition=diag(distribition)
    psi=matrix(0,nrow = length(Z[1,]),ncol=length(Z[,1]))
    M=t(Z)%*%distribition%*%X
    J=t(X)%*%distribition%*%X
    delta=M%*%solve(J)
    psi=t(Z)-delta%*%t(X)
    indicator=ifelse(e<=0,1,0)
    g=(psi%*%(tau-indicator))
    invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
    gmm=(t(g)%*%invsigma%*%g)
    return(gmm)
  } 
  stopCluster(cl)
  I=which.min(A)
  param1=alphaint[I]
  result=list(param1,A)
  return(result)
}


non_gmm_parallel<-function(y,D,X,Z,tau,alphaint,core){
  cl <- makeCluster(core)
  registerDoSNOW(cl)
  A=foreach(i = alphaint, .combine = "rbind") %dopar%{
    
    library(quantreg)
    beta<-  rq.fit(X, y-i*D, tau = tau, method="br")
    e=beta$residuals
    hh=sd(e)*(4/3/length(e))^(1/5)
    distribition=akj(e,z=e,h = hh)$dens
    distribition=diag(distribition)
    psi=t(Z)
    indicator=ifelse(e<=0,1,0)
    g=(psi%*%(tau-indicator))
    invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
    gmm=(t(g)%*%invsigma%*%g)
    return(gmm)
  } 
  stopCluster(cl)
  I=which.min(A)
  param1=alphaint[I]
  result=list(param1,A)
  return(result)
}