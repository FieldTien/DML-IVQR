
hdm_rq_parallel<-function(y,D,X,Z,tau,alphaint,penal_interv,core){
    cl <- makeCluster(core)
    registerDoSNOW(cl)
    A=foreach(i = alphaint, .combine = "rbind") %dopar%{
        library(hqreg)
        library(quantreg)
        library(hdm)
      avoid_singular<-function(train_y,train_x,valid_y,valid_x,tau,penalty_point,initial_penalty)
      {
        if ((penalty_point-initial_penalty)>0.15){
          mad<-10^9
          result_pe<-list(mad,penalty_point)
          names(result_pe) <- c("valid_value","penalty_point")
          return(result_pe)
        }
        fit <- try(rq(train_y ~ train_x, method="lasso",tau = tau,lambda = penalty_point))
        if (mode(fit)=="list")
        {
          y_hat <- cbind(1,valid_x)%*%matrix(fit$coefficients)
          mad <- sum(abs(valid_y-y_hat))/(dim(valid_x)[1]) 
          result_pe<-list(mad,penalty_point)
          names(result_pe) <- c("valid_value","penalty_point")
          return(result_pe)
        }
        else{
          avoid_singular(train_y,train_x,valid_y,valid_x,tau,penalty_point+0.01,initial_penalty)
        }
        
      }
      rq_lasso_tune_penalty<-function(Y,X,tau,panalty_interval)
      {
        random_shuffle=sample(rep(1:dim(X)[1]))
        X = X[random_shuffle,]
        Y = matrix(Y[random_shuffle,])
        dimen = as.integer(dim(X)[1]/5)
        valid_x = X[1:dimen,]
        valid_y = matrix(Y[1:dimen,])
        train_x = X[(dimen+1):dim(X)[1],]
        train_y = matrix(Y[(dimen+1):dim(X)[1],])
        penalty_valid=rep(0,length(panalty_interval))
        for (i in 1:length(panalty_interval)) 
        {
          penal<-avoid_singular(train_y,train_x,valid_y,valid_x,tau,panalty_interval[i],panalty_interval[i])
          penalty_valid[i] <- penal$valid_value
          panalty_interval[i] <- penal$penalty_point
        }
        I <- which.min(penalty_valid)
        best_penalty <- panalty_interval[I]
        result<-list(penalty_valid,panalty_interval,best_penalty)
        names(result) <- c("penalty_valid","panalty_interval","best_penalty")
        return(result) 
      }
        
        lasso=rq_lasso_tune_penalty(y-i*D,X,tau=tau,penal_interv)
        optim_penal=lasso$best_penalty
        fitting <- try(rq(y-i*D ~ X, method="lasso",tau = tau,lambda = optim_penal))
        if (mode(fitting)=="list")
        {
            
            beta=matrix(fitting$coefficients)
            e=y-i*D-cbind(1,X)%*%beta
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
        }
        else
        {
            
            gmm=10^9
        }
        return(gmm)
        
    } 
    stopCluster(cl)
    I=which.min(A)
    param1=alphaint[I]
    result=list(param1,A,alphaint)
    names(result) <- c("QTE","Grid_gmm_loss","Grid_value")
    return(result)
}

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

avoid_singular<-function(train_y,train_x,valid_y,valid_x,tau,penalty_point,initial_penalty)
{
  if ((penalty_point-initial_penalty)>0.15){
    mad<-10^9
    result_pe<-list(mad,penalty_point)
    names(result_pe) <- c("valid_value","penalty_point")
    return(result_pe)
  }
  fit <- try(rq(train_y ~ train_x, method="lasso",tau = tau,lambda = penalty_point))
  if (mode(fit)=="list")
  {
    y_hat <- cbind(1,valid_x)%*%matrix(fit$coefficients)
    mad <- sum(abs(valid_y-y_hat))/(dim(valid_x)[1]) 
    result_pe<-list(mad,penalty_point)
    names(result_pe) <- c("valid_value","penalty_point")
    return(result_pe)
  }
  else{
    avoid_singular(train_y,train_x,valid_y,valid_x,tau,penalty_point+0.01,initial_penalty)
  }
  
}
rq_lasso_tune_penalty<-function(Y,X,tau,panalty_interval)
{
  random_shuffle=sample(rep(1:dim(X)[1]))
  X = X[random_shuffle,]
  Y = matrix(Y[random_shuffle,])
  dimen = as.integer(dim(X)[1]/5)
  valid_x = X[1:dimen,]
  valid_y = matrix(Y[1:dimen,])
  train_x = X[(dimen+1):dim(X)[1],]
  train_y = matrix(Y[(dimen+1):dim(X)[1],])
  penalty_valid=rep(0,length(panalty_interval))
  for (i in 1:length(panalty_interval)) 
  {
    penal<-avoid_singular(train_y,train_x,valid_y,valid_x,tau,panalty_interval[i],panalty_interval[i])
    penalty_valid[i] <- penal$valid_value
    panalty_interval[i] <- penal$penalty_point
  }
  I <- which.min(penalty_valid)
  best_penalty <- panalty_interval[I]
  result<-list(penalty_valid,panalty_interval,best_penalty)
  names(result) <- c("penalty_valid","panalty_interval","best_penalty")
  return(result) 
}
seleted_nonzero <- function(y,x,tau,penal)
{
  tune=rq_lasso_tune_penalty(y,x,tau,penal)
  lam=tune$best_penalty
  fit=rq(y ~ x, method="lasso",tau = tau,lambda = lam)
  coeff<-matrix(fit$coefficients)
  nonzero_index=which(abs(coeff) > 10^-5)
  print(coeff[nonzero_index])
  nonzero_index=nonzero_index-1 
  if (nonzero_index[1]==0)
  {
    nonzero_index=nonzero_index[-1]
  }
  return(colnames(x)[nonzero_index])
}


hqreg_seleted_nonzero <- function(y,x,tau)
{
  lasso=cv.hqreg(x,y,method=c("quantile"),tau=tau,FUN = c("hqreg"),nfolds = 5,type.measure = c("mae"))
  cv.beta=as.matrix(lasso$fit$beta)
  kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
  kfold.beta=cv.beta[,kfold]
  beta=matrix(kfold.beta,nrow = 1)
  nonzero_index=which(abs(beta) > 10^-5)
  nonzero_index=nonzero_index-1 
  if (nonzero_index[1]==0)
  {
    nonzero_index=nonzero_index[-1]
  }
  return(colnames(x)[nonzero_index])
}
