library(quantreg)
library(hdm)
library(hqreg)
library(mvtnorm)
library(doSNOW)


iter=500
sample_size=1000 
set.seed(675)
getwd()
Node=5



cv_qr_penalty<-function(y,X,tau=tau,grid,kfold){
    valid_fold=rep(0,kfold)
    sample_size=dim(y)[1]
    index=sample(rep(1:sample_size))
    grid_mae=matrix(0,ncol=length(grid),nrow=kfold)
    for (cf in 1:kfold) {
        out_index=index[(((cf-1)/kfold)*sample_size+1):(cf/kfold*sample_size)]
        y_out = matrix(y[out_index]);X_out = X[out_index,]
        start=0
        for (out in 1:kfold) {
            if(out==cf){
                next
            }
            in_index=index[(((out-1)/kfold)*sample_size+1):(out/kfold*sample_size)]
            if(start==0){
                y_in = matrix(y[in_index]);X_in = X[in_index,]
                start=1
            }
            else{
                y_in = rbind(y_in,matrix(y[in_index]));X_in = rbind(X_in,X[in_index,])
            }
        }
        for (i in 1:length(grid)) {
            fit=rq(y_in ~ X_in,tau=tau, method="lasso",lambda = grid[i])
            beta=matrix(fit$coefficients,ncol = 1)
            e1=sum(abs(y_out-cbind(1,X_out)%*%beta))/dim(y)[1]
            grid_mae[cf,i]=e1
        }
    }
    grid_mae=colSums (grid_mae, na.rm = FALSE, dims = 1)/kfold
    I=which.min(grid_mae)
    return(grid[I])
        
}

gmm<-function(y,D,X,Z,tau,alphaint,core){
    cl <- makeCluster(core)
    registerDoSNOW(cl)
    A=foreach(i = alphaint, .combine = "rbind") %dopar%{
        
        library(quantreg)
        beta<- rq(y-i*D ~ X, tau = tau) 
        beta=matrix(beta$coefficients,nrow = 1)
        e=y-i*D-cbind(1,X)%*%t(beta)
        
        distribition=c(dnorm(e,mean(e),var(e)))   #(8)─ㄴずKernal좥�]소응KernalΞ쨢ぞㅐ컏
        distribition=diag(distribition)
        distribition=sqrt(distribition)
#        hh=sd(e)*(4/3/length(e))^(1/5)
#        distribition=akj(e,z=e,h = hh)$dens
#        distribition=diag(distribition)
        psi=matrix(0,nrow = length(Z[1,]),ncol=length(Z[,1]))
        for (j in 1:length(Z[1,])) {
            delta=lm(distribition%*%Z[,j] ~ distribition%*%X)
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
#    result=list(param1,A)
    return(param1)
}

hdm_naive<-function(y,D,X,Z,tau,alphaint,core,l1_norm=TRUE,POST=TRUE,penalty=TRUE){                        #l1_norm TRUE=hqreg False=quantreg
    cl <- makeCluster(core)
    registerDoSNOW(cl)
    A=foreach(i = alphaint, .combine = "rbind") %dopar%{
        library(hqreg)
        library(quantreg)
        library(hdm)
        
        
        make_index<-function(beta){
            ligic=rep(FALSE,dim(beta)[1]-1)
            for (i in 2:dim(beta)[1]) {
                if(abs(beta[i])>0.000001){
                    ligic[i-1]=TRUE
                }
            }
            return(ligic)
        }
        Select<-function(X,index){
            nonselct=TRUE
            for(i in 1:dim(X)[2]){
                if(index[i]==TRUE){
                    if(nonselct==TRUE){
                        new_x=matrix(X[i,])
                        nonselct=FALSE
                    }
                    else{
                        new_x=cbind(new_x,matrix(X[i,]))
                    }
                }
            }
            if(nonselct==TRUE){
                return(matrix(0))
            }
            else{
                return(new_x)
            }
        }
        if(l1_norm == TRUE){
            lasso=cv.hqreg(X,y-i*D,method=c("quantile"),tau=tau,FUN = c("hqreg"),nlambda = 50,lambda=seq(0.1,0.001,length.out = 50),nfolds = 5,type.measure = c("mae"),seed = 2021)
            cv.beta=as.matrix(lasso$fit$beta)
            kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
            kfold.beta=cv.beta[,kfold]
            beta=matrix(kfold.beta,ncol = 1)
            if(POST==TRUE){
                new_x=Select(X,make_index(beta))
                if(dim(new_x)[1] != dim(X)[1]){
                    e=y-i*D-cbind(1,X)%*%beta
                }
                else{
                    beta=rq(y-i*D ~ new_X, tau = tau) 
                    beta=matrix(beta$coefficients,ncol = 1)
                    e=y-i*D-cbind(1,new_X)%*%beta
                }
                
            }
            else{
                e=y-i*D-cbind(1,X)%*%beta
            }
        }
        else{
            norm2n<- function(z){  sqrt(mean(z^2)) }
            cv_qr_penalty<-function(y,X,tau=tau,grid,kfold){
                valid_fold=rep(0,kfold)
                sample_size=dim(y)[1]
                index=sample(rep(1:sample_size))
                grid_mae=matrix(0,ncol=length(grid),nrow=kfold)
                for (cf in 1:kfold) {
                    out_index=index[(((cf-1)/kfold)*sample_size+1):(cf/kfold*sample_size)]
                    y_out = matrix(y[out_index]);X_out = X[out_index,]
                    start=0
                    for (out in 1:kfold) {
                        if(out==cf){
                            next
                        }
                        in_index=index[(((out-1)/kfold)*sample_size+1):(out/kfold*sample_size)]
                        if(start==0){
                            y_in = matrix(y[in_index]);X_in = X[in_index,]
                            start=1
                        }
                        else{
                            y_in = rbind(y_in,matrix(y[in_index]));X_in = rbind(X_in,X[in_index,])
                        }
                    }
                    for (i in 1:length(grid)) {
                        fit=rq(y_in ~ X_in,tau=tau, method="lasso",lambda = grid[i])
                        beta=matrix(fit$coefficients,ncol = 1)
                        e1=sum(abs(y_out-cbind(1,X_out)%*%beta))/dim(y)[1]
                        grid_mae[cf,i]=e1
                    }
                }
                grid_mae=colSums (grid_mae, na.rm = FALSE, dims = 1)/kfold
                I=which.min(grid_mae)
                return(grid[I])
                
            }
            lambda.BC<- function(X, R = 1000, tau = 0.5, c = 2, alpha = .1){
                n <- nrow(X)
                sigs <- apply(X,2,norm2n)
                U <- matrix(runif(n * R),n)
                R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
                r <- apply(abs(R),2,max)
                c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
            }
            if(penalty == TRUE){
                lasso=rq(y-i*D ~ X,tau=tau, method="lasso",lambda = lambda.BC(X,tau=tau,c=2, alpha=0.1))
            }
            else{
                lasso=rq(y-i*D ~ X,tau=tau, method="lasso",lambda = cv_qr_penalty(y-i*D,X,tau=tau,seq(0,20,length=11),5))
            }
            
            beta=matrix(lasso$coefficients,ncol = 1)
            if(POST==TRUE){
                new_x=Select(X,make_index(beta))
                if(dim(new_x)[1] != dim(X)[1]){
                    e=y-i*D-cbind(1,X)%*%beta
                }
                else{
                    beta=rq(y-i*D ~ new_X, tau = tau) 
                    beta=matrix(beta$coefficients,ncol = 1)
                    e=y-i*D-cbind(1,new_X)%*%beta
                }
                new_X=X[,which(abs(beta)>0.000001)-1]
                beta=rq(y-i*D ~ new_X, tau = tau) 
                beta=matrix(beta$coefficients)
                e=y-i*D-cbind(1,new_X)%*%beta
            }
            else{
                e=y-i*D-cbind(1,X)%*%beta
            }
        }
        distribition=c(dnorm(e,mean(e),var(e)))   #(8)─ㄴずKernal좥�]소응KernalΞ쨢ぞㅐ컏
        distribition=diag(distribition)
        distribition=sqrt(distribition)
        #hh=sd(e)*(4/3/length(e))^(1/5)
        #distribition=akj(e,z=e,h = hh)$dens
        #distribition=diag(distribition)
        psi=matrix(0,nrow = length(Z[1,]),ncol=length(Z[,1]))
        for (j in 1:length(Z[1,])) {
            if(POST==TRUE){
                delta=rlasso(distribition%*%Z[,j] ~ distribition%*%X, post = TRUE)
            }
            else{
                delta=rlasso(distribition%*%Z[,j] ~ distribition%*%X, post = FALSE)
            }
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
    #result=list(param1,A)
    return(param1)
}





hdm_crossfit<-function(y,D,X,Z,tau,alphaint,crossfold,core,l1_norm=TRUE,POST=TRUE){
    alpha_fold=rep(0,crossfold)
    sample_size=dim(y)[1]
    index=sample(rep(1:sample_size))
    for (cf in 1:crossfold) {
        out_index=index[(((cf-1)/crossfold)*sample_size+1):(cf/crossfold*sample_size)]
        y_out = matrix(y[out_index]);D_out = matrix(D[out_index]);X_out = X[out_index,];Z_out = Z[out_index,]
        
        
        start=0
        for (out in 1:crossfold) {
            if(out==cf){
                next
            }
            in_index=index[(((out-1)/crossfold)*sample_size+1):(out/crossfold*sample_size)]
            if(start==0){
                y_in = matrix(y[in_index]);D_in = matrix(D[in_index]);X_in = X[in_index,];Z_in = Z[in_index,]
                start=1
            }
            else{
                y_in = rbind(y_in,matrix(y[in_index]));D_in = rbind(D_in,matrix(D[in_index]));X_in = rbind(X_in,X[in_index,]);Z_in = rbind(Z_in,Z[in_index,])
                
            }
        }
        cl <- makeCluster(core)
        registerDoSNOW(cl)
        A=foreach(i = alphaint, .combine = "rbind") %dopar%{
            library(hqreg)
            library(quantreg)
            library(hdm)
            make_index<-function(beta){
                ligic=rep(FALSE,dim(beta)[1]-1)
                for (i in 2:dim(beta)[1]) {
                    if(abs(beta[i])>0.000001){
                        ligic[i-1]=TRUE
                    }
                }
                return(ligic)
            }
            Select<-function(X,index){
                nonselct=TRUE
                for(i in 1:dim(X)[2]){
                    if(index[i]==TRUE){
                        if(nonselct==TRUE){
                            new_x=matrix(X[i,])
                            nonselct=FALSE
                        }
                        else{
                            new_x=cbind(new_x,matrix(X[i,]))
                        }
                    }
                }
                if(nonselct==TRUE){
                    return(matrix(0))
                }
                else{
                    return(new_x)
                }
            }
            if(l1_norm == TRUE){
                lasso=cv.hqreg(X_in,y_in-i*D_in,method=c("quantile"),tau=tau,FUN = c("hqreg"),nlambda = 50,lambda=seq(0.1,0.001,length.out = 50),nfolds = 5,type.measure = c("mae"),seed = 2021)
                cv.beta=as.matrix(lasso$fit$beta)
                kfold=which(lasso$lambda==lasso$lambda.min, arr.ind=T )
                kfold.beta=cv.beta[,kfold]
                beta=matrix(kfold.beta,ncol = 1)
                
                if(POST==TRUE){
                    new_X_in=Select(X_in,make_index(beta))
                    new_X_out=Select(X_out,make_index(beta))
                    if(dim(new_X_in)[1] != dim(X)[1]){
                        e_out=y_out-i*D_out-cbind(1,X_out)%*%beta
                        e_in=y_in-i*D_in-cbind(1,X_in)%*%beta
                    }
                    else{
                        beta=rq(y_out-i*D_out ~ new_X_out, tau = tau) 
                        beta=matrix(beta$coefficients,ncol = 1)
                        e_out=y_out-i*D_out-cbind(1,new_X_out)%*%beta
                        e_in=y_in-i*D_in-cbind(1,new_X_in)%*%beta
                    }
                    

                }
                else{
                    e_out=y_out-i*D_out-cbind(1,X_out)%*%beta
                    e_in=y_in-i*D_in-cbind(1,X_in)%*%beta
                }
            }
            else{
                norm2n<- function(z){  sqrt(mean(z^2)) }
                lambda.BC<- function(X, R = 1000, tau = 0.5, c = 2, alpha = .1){
                    n <- nrow(X)
                    sigs <- apply(X,2,norm2n)
                    U <- matrix(runif(n * R),n)
                    R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
                    r <- apply(abs(R),2,max)
                    c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
                }
                lasso=rq(y_in-i*D_in ~ X_in,tau=tau, method="lasso",lambda = lambda.BC(X,tau=tau,c=2, alpha=0.1))
                beta=matrix(lasso$coefficients,ncol = 1)
                if(POST==TRUE){
                    new_X_in=Select(X_in,make_index(beta))
                    new_X_out=Select(X_out,make_index(beta))
                    if(dim(new_X_in)[1] != dim(X)[1]){
                        e_out=y_out-i*D_out-cbind(1,X_out)%*%beta
                        e_in=y_in-i*D_in-cbind(1,X_in)%*%beta
                    }
                    else{
                        beta=rq(y_out-i*D_out ~ new_X_out, tau = tau) 
                        beta=matrix(beta$coefficients,ncol = 1)
                        e_out=y_out-i*D_out-cbind(1,new_X_out)%*%beta
                        e_in=y_in-i*D_in-cbind(1,new_X_in)%*%beta
                    }
                    
                }
                else{
                    e_out=y_out-i*D_out-cbind(1,X_out)%*%beta
                    e_in=y_in-i*D_in-cbind(1,X_in)%*%beta
                }
            }
            distribition=sqrt(diag(c(dnorm(e_in,mean(e_in),var(e_in)))))   #(8)─ㄴずKernal좥�]소응KernalΞ쨢ぞㅐ컏
            distribition_out=sqrt(diag(c(dnorm(e_out,mean(e_out),var(e_out)))))
            #hh=sd(e_in)*(4/3/length(e_in))^(1/5)
            #distribition=akj(e_in,z=e_in,h = hh)$dens
            #distribition=diag(distribition)
            
            psi=matrix(0,nrow = length(Z_out[1,]),ncol=length(Z_out[,1]))
            for (j in 1:length(Z_out[1,])) {
                if(POST==TRUE){
                    delta=rlasso(distribition%*%Z_in[,j] ~ distribition%*%X_in, post = FALSE)
                    new_x=Select(X_out,delta$index)
                    if(dim(new_x)[1] != dim(X)[1]){
                        delta=matrix(delta$coefficients,ncol=1)
                        delta=Z_out[,j]-cbind(1,X_out)%*%delta
                    }
                    else{
                        delta=lm(distribition_out%*%Z_out[,j] ~ distribition_out%*%new_x)
                        delta=matrix(delta$coefficients,ncol=1)
                        delta=Z_out[,j]-cbind(1,new_x)%*%delta
                    }
                }
                else{
                    delta=rlasso(distribition%*%Z_in[,j] ~ distribition%*%X_in, post = FALSE)
                    delta=matrix(delta$coefficients,ncol=1)
                    delta=Z_out[,j]-cbind(1,X_out)%*%delta
                }
                
                
                psi[j,]=t(delta)
                j=j+1
            }
            indicator=ifelse(e_out<=0,1,0)
            g=(psi%*%(tau-indicator))
            invsigma=(solve(psi%*%diag(diag((tau-indicator)%*%t(tau-indicator)))%*%t(psi)))
            gmm=(t(g)%*%invsigma%*%g)
            return(gmm)
        } 
        stopCluster(cl)
        I=which.min(A)
        param1=alphaint[I]
        alpha_fold[cf]=param1
    }
    return(sum(alpha_fold)/crossfold)
}


for (it in 1:iter) {
    ptm <- proc.time()
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
    
    
    
    
    exact_q25=gmm(y,D,X1,Z,tau=0.25,seq(-0.5,1.5,length=21),core=Node)
    #qr_post_q25=hdm_naive(y,D,X,Z,tau=0.25,seq(-0.5,1.5,length=21),core=Node,l1_norm = FALSE,POST=TRUE)
    qr_q25=hdm_naive(y,D,X,Z,tau=0.25,seq(-0.5,1.5,length=21),core=Node,l1_norm = FALSE,POST=FALSE)
    qr_cv_q25=hdm_naive(y,D,X,Z,tau=0.25,seq(-0.5,1.5,length=21),core=Node,l1_norm = FALSE,POST=FALSE,penalty = FALSE)
    #qr_cv_post_q25=hdm_naive(y,D,X,Z,tau=0.25,seq(-0.5,1.5,length=21),core=Node,l1_norm = FALSE,POST=TRUE,penalty = FALSE)
    
    exact_q50=gmm(y,D,X1,Z,tau=0.5,seq(0,2,length=21),core=Node)
    #qr_post_q50=hdm_naive(y,D,X,Z,tau=0.5,seq(0,2,length=21),core=Node,l1_norm = FALSE,POST=TRUE)
    qr_q50=hdm_naive(y,D,X,Z,tau=0.5,seq(0,2,length=21),core=Node,l1_norm = FALSE,POST=FALSE)
    qr_cv_q50=hdm_naive(y,D,X,Z,tau=0.5,seq(0,2,length=21),core=Node,l1_norm = FALSE,POST=FALSE,penalty = FALSE)
    #qr_cv_post_q50=hdm_naive(y,D,X,Z,tau=0.5,seq(0,2,length=21),core=Node,l1_norm = FALSE,POST=TRUE,penalty = FALSE)
    
    
    
    exact_q75=gmm(y,D,X1,Z,tau=0.75,seq(1,3,length=21),core=Node)
    #qr_post_q75=hdm_naive(y,D,X,Z,tau=0.75,seq(1,3,length=21),core=Node,l1_norm = FALSE,POST=TRUE)
    qr_q75=hdm_naive(y,D,X,Z,tau=0.75,seq(1,3,length=21),core=Node,l1_norm = FALSE,POST=FALSE)
    qr_cv_q75=hdm_naive(y,D,X,Z,tau=0.75,seq(1,3,length=21),core=Node,l1_norm = FALSE,POST=FALSE,penalty = FALSE)
    #qr_cv_post_q75=hdm_naive(y,D,X,Z,tau=0.75,seq(1,3,length=21),core=Node,l1_norm = FALSE,POST=TRUE,penalty = FALSE)
    
    
    
    q=cbind(exact_q25,exact_q50,exact_q75,qr_q25,qr_q50,qr_q75,
            
            qr_cv_q25,qr_cv_q50,qr_cv_q75)
    
    print(c(it,proc.time() - ptm))
    
    new_est=q
    if(it == 1){
        est = new_est
    }
    else{
        est = rbind(est,new_est)
    }
}


#Calculate rmse or mae
bias <- function(x,tau,sample_size,name="RMSE"){
    true_para=1+qnorm(tau)
    if (name == "RMSE") {
        mse=sqrt(sum((x-true_para)^2)/sample_size)
    }
    else if(name == "MAE"){
        mse=sum(abs(x-true_para))/sample_size
    }
    else if(name == "BIAS"){
        mse=sum(true_para-x)/sample_size
    }
    return(mse)
}

exact_25_mae=bias(est[,1],.25,iter,name="MAE")
exact_50_mae=bias(est[,2],.5,iter,name="MAE")
exact_75_mae=bias(est[,3],.75,iter,name="MAE")

exact_25_rmse=bias(est[,1],.25,iter,name="RMSE")
exact_50_rmse=bias(est[,2],.5,iter,name="RMSE")
exact_75_rmse=bias(est[,3],.75,iter,name="RMSE")

exact_25_bias=bias(est[,1],.25,iter,name="BIAS")
exact_50_bias=bias(est[,2],.5,iter,name="BIAS")
exact_75_bias=bias(est[,3],.75,iter,name="BIAS")

result=data.frame(t(c("item","rmse","mae","BIAS","ratio_rmse","ratio_mae","ratio_bias")))
#item=c("naive_qr_post_q","naive_qr_q","naive_cv_qr_post_q","naive_cv_qr_q")
for (i in 1:dim(est)[2]) {
    if(i%%3 == 1){
        quantile=.25
        exact_mae=exact_25_mae
        exact_rmse=exact_25_rmse
        exact_bias=exact_25_bias
    }
    else if(i%%3 == 2){
        quantile=.5
        exact_mae=exact_50_mae
        exact_rmse=exact_50_rmse
        exact_bias=exact_50_bias
    }
    else if(i%%3 == 0){
        quantile=.75
        exact_mae=exact_75_mae
        exact_rmse=exact_75_rmse
        exact_bias=exact_75_bias
    }
    qname=colnames(est)[i]
    mae=bias(est[,i],quantile,iter,name="MAE")
    rmse=bias(est[,i],quantile,iter,name="RMSE")
    bais=bias(est[,i],quantile,iter,name="BIAS")
    ratio_mae=mae/exact_mae
    ratio_rmse=rmse/exact_rmse
    ratio_bias=bais/exact_bias
    it=data.frame(t(c(qname,rmse,mae,bais,ratio_rmse,ratio_mae,ratio_bias)))
    result=rbind.data.frame(result,it)
}





filename=paste("result_sample_cross_fit",sep="",sample_size)
filename=paste(filename,sep="",".csv")
write.table(result, sep = ",",col.names = FALSE,row.names = FALSE,file = filename)

est=data.frame(est)
filename=paste("result_cross_fit",sep="",sample_size)
filename=paste(filename,sep="",".csv")
write.table(est, sep = ",",col.names = TRUE,row.names = FALSE,file = filename)





