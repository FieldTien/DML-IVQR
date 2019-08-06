
source('fun_callback.R')
library(hdm)
library(hqreg)
library(quantreg)
library(doSNOW)
library(IVQR)
#---------------------------------------------------------data preprocessing------------------------------------------------------------
normal<-function(x){
  y=(x-min(x))/(max(x)-min(x))
  return(y)
}
data=pension
alpha=data$p401
z=data$e401

y=data$tw
control=cbind(data$i2,data$i3,data$i4,data$i5,data$i6,data$i7,data$a1,data$a2,data$a3,data$a4,data$marr,data$fsize,data$twoearn,data$db,data$pira,data$hown
              ,data$hs,data$smcol,data$col)
control=as.matrix(control,nrow=9915)
alpha=matrix(alpha)
z=matrix(z)
y=matrix(y)
y2=(y-mean(y))/sd(y)

model <- tw  ~ p401 | e401 | i2+i3+i4+i5+i6+i7+a1+a2+a3+a4+marr+fsize+twoearn+db+pira+hown+hs+smcol+col




#-------------------------------------------------------------------TW------------------------------------------------------------------

ivqr_tw10=ivqr(formula =model,taus = 0.1,data = data ,grid = seq(0,25000,100), qrMethod = 'br')   #4100
ivqr_tw15=ivqr(formula =model,taus = 0.15,data = data ,grid = seq(0,25000,100), qrMethod = 'br')   #4100
ivqr_tw25=ivqr(formula =model,taus = 0.25,data = data ,grid = seq(0,25000,100), qrMethod = 'br')  #4800
ivqr_tw50=ivqr(formula =model,taus = 0.5,data = data ,grid = seq(0,25000,100), qrMethod = 'br')   #4000
ivqr_tw75=ivqr(formula =model,taus = 0.75,data = data ,grid = seq(0,25000,100), qrMethod = 'br')  #9000
ivqr_tw85=ivqr(formula =model,taus = 0.85,data = data ,grid = seq(0,25000,100), qrMethod = 'br')   #4900
ivqr_tw90=ivqr(formula =model,taus = 0.9,data = data ,grid = seq(0,25000,100), qrMethod = 'br')   #4900
hdm10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.3,0.002),8)                  # 
tw_gmm_normalize10=gmm_parallel(y2,alpha,control,z,0.10,seq(0,0.3,0.002),8)      # 
tw_gmm10=gmm_parallel(y,alpha,control,z,0.10,seq(0,35000,100),8)                 # 
tw_non_gmm10=non_gmm_parallel(y,alpha,control,z,0.10,seq(0,35000,100),8)         # 
hdm_rq10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #3800


hdm15=hdm_parallel(y2,alpha,control,z,0.15,seq(0,0.3,0.002),8)                  # 
tw_gmm_normalize15=gmm_parallel(y2,alpha,control,z,0.15,seq(0,0.3,0.002),8)      # 
tw_gmm15=gmm_parallel(y,alpha,control,z,0.15,seq(0,35000,100),8)                 # 
tw_non_gmm15=non_gmm_parallel(y,alpha,control,z,0.15,seq(0,35000,100),8)         # 
tw_hdm_rq15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #3800



hdm25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.3,0.002),8)                   # 
tw_gmm_normalize25=gmm_parallel(y2,alpha,control,z,0.25,seq(0,0.3,0.002),8)      # 
tw_gmm25=gmm_parallel(y,alpha,control,z,0.25,seq(0,35000,100),8)                 # 
tw_non_gmm25=non_gmm_parallel(y,alpha,control,z,0.25,seq(0,35000,100),8)         # 
tw_hdm_rq25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #4800


hdm50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.3,0.002),8)                    # 
tw_gmm_normalize50=gmm_parallel(y2,alpha,control,z,0.5,seq(0,0.3,0.002),8)       # 
tw_gmm50=gmm_parallel(y,alpha,control,z,0.5,seq(0,35000,100),8)                  # 
tw_non_gmm50=non_gmm_parallel(y,alpha,control,z,0.5,seq(0,35000,100),8)          # 
tw_hdm_rq50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)     #3900


hdm75=hdm_parallel(y2,alpha,control,z,0.75,seq(0,0.3,0.002),8)               # 
tw_gmm_normalize75=gmm_parallel(y2,alpha,control,z,0.75,seq(0,0.3,0.002),8)      # 
tw_gmm75=gmm_parallel(y,alpha,control,z,0.75,seq(0,35000,100),8)                 # 
tw_non_gmm75=non_gmm_parallel(y,alpha,control,z,0.75,seq(0,35000,100),8)         # 
tw_hdm_rq75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #4400


hdm85=hdm_parallel(y2,alpha,control,z,0.85,seq(0,0.3,0.002),8)                  # 
tw_gmm_normalize85=gmm_parallel(y2,alpha,control,z,0.85,seq(0,0.3,0.002),8)      # 
tw_gmm85=gmm_parallel(y,alpha,control,z,0.85,seq(0,35000,100),8)                 # 
tw_non_gmm85=non_gmm_parallel(y,alpha,control,z,0.85,seq(0,35000,100),8)         # 
tw_hdm_rq85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #3800


hdm90=hdm_parallel(y2,alpha,control,z,0.9,seq(0,0.3,0.002),8)                 # 
tw_gmm_normalize90=gmm_parallel(y2,alpha,control,z,0.9,seq(0,0.3,0.002),8)           # 
tw_gmm90=gmm_parallel(y,alpha,control,z,0.9,seq(0,35000,100),8)                  # 
tw_non_gmm90=non_gmm_parallel(y,alpha,control,z,0.9,seq(0,35000,100),8)          # 
tw_hdm_rq90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #2300








#-------------------------------------------------------------------NFTA------------------------------------------------------------------


y=data$net_tfa
y2=(y-mean(y))/sd(y)
model <- net_tfa  ~ p401 | e401 | i2+i3+i4+i5+i6+i7+a1+a2+a3+a4+marr+fsize+twoearn+db+pira+hown+hs+smcol+col

nfta_ivqr_tw10=ivqr(formula =model,taus = 0.1,data = data ,grid = seq(0,35000,100), qrMethod = 'br')   #4100
nfta_ivqr_tw15=ivqr(formula =model,taus = 0.15,data = data ,grid = seq(0,35000,100), qrMethod = 'br')   #4100
nfta_ivqr_tw25=ivqr(formula =model,taus = 0.25,data = data ,grid = seq(0,35000,100), qrMethod = 'br')  #4800
nfta_ivqr_tw50=ivqr(formula =model,taus = 0.5,data = data ,grid = seq(0,35000,100), qrMethod = 'br')   #4000
nfta_ivqr_tw75=ivqr(formula =model,taus = 0.75,data = data ,grid = seq(0,35000,100), qrMethod = 'br')  #9000
nfta_ivqr_tw85=ivqr(formula =model,taus = 0.85,data = data ,grid = seq(0,35000,100), qrMethod = 'br')   #4900
nfta_ivqr_tw90=ivqr(formula =model,taus = 0.9,data = data ,grid = seq(0,35000,100), qrMethod = 'br')   #4900

nfta_hdm10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.4,0.002),8)                # 
nfta_gmm_normalize10=gmm_parallel(y2,alpha,control,z,0.10,seq(0,0.4,0.002),8)         # 
nfta_gmm10=gmm_parallel(y,alpha,control,z,0.10,seq(0,35000,100),8)               # 
nfta_non_gmm10=non_gmm_parallel(y,alpha,control,z,0.10,seq(0,35000,100),8)       # 
nfta_hdm_rq_nfta10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #3000

nfta_hdm15=hdm_parallel(y2,alpha,control,z,0.15,seq(0,0.4,0.002),8)                   # 
nfta_gmm_normalize15=gmm_parallel(y2,alpha,control,z,0.15,seq(0,0.4,0.002),8)        # 
nfta_gmm15=gmm_parallel(y,alpha,control,z,0.15,seq(0,35000,100),8)                   # 
nfta_non_gmm15=non_gmm_parallel(y,alpha,control,z,0.15,seq(0,35000,100),8)           # 
nfta_hdm_rq_nfta15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #2900


nfta_hdm25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.4,0.002),8)                   # 
nfta_gmm_normalize25=gmm_parallel(y2,alpha,control,z,0.25,seq(0,0.4,0.002),8)        # 
nfta_gmm25=gmm_parallel(y,alpha,control,z,0.25,seq(0,35000,100),8)                   # 
nfta_non_gmm25=non_gmm_parallel(y,alpha,control,z,0.25,seq(0,35000,100),8)           # 
nfta_hdm_rq_nfta25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #2900


nfta_hdm50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.4,0.002),8)                   # 
nfta_gmm_normalize50=gmm_parallel(y2,alpha,control,z,0.5,seq(0,0.4,0.002),8)            # 
nfta_gmm50=gmm_parallel(y,alpha,control,z,0.5,seq(0,35000,100),8)                    # 
nfta_non_gmm50=non_gmm_parallel(y,alpha,control,z,0.5,seq(0,35000,100),8)            # 
nfta_hdm_rq_nfta50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)     #6000


nfta_hdm75=hdm_parallel(y2,alpha,control,z,0.75,seq(0,0.4,0.002),8)                 # 
nfta_gmm_normalize75=gmm_parallel(y2,alpha,control,z,0.75,seq(0,0.4,0.002),8)         # 
nfta_gmm75=gmm_parallel(y,alpha,control,z,0.75,seq(0,35000,100),8)                  # 
nfta_non_gmm75=non_gmm_parallel(y,alpha,control,z,0.75,seq(0,35000,100),8)          # 
nfta_hdm_rq_nfta75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #11900


nfta_hdm85=hdm_parallel(y2,alpha,control,z,0.85,seq(0,0.4,0.002),8)                   # 
nfta_gmm_normalize85=gmm_parallel(y2,alpha,control,z,0.85,seq(0,0.4,0.002),8)        # 
nfta_gmm85=gmm_parallel(y,alpha,control,z,0.85,seq(0,35000,100),8)                   # 
nfta_non_gmm85=non_gmm_parallel(y,alpha,control,z,0.85,seq(0,35000,100),8)           # 
nfta_hdm_rq_nfta85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #2900


nfta_hdm90=hdm_parallel(y2,alpha,control,z,0.9,seq(0,0.4,0.002),8)                    # 
nfta_gmm_normalize90=gmm_parallel(y2,alpha,control,z,0.9,seq(0,0.4,0.002),8)         # 
nfta_gmm90=gmm_parallel(y,alpha,control,z,0.9,seq(0,35000,100),8)                   # 
nfta_non_gmm90=non_gmm_parallel(y,alpha,control,z,0.9,seq(0,35000,100),8)          # 
nfta_hdm_rq_nfta90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #19900


q10=t(matrix(c(ivqr_tw10$coef$endg_var,tw_gmm10[1],tw_non_gmm10[1],nfta_ivqr_tw10$coef$endg_var,nfta_gmm10[1],nfta_non_gmm10[1])))
q15=t(matrix(c(ivqr_tw15$coef$endg_var,tw_gmm15[1],tw_non_gmm15[1],nfta_ivqr_tw15$coef$endg_var,nfta_gmm15[1],nfta_non_gmm15[1])))
q25=t(matrix(c(ivqr_tw25$coef$endg_var,tw_gmm25[1],tw_non_gmm25[1],nfta_ivqr_tw25$coef$endg_var,nfta_gmm25[1],nfta_non_gmm25[1])))
q50=t(matrix(c(ivqr_tw50$coef$endg_var,tw_gmm50[1],tw_non_gmm50[1],nfta_ivqr_tw50$coef$endg_var,nfta_gmm50[1],nfta_non_gmm50[1])))
q75=t(matrix(c(ivqr_tw75$coef$endg_var,tw_gmm75[1],tw_non_gmm75[1],nfta_ivqr_tw75$coef$endg_var,nfta_gmm75[1],nfta_non_gmm75[1])))
q85=t(matrix(c(ivqr_tw85$coef$endg_var,tw_gmm85[1],tw_non_gmm85[1],nfta_ivqr_tw85$coef$endg_var,nfta_gmm85[1],nfta_non_gmm85[1])))
q90=t(matrix(c(ivqr_tw90$coef$endg_var,tw_gmm90[1],tw_non_gmm90[1],nfta_ivqr_tw90$coef$endg_var,nfta_gmm90[1],nfta_non_gmm90[1])))

original<-rbind(q10,q15,q25,q50,q75,q85,q90)
original<-cbind(c("q10","q15","q25","q50","q75","q85","q90"),original)
colnames(original)<-c("Quantile","ivqr_tw","gmm_po_tw","gmm_std_tw","ivqr_nfta","gmm_po_nfta","gmm_std_nfta")
write.table(original,"original_reg/401k_result.csv",sep = ",",row.names = FALSE)
