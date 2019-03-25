library(haven)
library(dplyr)
library(hdm)
library(hqreg)
library(quantreg)
library(doSNOW)
library(IVQR)


source('fun_callback.R')
core=8
data_jtpa    <- read_sav("jtpa.sav")
colnames(data_jtpa) <- c("id", "earning", "randomized_offer", "program_participation", "sex", "hsorged", 
                         "black", "hispanic", "married", "wkless13", "afdc", "age2225", "age2629", "age3035"
                         , "age3644", "age4554", "class_tr", "ojt_jsa", "f2sms")
male=filter(data_jtpa, sex == 1)
female=filter(data_jtpa, sex == 0)

Y_male=male["earning"]
D_male=male["program_participation"]
Z_male=male["randomized_offer"]
X_male=male[c("age2225","age2629","age3035","age3644","age4554","hsorged","married","black","hispanic","wkless13","afdc","class_tr", "ojt_jsa", "f2sms")]
Y_male=as.matrix(Y_male)
D_male=as.matrix(D_male)
Z_male=as.matrix(Z_male)
X_male=as.matrix(X_male)
Y_male_std=(Y_male-mean(Y_male))/sd(Y_male) # std 19540
model <-earning ~ program_participation | randomized_offer | age2225+age2629+age3035+age3644+age4554+hsorged+married+black+hispanic+wkless13+afdc+class_tr+ojt_jsa+f2sms

ivqr_male15=ivqr(formula =model,taus = 0.15,data = male ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_male15=hdm_parallel(Y_male,D_male,X_male,Z_male,0.15,seq(-1000,4000,100),core) 
gmm_male15=gmm_parallel(Y_male,D_male,X_male,Z_male,0.15,seq(-1000,4000,100),core) 
non_gmm_male15=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.15,seq(-1000,4000,100),core)
hdm_male15=hdm_parallel(Y_male_std,D_male,X_male,Z_male,0.15,seq(-0.2,0.5,0.01),core)  
gmm_nomalize_male15=gmm_parallel(Y_male_std,D_male,X_male,Z_male,0.15,seq(-0.2,0.5,0.01),core)

ivqr_male25=ivqr(formula =model,taus = 0.25,data = male ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_male25=hdm_parallel(Y_male,D_male,X_male,Z_male,0.25,seq(-1000,4000,100),core) 
gmm_male25=gmm_parallel(Y_male,D_male,X_male,Z_male,0.25,seq(-1000,4000,100),core) 
non_gmm_male25=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.25,seq(-1000,4000,100),core) 
hdm_male25=hdm_parallel(Y_male_std,D_male,X_male,Z_male,0.25,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_male25=gmm_parallel(Y_male_std,D_male,X_male,Z_male,0.25,seq(-0.2,0.5,0.01),core) 

ivqr_male50=ivqr(formula =model,taus = 0.5,data = male ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_male50=hdm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(-1000,4000,100),core) 
gmm_male50=gmm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(-1000,4000,100),core) 
non_gmm_male50=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(-1000,5000,100),core)
hdm_male50=hdm_parallel(Y_male_std,D_male,X_male,Z_male,0.5,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_male50=gmm_parallel(Y_male_std,D_male,X_male,Z_male,0.5,seq(-0.2,0.5,0.01),core) 


ivqr_male75=ivqr(formula =model,taus = 0.75,data = male ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_male75=hdm_parallel(Y_male,D_male,X_male,Z_male,0.75,seq(-1000,4000,100),core) 
gmm_male75=gmm_parallel(Y_male,D_male,X_male,Z_male,0.75,seq(-1000,8000,100),core) 
non_gmm_male75=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.75,seq(-1000,8000,100),core) 
hdm_male75=hdm_parallel(Y_male_std,D_male,X_male,Z_male,0.75,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_male75=gmm_parallel(Y_male_std,D_male,X_male,Z_male,0.75,seq(-0.2,0.5,0.01),core) 

ivqr_male85=ivqr(formula =model,taus = 0.85,data = male ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_male85=hdm_parallel(Y_male,D_male,X_male,Z_male,0.85,seq(-1000,4000,100),core) 
gmm_male85=gmm_parallel(Y_male,D_male,X_male,Z_male,0.85,seq(-1000,5000,8000),core) 
non_gmm_male85=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.85,seq(-1000,8000,100),core) 
hdm_male85=hdm_parallel(Y_male_std,D_male,X_male,Z_male,0.85,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_male85=gmm_parallel(Y_male_std,D_male,X_male,Z_male,0.85,seq(-0.2,0.5,0.01),core) 

#---------------------------------female QTE ----------------------------------

Y_female=female["earning"]
D_female=female["program_participation"]
Z_female=female["randomized_offer"]
X_female=female[c("age2225","age2629","age3035","age3644","age4554","hsorged","married","black","hispanic","wkless13","afdc","class_tr", "ojt_jsa", "f2sms")]
Y_female=as.matrix(Y_female)
D_female=as.matrix(D_female)
Z_female=as.matrix(Z_female)
X_female=as.matrix(X_female)
Y_female_std=(Y_female-mean(Y_female))/sd(Y_female) # std 19540



ivqr_female15=ivqr(formula =model,taus = 0.15,data = female ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_female15=hdm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-500,3000,100),core) 
gmm_female15=gmm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-1000,4000,100),core) 
non_gmm_female15=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-1000,4000,100),core)
hdm_female15=hdm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-0.2,0.5,0.01),core)  
gmm_nomalize_female15=gmm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-0.2,0.5,0.01),core) 

ivqr_female25=ivqr(formula =model,taus = 0.25,data = female ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_female25=hdm_parallel(Y_female,D_male,X_male,Z_male,0.25,seq(-1000,4000,100),core) 
gmm_female25=gmm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-1000,4000,100),core) 
non_gmm_female25=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-1000,4000,100),core) 
hdm_female25=hdm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_female25=gmm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-0.2,0.5,0.01),core) 

ivqr_female50=ivqr(formula =model,taus = 0.25,data = female ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_female50=hdm_parallel(Y_female,D_male,X_male,Z_male,0.5,seq(-1000,4000,100),core) 
gmm_female50=gmm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-1000,4000,100),core) 
non_gmm_female50=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-1000,5000,100),core)
hdm_female50=hdm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_female50=gmm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-0.2,0.5,0.01),core) 

ivqr_female75=ivqr(formula =model,taus = 0.75,data = female ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_female75=hdm_parallel(Y_female,D_male,X_male,Z_male,0.75,seq(-1000,4000,100),core) 
gmm_female75=gmm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-1000,8000,100),core) 
non_gmm_female75=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-1000,8000,100),core) 
hdm_female75=hdm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_female75=gmm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-0.2,0.5,0.01),core) 

ivqr_female85=ivqr(formula =model,taus = 0.85,data = female ,grid = seq(-1000,4000,100), qrMethod = 'br')
hdm_ori_female85=hdm_parallel(Y_female,D_male,X_male,Z_male,0.85,seq(-1000,4000,100),core) 
gmm_female85=gmm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-1000,5000,8000),core) 
non_gmm_female85=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-1000,8000,100),core) 
hdm_female85=hdm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-0.2,0.5,0.01),core)   
gmm_nomalize_female85=gmm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-0.2,0.5,0.01),core) 

