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

ivqr_male10=ivqr(formula =model,taus = 0.10,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male10=gmm_parallel(Y_male,D_male,X_male,Z_male,0.10,seq(-2000,10000,100),core) 
non_gmm_male10=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.10,seq(-2000,10000,100),core)



ivqr_male15=ivqr(formula =model,taus = 0.15,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male15=gmm_parallel(Y_male,D_male,X_male,Z_male,0.15,seq(-2000,10000,100),core) 
non_gmm_male15=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.15,seq(-2000,10000,100),core)


ivqr_male25=ivqr(formula =model,taus = 0.25,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male25=gmm_parallel(Y_male,D_male,X_male,Z_male,0.25,seq(-2000,10000,100),core) 
non_gmm_male25=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.25,seq(-2000,10000,100),core) 

ivqr_male50=ivqr(formula =model,taus = 0.5,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male50=gmm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(-2000,10000,100),core) 
non_gmm_male50=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(-2000,10000,100),core)


ivqr_male75=ivqr(formula =model,taus = 0.75,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male75=gmm_parallel(Y_male,D_male,X_male,Z_male,0.75,seq(-2000,10000,100),core) 
non_gmm_male75=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.75,seq(-2000,10000,100),core) 

ivqr_male85=ivqr(formula =model,taus = 0.85,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male85=gmm_parallel(Y_male,D_male,X_male,Z_male,0.85,seq(-2000,10000,100),core) 
non_gmm_male85=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.85,seq(-2000,10000,100),core) 

ivqr_male90=ivqr(formula =model,taus = 0.90,data = male ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_male90=gmm_parallel(Y_male,D_male,X_male,Z_male,0.90,seq(-2000,10000,100),core) 
non_gmm_male90=non_gmm_parallel(Y_male,D_male,X_male,Z_male,0.90,seq(-2000,10000,100),core) 

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




ivqr_female10=ivqr(formula =model,taus = 0.10,data = female ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_female10=gmm_parallel(Y_female,D_female,X_female,Z_female,0.10,seq(-2000,10000,100),core) 
non_gmm_female10=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.10,seq(-2000,10000,100),core)



ivqr_female15=ivqr(formula =model,taus = 0.15,data = female ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_female15=gmm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-2000,10000,100),core) 
non_gmm_female15=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.15,seq(-2000,10000,100),core)

ivqr_female25=ivqr(formula =model,taus = 0.25,data = female ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_female25=gmm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-2000,10000,100),core) 
non_gmm_female25=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.25,seq(-2000,10000,100),core) 

ivqr_female50=ivqr(formula =model,taus = 0.5,data = female ,grid =seq(-2000,10000,100), qrMethod = 'br')
gmm_female50=gmm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-2000,10000,100),core) 
non_gmm_female50=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.5,seq(-2000,10000,100),core)

ivqr_female75=ivqr(formula =model,taus = 0.75,data = female ,grid =seq(-2000,10000,100), qrMethod = 'br')
gmm_female75=gmm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-2000,10000,100),core) 
non_gmm_female75=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.75,seq(-2000,10000,100),core) 

ivqr_female85=ivqr(formula =model,taus = 0.85,data = female ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_female85=gmm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-2000,10000,100),core) 
non_gmm_female85=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.85,seq(-2000,10000,100),core) 


ivqr_female90=ivqr(formula =model,taus = 0.90,data = female ,grid = seq(-2000,10000,100), qrMethod = 'br')
gmm_female90=gmm_parallel(Y_female,D_female,X_female,Z_female,0.90,seq(-2000,10000,100),core) 
non_gmm_female90=non_gmm_parallel(Y_female,D_female,X_female,Z_female,0.90,seq(-2000,10000,100),core)


q10=matrix(cbind(ivqr_male10$coef$endg_var,gmm_male10[1],non_gmm_male10[1],ivqr_female10$coef$endg_var,gmm_female10[1],non_gmm_female10[1]))
q15=matrix(cbind(ivqr_male15$coef$endg_var,gmm_male15[1],non_gmm_male15[1],ivqr_female15$coef$endg_var,gmm_female15[1],non_gmm_female15[1]))
q25=matrix(cbind(ivqr_male25$coef$endg_var,gmm_male25[1],non_gmm_male25[1],ivqr_female25$coef$endg_var,gmm_female25[1],non_gmm_female25[1]))
q50=matrix(cbind(ivqr_male50$coef$endg_var,gmm_male50[1],non_gmm_male50[1],ivqr_female50$coef$endg_var,gmm_female50[1],non_gmm_female50[1]))
q75=matrix(cbind(ivqr_male75$coef$endg_var,gmm_male75[1],non_gmm_male75[1],ivqr_female75$coef$endg_var,gmm_female75[1],non_gmm_female75[1]))
q85=matrix(cbind(ivqr_male85$coef$endg_var,gmm_male85[1],non_gmm_male85[1],ivqr_female85$coef$endg_var,gmm_female85[1],non_gmm_female85[1]))
q90=matrix(cbind(ivqr_male90$coef$endg_var,gmm_male90[1],non_gmm_male90[1],ivqr_female90$coef$endg_var,gmm_female90[1],non_gmm_female90[1]))

result <- cbind(c('ivqr_male','gmm_male','non_gmm_male','ivqr_female','gmm_female','non_gmm_female'),q10,q15,q25,q50,q75,q85,q90)
colnames(result) <- c('item','q10','q15','q25','q50','q75','q85','q90')
write.table(result,"original_reg/jpta_result.csv",sep = ",",row.names = FALSE)
