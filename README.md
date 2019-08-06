# DML-IVQR 
### 環境需求(實測套件版本)： 
- R version 3.4.3
- quantreg 5.34
- hdm 0.2.0
- hqreg 1.4
- mvtnorm 1.0.6
- doSNOW 1.0.16


## fuction_callback：
### Simulation:
* **hdm_quantile(Y, D, X, Z, tau):** DML-IVQR

* **gmm_quantile(Y, D, X, Z, tau):** GMM-IVQR with partialing out

* **nongmm_quantile(Y, D, X, Z, tau):** GMM-IVQR  without partialing out

### Empirical: 
* **gmm_parallel(Y, D, X, Z, tau, QTE_interval, number of core):** 
  * GMM-IVQR with partialing out
  
  * Output: QTE, QTE_interval_loss
* **nongmm_quantile(Y, D, X, Z, tau, QTE_interval, number of core):** 
  * GMM-IVQR  without partialing out
  
  * Output: QTE, QTE_interval_loss


* **hdm_rq_parallel(Y, D, X, Z, tau, QTE_interval, penaly_interval, number of core):** 
  * DML-IVQR based on quantreg
  
  * Output: QTE, QTE_interval_loss, QTE_interval_value
  * **seleted_nonzero(Y,X,tau,penalty):** Return nonzero control variable
    
* **hdm_parallel(Y, D, X, Z, tau, QTE_interval, number of core):**  
  * DML-IVQR based on hqreg
 
  * Output: QTE, QTE_interval_loss, QTE_interval_value
  * **hqreg_seleted_nonzero(Y,X,tau,penalty):** Return nonzero control variable
    
## 401K Example 
```gherkin=
#=====================================================================
#==========================Preprocessing==============================
#=====================================================================
source('Empirical_work/fun_callback.R')
library(hdm)
library(hqreg)
library(quantreg)
library(doSNOW)
data=matrix(pension)
alpha=matrix(data$p401)
z=matrix(data$e401)
y=matrix(data$net_tfa)
control=cbind(data$i2,data$i3,data$i4,data$i5,data$i6,data$i7,data$a1,data$a2,data$a3,data$a4,data$marr,data$fsize)
control=as.matrix(control,nrow=9915)

#=============================Estimation==============================
#=====================================================================
hdm50=hdm_parallel(y,alpha,control,z,0.5,seq(0,35000,100),2)  
```
