# DML-QR
### 環境需求(實測套件版本)：
- R version 3.4.3
- quantreg 5.34
- hdm 0.2.0
- hqreg 1.4
- mvtnorm 1.0.6
- doSNOW 1.0.16


### Simulation ：
1. The fun_callback in simulation folder contains the HDM, IVQR-GMM with partialing out Z on X(gmm),  IVQR-GMM without partialing out Z on X(non-gmm).

2. The data generation process is extended by Exact computation of GMM estimators for instrumental variable quantile
regression models, Journal of Applied Econometrics.

3. Monte Carlo Experiment 

    'exact' : Exact specification of IVQR-GMM  with partialing out Z on X(gmm).

    'nonexact'：Exact specification of IVQR-GMM  without partialing out Z on X(non-gmm).
	
	'fullgmm'：Mispecification of IVQR-GMM  with partialing out Z on X(gmm), containing many meaningless control variables.
	
	'hdm'：Mispecification of DML-QR, containing many meaningless control variables.

### Empirical：
