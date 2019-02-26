getwd()
source('fun_callback.R')
library(hdm)
library(hqreg)
library(quantreg)
library(doSNOW)
#---------------------------------------------------------data preprocessing------------------------------------------------------------
normal<-function(x){
  y=(x-min(x))/(max(x)-min(x))
  return(y)
}
data=pension
alpha=data$p401
z=data$e401
#y=data$net_tfa
y=data$tw
age=data$age
fsize=data$fsize
ira=data$ira
marr=data$marr
db=data$db
hown=data$hown
nohs=data$nohs
hs=data$hs
smcol=data$smcol
col=data$col
male=data$male
twoearn=data$twoearn
hmort=data$hmort/1000

hequity=data$hequity/1000
educ=data$educ
hval=data$hval/1000

inc=data$inc/1000

control=cbind(inc,age,fsize,ira,marr,db,hown,nohs,hs,smcol)
x=as.matrix(control,nrow=9915)
alpha=matrix(alpha)
z=matrix(z)
y=matrix(y)
y2=(y-mean(y))/sd(y)

#-------------------------------------------------------------------NFTA------------------------------------------------------------------
hdm10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.1,0.001),8)                  # NFTA 0.1   0.045*63522=2858.49
gmm_normalize10=gmm_parallel(y2,alpha,control,z,0.10,seq(0,0.1,0.001),8)        # NFTA 0.1   0.034*63522=2159.748
gmm10=gmm_parallel(y,alpha,control,z,0.10,seq(1000,5000,100),8)                 # NFTA 0.1   2200
non_gmm10=non_gmm_parallel(y,alpha,control,z,0.10,seq(1000,5000,100),8)         # NFTA 0.1   2100

hdm25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.1,0.001),8)                  # NFTA 0.25  0.044*63522=2794.968
gmm_normalize25=gmm_parallel(y2,alpha,control,z,0.25,seq(0,0.1,0.001),8)        # NFTA 0.25  0.051*63522=3239.622
gmm25=gmm_parallel(y,alpha,control,z,0.25,seq(1000,5000,100),8)                 # NFTA 0.25  2600
non_gmm25=non_gmm_parallel(y,alpha,control,z,0.25,seq(1000,5000,100),8)         # NFTA 0.1   2600

hdm50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.15,0.001),8)                  # NFTA 0.5  0.092*63522=5844.024
gmm_normalize50=gmm_parallel(y2,alpha,control,z,0.5,seq(0,0.15,0.001),8)        # NFTA 0.5  0.095*63522=6034.59
gmm50=gmm_parallel(y,alpha,control,z,0.5,seq(3000,8000,150),8)                  # NFTA 0.5  6150
non_gmm50=non_gmm_parallel(y,alpha,control,z,0.5,seq(3000,8000,150),8)          # NFTA 0.5   6150

hdm75=hdm_parallel(y2,alpha,control,z,0.75,seq(0.05,0.35,0.001),8)              # NFTA 0.75  0.296*63522=18802.51
gmm_normalize75=gmm_parallel(y2,alpha,control,z,0.75,seq(0.05,0.3,0.001),8)     # NFTA 0.75  0.191*63522=12132.7
gmm75=gmm_parallel(y,alpha,control,z,0.75,seq(8000,20000,100),8)                # NFTA 0.75  12900
non_gmm75=non_gmm_parallel(y,alpha,control,z,0.75,seq(8000,20000,100),8)        # NFTA 0.75  12900

hdm90=hdm_parallel(y2,alpha,control,z,0.9,seq(0.25,0.5,0.001),8)                 # NFTA 0.9  0.456*63522=28966.03
gmm_normalize90=gmm_parallel(y2,alpha,control,z,0.9,seq(0.15,0.5,0.001),8)       # NFTA 0.9  0.354*63522=22486.79
gmm90=gmm_parallel(y,alpha,control,z,0.9,seq(8000,25000,100),8)                  # NFTA 0.9  21400
non_gmm90=non_gmm_parallel(y,alpha,control,z,0.9,seq(8000,25000,100),8)          # NFTA 0.9  21600


#-------------------------------------------------------------------TW------------------------------------------------------------------
hdm10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.1,0.001),8)                  # NFTA 0.1   0.028*111529=3122.812
gmm_normalize10=gmm_parallel(y2,alpha,control,z,0.10,seq(0,0.1,0.001),8)        # NFTA 0.1   0.009*111529=1003.761
gmm10=gmm_parallel(y,alpha,control,z,0.10,seq(1000,5000,100),8)                 # NFTA 0.1   2300
non_gmm10=non_gmm_parallel(y,alpha,control,z,0.10,seq(1000,5000,100),8)         # NFTA 0.1   2400

hdm25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.1,0.001),8)                  # NFTA 0.25  0.039*111529= 4349.631
gmm_normalize25=gmm_parallel(y2,alpha,control,z,0.25,seq(0,0.1,0.001),8)        # NFTA 0.25  0.018*111529=2007.522
gmm25=gmm_parallel(y,alpha,control,z,0.25,seq(1000,5000,100),8)                 # NFTA 0.25  3000
non_gmm25=non_gmm_parallel(y,alpha,control,z,0.25,seq(1000,5000,100),8)         # NFTA 0.1   3000

hdm50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.15,0.001),8)                  # NFTA 0.5  0.036*111529=4015.044
gmm_normalize50=gmm_parallel(y2,alpha,control,z,0.5,seq(0,0.15,0.001),8)        # NFTA 0.5  0.03*111529=3345.87
gmm50=gmm_parallel(y,alpha,control,z,0.5,seq(2000,6000,100),8)                  # NFTA 0.5  3900
non_gmm50=non_gmm_parallel(y,alpha,control,z,0.5,seq(2000,6000,100),8)          # NFTA 0.5  4000

hdm75=hdm_parallel(y2,alpha,control,z,0.75,seq(0,0.2,0.001),8)                 # NFTA 0.75  0.037*111529=4126.573
gmm_normalize75=gmm_parallel(y2,alpha,control,z,0.75,seq(0,0.2,0.001),8)       # NFTA 0.75  0.028*111529=3122.812
gmm75=gmm_parallel(y,alpha,control,z,0.75,seq(2000,6000,100),8)                # NFTA 0.75  3600
non_gmm75=non_gmm_parallel(y,alpha,control,z,0.75,seq(2000,6000,100),8)        # NFTA 0.75  3500

hdm90=hdm_parallel(y2,alpha,control,z,0.9,seq(0,0.2,0.001),8)                  # NFTA 0.9  0.033*111529=3680.457
gmm_normalize90=gmm_parallel(y2,alpha,control,z,0.9,seq(0,0.2,0.001),8)        # NFTA 0.9  0.065*111529=7249.385
gmm90=gmm_parallel(y,alpha,control,z,0.9,seq(1000,6000,100),8)                 # NFTA 0.9  2600
non_gmm90=non_gmm_parallel(y,alpha,control,z,0.9,seq(1000,6000,100),8)         # NFTA 0.9  2800