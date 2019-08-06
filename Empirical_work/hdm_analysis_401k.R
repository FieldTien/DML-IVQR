

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
age=apply(matrix(data$age),2,normal)
fsize=apply(matrix(data$fsize),2,normal)
ira=apply(matrix(data$ira),2,normal)
marr=data$marr
db=data$db
hown=data$hown
hs=data$hs
smcol=data$smcol
col=data$col
male=data$male
twoearn=data$twoearn
hmort=apply(matrix(data$hmort),2,normal)

hequity=data$hequity
educ=apply(matrix(data$educ),2,normal)
hval=apply(matrix(data$hval),2,normal)
inc=apply(matrix(data$inc),2,normal)

i2=data$i2
i3=data$i3
i4=data$i4
i5=data$i5
i6=data$i6
i7=data$i7
a1=data$a1
a2=data$a2
a3=data$a3
a4=data$a4
pira=data$pira
twoearn=data$twoearn


alpha=matrix(alpha)
z=matrix(z)
y=matrix(y)
y2=(y-mean(y))/sd(y)



control=cbind(inc,age,fsize,ira,hmort,hequity,hval,educ
             ,inc^2,age^2,fsize^2,ira^2,hmort^2,hequity^2,hval^2,educ^2
             ,inc^3,age^3,fsize^3,ira^3,hmort^3,hequity^3,hval^3,educ^3
             ,age*fsize,age*ira,age*educ,age*hmort,age*hequity,age*hval,age*inc
             ,fsize*ira,fsize*educ,fsize*hmort,fsize*hequity,fsize*hval,fsize*inc
             ,ira*educ,ira*hmort,ira*hequity,ira*hval,ira*inc
             ,educ*hmort,educ*hequity,educ*hval,educ*inc
             ,hmort*hequity,hmort*hval,hmort*inc
             ,hval*inc,marr,db,hown,hs,smcol,col,male,i2,i3,i4,i5,i6,i7,a1,a2,a3,a4,pira,twoearn
             ,marr*inc,marr*age,marr*fsize,marr*ira,marr*hmort,marr*hequity,marr*hval,marr*educ
             ,pira*inc,pira*age,pira*fsize,pira*hmort,pira*hequity,pira*hval,pira*educ
             ,twoearn*inc,twoearn*age,twoearn*fsize,twoearn*ira,twoearn*hmort,twoearn*hequity,twoearn*hval,twoearn*educ
)

colnames(control) = c('inc','age','fsize','ira','hmort','hequity','hval','educ'
                     ,'inc^2','age^2','fsize^2','ira^2','hmort^2','hequity^2','hval^2','educ^2'
                     ,'inc^3','age^3','fsize^3','ira^3','hmort^3','hequity^3','hval^3','educ^3'
                     ,'age*fsize','age*ira','age*educ','age*hmort','age*hequity','age*hval','age*inc'
                     ,'fsize*ira','fsize*educ','fsize*hmort','fsize*hequity','fsize*hval','fsize*inc'
                     ,'ira*educ','ira*hmort','ira*hequity','ira*hval','ira*inc'
                     ,'educ*hmort','educ*hequity','educ*hval','educ*inc'
                     ,'hmort*hequity','hmort*hval','hmort*inc'
                     ,'hval*inc','marr','db','hown','hs','smcol','col','male'
                     ,'i2','i3','i4','i5','i6','i7','a1','a2','a3','a4','pira','twoearn'
                     ,'marr*inc','marr*age','marr*fsize','marr*ira','marr*hmort','marr*hequity','marr*hval','marr*educ'
                     ,'pira*inc','pira*age','pira*fsize','pira*hmort','pira*hequity','pira*hval','pira*educ'
                     ,'twoearn*inc','twoearn*age','twoearn*fsize','twoearn*ira','twoearn*hmort','twoearn*hequity','twoearn*hval','twoearn*educ'
                     
                     )
cubicage=control[,"age^3"]
powerage=control[,"age^2"]
age=control[,"age"]
cubicthesholdage20=rep(0,9915)
cubicthesholdage40=rep(0,9915)
cubicthesholdage60=rep(0,9915)
powerthesholdage20=rep(0,9915)
powerthesholdage40=rep(0,9915)
powerthesholdage60=rep(0,9915)
thesholdage20=rep(0,9915)
thesholdage40=rep(0,9915)
thesholdage60=rep(0,9915)

cubicinc=control[,"inc^3"]
powerinc=control[,"inc^2"]
inc=control[,"inc"]
cubicthesholdinc20=rep(0,9915)
cubicthesholdinc40=rep(0,9915)
cubicthesholdinc60=rep(0,9915)
powerthesholdinc20=rep(0,9915)
powerthesholdinc40=rep(0,9915)
powerthesholdinc60=rep(0,9915)
thesholdinc20=rep(0,9915)
thesholdinc40=rep(0,9915)
thesholdinc60=rep(0,9915)
cubiceduc=control[,"educ^3"]
powereduc=control[,"educ^2"]
educ=control[,"educ"]
cubicthesholdeduc20=rep(0,9915)
cubicthesholdeduc40=rep(0,9915)
cubicthesholdeduc60=rep(0,9915)
powerthesholdeduc20=rep(0,9915)
powerthesholdeduc40=rep(0,9915)
powerthesholdeduc60=rep(0,9915)
thesholdeduc20=rep(0,9915)
thesholdeduc40=rep(0,9915)
thesholdeduc60=rep(0,9915)

for (i in 1:9915) {
  cubicthesholdage40[i]=(function (x) max(0,x-0.4)) (cubicage[i])
  cubicthesholdage60[i]=(function (x) max(0,x-0.6)) (cubicage[i])
  cubicthesholdage20[i]=(function (x) max(0,x-0.2)) (cubicage[i])
  powerthesholdage40[i]=(function (x) max(0,x-0.4)) (powerage[i])
  powerthesholdage60[i]=(function (x) max(0,x-0.6)) (powerage[i])
  powerthesholdage20[i]=(function (x) max(0,x-0.2)) (powerage[i])
  thesholdage40[i]=(function (x) max(0,x-0.4)) (age[i])
  thesholdage60[i]=(function (x) max(0,x-0.6)) (age[i])
  thesholdage20[i]=(function (x) max(0,x-0.2)) (age[i])
  cubicthesholdinc40[i]=(function (x) max(0,x-0.4)) (cubicinc[i])
  cubicthesholdinc60[i]=(function (x) max(0,x-0.6)) (cubicinc[i])
  cubicthesholdinc20[i]=(function (x) max(0,x-0.2)) (cubicinc[i])
  powerthesholdinc40[i]=(function (x) max(0,x-0.4)) (powerinc[i])
  powerthesholdinc60[i]=(function (x) max(0,x-0.6)) (powerinc[i])
  powerthesholdinc20[i]=(function (x) max(0,x-0.2)) (powerinc[i])
  thesholdinc40[i]=(function (x) max(0,x-0.4)) (inc[i])
  thesholdinc60[i]=(function (x) max(0,x-0.6)) (inc[i])
  thesholdinc20[i]=(function (x) max(0,x-0.2)) (inc[i])
  cubicthesholdeduc40[i]=(function (x) max(0,x-0.4)) (cubiceduc[i])
  cubicthesholdeduc60[i]=(function (x) max(0,x-0.6)) (cubiceduc[i])
  cubicthesholdeduc20[i]=(function (x) max(0,x-0.2)) (cubiceduc[i])
  powerthesholdeduc40[i]=(function (x) max(0,x-0.4)) (powereduc[i])
  powerthesholdeduc60[i]=(function (x) max(0,x-0.6)) (powereduc[i])
  powerthesholdeduc20[i]=(function (x) max(0,x-0.2)) (powereduc[i])
  thesholdeduc40[i]=(function (x) max(0,x-0.4)) (educ[i])
  thesholdeduc60[i]=(function (x) max(0,x-0.6)) (educ[i])
  thesholdeduc20[i]=(function (x) max(0,x-0.2)) (educ[i])
  i=i+1
}

control=cbind(control,cubicthesholdage40,cubicthesholdage60,cubicthesholdage20
             ,powerthesholdage40,powerthesholdage60,powerthesholdage20
             ,thesholdage40,thesholdage60,thesholdage20
             ,cubicthesholdinc40,cubicthesholdinc60,cubicthesholdinc20
             ,powerthesholdinc40,powerthesholdinc60,powerthesholdinc20
             ,thesholdinc40,thesholdinc60,thesholdinc20
             ,cubicthesholdeduc40,cubicthesholdeduc60,cubicthesholdeduc20
             ,powerthesholdeduc40,powerthesholdeduc60,powerthesholdeduc20
             ,thesholdeduc40,thesholdeduc60,thesholdeduc20)

#-------------------------------------------------------- HDM tw  training ----------------------------------------------------------------------

hqreg_tw_a10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.25,0.001),8)     # nfta10 0.055*63522.5=3493.738               tw10 0.021*111529.7=2342.124
hqreg_tw_a15=hdm_parallel(y2,alpha,control,z,0.15,seq(0,0.25,0.001),8)   # nfta15 0.049*63522.5=3112.6025              tw15 0.026*111529.7=2899.772
hqreg_tw_a20=hdm_parallel(y2,alpha,control,z,0.20,seq(0,0.25,0.001),8)      # nfta20 0.047*63522.5=2985.5575              tw20 0.026*111529.7=2899.772
hqreg_tw_a25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.25,0.001),8)     # nfta25 0.049*63522.5=3112.6025              tw25 0.033*111529.7=3680.48
hqreg_tw_a30=hdm_parallel(y2,alpha,control,z,0.30,seq(0,0.25,0.001),8)       # nfta30 0.05*63522.5=3176.125                tw30 0.04*111529.7=4461.188
hqreg_tw_a35=hdm_parallel(y2,alpha,control,z,0.35,seq(0,0.25,0.001),8)       # nfta35 0.058*63522.5= 3684.305              tw35 0.047*111529.7=5241.896
hqreg_tw_a40=hdm_parallel(y2,alpha,control,z,0.40,seq(0,0.25,0.001),8)       # nfta40 0.074*63522.5=4700.665               tw40 0.055*111529.7=6134.133
hqreg_tw_a45=hdm_parallel(y2,alpha,control,z,0.45,seq(0,0.25,0.001),8)      # nfta45 0.086*63522.5=5462.9349              tw45 0.061*111529.7=6803.312
hqreg_tw_a50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.25,0.001),8)     # nfta50 0.104*63522.5=6606.34                tw50 0.07*111529.7=7807.079
hqreg_tw_a55=hdm_parallel(y2,alpha,control,z,0.55,seq(0,0.25,0.001),8)      # nfta55 0.12*63522.5=7559.1775               tw55 0.074*111529.7=8253.198
hqreg_tw_a60=hdm_parallel(y2,alpha,control,z,0.6,seq(0,0.25,0.001),8)      # nfta60 0.144*63522.5=9083.7174              tw60 0.085*111529.7=9480.024
hqreg_tw_a65=hdm_parallel(y2,alpha,control,z,0.65,seq(0,0.25,0.001),8)    # nfta65 0.178*63522.5=11307                  tw65 0.096*111529.7=10706.85
hqreg_tw_a70=hdm_parallel(y2,alpha,control,z,0.70,seq(0,0.25,0.001),8)      # nfta70 0.218*63522.5=13847.905              tw70 0.104*111529.7=11599.09
hqreg_tw_a75=hdm_parallel(y2,alpha,control,z,0.75,seq(0,0.25,0.001),8)     #                                             tw75 0.132*111529.7=14721.92
hqreg_tw_a80=hdm_parallel(y2,alpha,control,z,0.80,seq(0,0.25,0.001),8)       #                                             tw80 0.152*111529.7 =16952.51    
hqreg_tw_a85=hdm_parallel(y2,alpha,control,z,0.85,seq(0,0.25,0.001),8)        #                                             tw85 0.136*111529.7=15168.04
hqreg_tw_a90=hdm_parallel(y2,alpha,control,z,0.90,seq(0,0.25,0.001),8)     #                                             tw90 0.16*111529.7=17844.75


quantreg_tw_a10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3400        tw 2800
quantreg_tw_a15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3100        tw 3000    
quantreg_tw_a20=hdm_rq_parallel(y,alpha,control,z,0.20,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3100        tw 3300
quantreg_tw_a25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3400        tw 3700
quantreg_tw_a30=hdm_rq_parallel(y,alpha,control,z,0.30,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3300        tw 4600
quantreg_tw_a35=hdm_rq_parallel(y,alpha,control,z,0.35,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 3700        tw 5300
quantreg_tw_a40=hdm_rq_parallel(y,alpha,control,z,0.40,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 4700        tw 7800
quantreg_tw_a45=hdm_rq_parallel(y,alpha,control,z,0.45,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 5100        tw 7800
quantreg_tw_a50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)       # nfta 6100        tw 7400 
quantreg_tw_a55=hdm_rq_parallel(y,alpha,control,z,0.55,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 6800        tw 8400
quantreg_tw_a60=hdm_rq_parallel(y,alpha,control,z,0.6,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)       # nfta 8600        tw 9200
quantreg_tw_a65=hdm_rq_parallel(y,alpha,control,z,0.65,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 10300       tw 9700
quantreg_tw_a70=hdm_rq_parallel(y,alpha,control,z,0.70,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 12300       tw 10900
quantreg_tw_a75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 13700       tw 12900                              
quantreg_tw_a80=hdm_rq_parallel(y,alpha,control,z,0.80,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 18400       tw 14100                                 
quantreg_tw_a85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 17200       tw 15900                                
quantreg_tw_a90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,25000,100),seq(0.1,10,0.22),8)      # nfta 20800       tw 12900

tw_hqreg <- cbind(hqreg_tw_a10[1],hqreg_tw_a15[1],hqreg_tw_a20[1],hqreg_tw_a25[1],hqreg_tw_a30[1],hqreg_tw_a35[1],hqreg_tw_a40[1],hqreg_tw_a45[1],hqreg_tw_a50[1],hqreg_tw_a55[1],hqreg_tw_a60[1],hqreg_tw_a65[1],hqreg_tw_a70[1],hqreg_tw_a75[1],hqreg_tw_a80[1],hqreg_tw_a85[1],hqreg_tw_a90[1])
tw_hqreg_normal <- as.numeric(tw_hqreg)*111529.7
tw_quantreg <- cbind(quantreg_tw_a10$QTE,quantreg_tw_a15$QTE,quantreg_tw_a20$QTE,quantreg_tw_a25$QTE,quantreg_tw_a30$QTE,quantreg_tw_a35$QTE,quantreg_tw_a40$QTE,quantreg_tw_a45$QTE,quantreg_tw_a50$QTE,quantreg_tw_a55$QTE,quantreg_tw_a60$QTE,quantreg_tw_a65$QTE,quantreg_tw_a70$QTE,quantreg_tw_a75$QTE,quantreg_tw_a80$QTE,quantreg_tw_a85$QTE,quantreg_tw_a90$QTE)
rbind(tw_hqreg,tw_hqreg_normal
      ,tw_quantreg)
#-------------------------------------------------------- HDM nfta training ----------------------------------------------------------------------
y=data$net_tfa
y2=(y-mean(y))/sd(y)

hqreg_nfta_a10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.5,0.002),8)     # nfta10 0.055*63522.5=3493.738               tw10 0.021*111529.7=2342.124
hqreg_nfta_a15=hdm_parallel(y2,alpha,control,z,0.15,seq(0,0.5,0.002),8)     # nfta15 0.049*63522.5=3112.6025              tw15 0.026*111529.7=2899.772
hqreg_nfta_a20=hdm_parallel(y2,alpha,control,z,0.20,seq(0,0.5,0.002),8)       # nfta20 0.047*63522.5=2985.5575              tw20 0.026*111529.7=2899.772
hqreg_nfta_a25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.5,0.002),8)        # nfta25 0.049*63522.5=3112.6025              tw25 0.033*111529.7=3680.48
hqreg_nfta_a30=hdm_parallel(y2,alpha,control,z,0.301,seq(0,0.5,0.002),8)     # nfta30 0.05*63522.5=3176.125                tw30 0.04*111529.7=4461.188
hqreg_nfta_a35=hdm_parallel(y2,alpha,control,z,0.35,seq(0,0.5,0.002),8)        # nfta35 0.058*63522.5= 3684.305              tw35 0.047*111529.7=5241.896
hqreg_nfta_a40=hdm_parallel(y2,alpha,control,z,0.40,seq(0,0.5,0.002),8)       # nfta40 0.074*63522.5=4700.665               tw40 0.055*111529.7=6134.133
hqreg_nfta_a45=hdm_parallel(y2,alpha,control,z,0.45,seq(0,0.5,0.002),8)      # nfta45 0.086*63522.5=5462.9349              tw45 0.061*111529.7=6803.312
hqreg_nfta_a50=hdm_parallel(y2,alpha,control,z,0.5,seq(0,0.5,0.002),8)     # nfta50 0.104*63522.5=6606.34                tw50 0.07*111529.7=7807.079
hqreg_nfta_a55=hdm_parallel(y2,alpha,control,z,0.55,seq(0,0.5,0.002),8)        # nfta55 0.12*63522.5=7559.1775               tw55 0.074*111529.7=8253.198
hqreg_nfta_a60=hdm_parallel(y2,alpha,control,z,0.6,seq(0,0.5,0.002),8)         # nfta60 0.144*63522.5=9083.7174              tw60 0.085*111529.7=9480.024
hqreg_nfta_a65=hdm_parallel(y2,alpha,control,z,0.65,seq(0,0.5,0.002),8)     # nfta65 0.178*63522.5=11307                  tw65 0.096*111529.7=10706.85
hqreg_nfta_a70=hdm_parallel(y2,alpha,control,z,0.70,seq(0,0.5,0.002),8)        # nfta70 0.218*63522.5=13847.905              tw70 0.104*111529.7=11599.09
hqreg_nfta_a75=hdm_parallel(y2,alpha,control,z,0.75,seq(0,0.5,0.002),8)       #                                             tw75 0.132*111529.7=14721.92
hqreg_nfta_a80=hdm_parallel(y2,alpha,control,z,0.80,seq(0,0.5,0.002),8)       #                                             tw80 0.152*111529.7 =16952.51    
hqreg_nfta_a85=hdm_parallel(y2,alpha,control,z,0.85,seq(0,0.5,0.002),8)        #                                             tw85 0.136*111529.7=15168.04
hqreg_nfta_a90=hdm_parallel(y2,alpha,control,z,0.90,seq(0,0.5,0.002),8)       #                                             tw90 0.16*111529.7=17844.75


quantreg_nfta_a10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3400        tw 2800
quantreg_nfta_a15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3100        tw 3000    
quantreg_nfta_a20=hdm_rq_parallel(y,alpha,control,z,0.20,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3100        tw 3300
quantreg_nfta_a25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3400        tw 3700
quantreg_nfta_a30=hdm_rq_parallel(y,alpha,control,z,0.30,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3300        tw 4600
quantreg_nfta_a35=hdm_rq_parallel(y,alpha,control,z,0.35,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 3700        tw 5300
quantreg_nfta_a40=hdm_rq_parallel(y,alpha,control,z,0.40,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 4700        tw 7800
quantreg_nfta_a45=hdm_rq_parallel(y,alpha,control,z,0.45,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 5100        tw 7800
quantreg_nfta_a50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)       # nfta 6100        tw 7400 
quantreg_nfta_a55=hdm_rq_parallel(y,alpha,control,z,0.55,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 6800        tw 8400
quantreg_nfta_a60=hdm_rq_parallel(y,alpha,control,z,0.6,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)       # nfta 8600        tw 9200
quantreg_nfta_a65=hdm_rq_parallel(y,alpha,control,z,0.65,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 10300       tw 9700
quantreg_nfta_a70=hdm_rq_parallel(y,alpha,control,z,0.70,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 12300       tw 10900
quantreg_nfta_a75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 13700       tw 12900                              
quantreg_nfta_a80=hdm_rq_parallel(y,alpha,control,z,0.80,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 18400       tw 14100                                 
quantreg_nfta_a85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 17200       tw 15900                                
quantreg_nfta_a90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,30000,100),seq(0.1,10,0.22),8)      # nfta 20800       tw 12900






grid=seq(0,0.25,0.001)
hqreg_tw=cbind(grid,unlist(hqreg_tw_a10[2]),unlist(hqreg_tw_a15[2]),unlist(hqreg_tw_a20[2]),unlist(hqreg_tw_a25[2]),unlist(hqreg_tw_a30[2]),unlist(hqreg_tw_a35[2]),
              unlist(hqreg_tw_a40[2]),unlist(hqreg_tw_a45[2]),unlist(hqreg_tw_a50[2]),unlist(hqreg_tw_a55[2]),unlist(hqreg_tw_a60[2]),unlist(hqreg_tw_a65[2]),
              unlist(hqreg_tw_a70[2]),unlist(hqreg_tw_a75[2]),unlist(hqreg_tw_a80[2]),unlist(hqreg_tw_a85[2]),unlist(hqreg_tw_a90[2]))
hqreg_tw=as.matrix(hqreg_tw)
colnames(hqreg_tw) = c('grid','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90')
write.table(hqreg_tw,"hqreg_data/twci.csv",sep = ",",row.names = FALSE)

grid=seq(0,0.5,0.002)
hqreg_nfta=cbind(grid,unlist(hqreg_nfta_a10[2]),unlist(hqreg_nfta_a15[2]),unlist(hqreg_nfta_a20[2]),unlist(hqreg_nfta_a25[2]),unlist(hqreg_nfta_a30[2]),unlist(hqreg_nfta_a35[2]),
               unlist(hqreg_nfta_a40[2]),unlist(hqreg_nfta_a45[2]),unlist(hqreg_nfta_a50[2]),unlist(hqreg_nfta_a55[2]),unlist(hqreg_nfta_a60[2]),unlist(hqreg_nfta_a65[2]),
               unlist(hqreg_nfta_a70[2]),unlist(hqreg_nfta_a75[2]),unlist(hqreg_nfta_a80[2]),unlist(hqreg_nfta_a85[2]),unlist(hqreg_nfta_a90[2]))
hqreg_tw=as.matrix(hqreg_nfta)
colnames(hqreg_nfta) = c('grid','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90')
write.table(hqreg_nfta,"hqreg_data/nftaci.csv",sep = ",",row.names = FALSE)

grid=seq(0,25000,100)
quantreg_tw=cbind(grid,unlist(quantreg_tw_a10[2]),unlist(quantreg_tw_a15[2]),unlist(quantreg_tw_a20[2]),unlist(quantreg_tw_a25[2]),unlist(quantreg_tw_a30[2]),unlist(quantreg_tw_a35[2]),
               unlist(quantreg_tw_a40[2]),unlist(quantreg_tw_a45[2]),unlist(quantreg_tw_a50[2]),unlist(quantreg_tw_a55[2]),unlist(quantreg_tw_a60[2]),unlist(quantreg_tw_a65[2]),
               unlist(quantreg_tw_a70[2]),unlist(quantreg_tw_a75[2]),unlist(quantreg_tw_a80[2]),unlist(quantreg_tw_a85[2]),unlist(quantreg_tw_a90[2]))
quantreg_tw=as.matrix(quantreg_tw)
colnames(quantreg_tw) = c('grid','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90')
write.table(quantreg_tw,"quantreg_data/twci.csv",sep = ",",row.names = FALSE)

grid=seq(0,30000,100)
quantreg_nfta=cbind(grid,unlist(quantreg_nfta_a10[2]),unlist(quantreg_nfta_a15[2]),unlist(quantreg_nfta_a20[2]),unlist(quantreg_nfta_a25[2]),unlist(quantreg_nfta_a30[2]),unlist(quantreg_nfta_a35[2]),
                  unlist(quantreg_nfta_a40[2]),unlist(quantreg_nfta_a45[2]),unlist(quantreg_nfta_a50[2]),unlist(quantreg_nfta_a55[2]),unlist(quantreg_nfta_a60[2]),unlist(quantreg_nfta_a65[2]),
                  unlist(quantreg_nfta_a70[2]),unlist(quantreg_nfta_a75[2]),unlist(quantreg_nfta_a80[2]),unlist(quantreg_nfta_a85[2]),unlist(quantreg_nfta_a90[2]))
quantreg_nfta=as.matrix(quantreg_nfta)
colnames(quantreg_nfta) = c('grid','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90')
write.table(quantreg_nfta,"quantreg_data/nftaci.csv",sep = ",",row.names = FALSE)

Y=data$tw
selected10=seleted_nonzero(Y-quantreg_tw_a15$QTE*alpha,control,tau = 0.15,penal = seq(0.1,10,0.22))
selected25=seleted_nonzero(Y-quantreg_tw_a25$QTE*alpha,control,tau = 0.25,penal = seq(0.1,10,.22))
selected50=seleted_nonzero(Y-quantreg_tw_a50$QTE*alpha,control,tau = 0.5,penal = seq(0.1,10,.22))
selected75=seleted_nonzero(Y-quantreg_tw_a75$QTE*alpha,control,tau = 0.75,penal = seq(0.1,10,.22))
selected90=seleted_nonzero(Y-quantreg_tw_a85$QTE*alpha,control,tau = 0.85,penal = seq(0.1,10,.22))

select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q15","q25","q50","q75","q85")
write.table(select, file = "quantreg_data/selected_tw.csv", sep = ",",row.names = FALSE)

Y2=(Y-mean(Y))/sd(Y)
selected10=hqreg_seleted_nonzero(Y2-unlist(hqreg_tw_a15[1])[1]*alpha,control,tau = 0.15)
selected25=hqreg_seleted_nonzero(Y2-unlist(hqreg_tw_a25[1])[1]*alpha,control,tau = 0.25)
selected50=hqreg_seleted_nonzero(Y2-unlist(hqreg_tw_a50[1])[1]*alpha,control,tau = 0.5)
selected75=hqreg_seleted_nonzero(Y2-unlist(hqreg_tw_a75[1])[1]*alpha,control,tau = 0.75)
selected90=hqreg_seleted_nonzero(Y2-unlist(hqreg_tw_a85[1])[1]*alpha,control,tau = 0.85)
select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q15","q25","q50","q75","q85")
write.table(select, file = "hqreg_data/selected_tw.csv", sep = ",",row.names = FALSE)



Y=data$net_tfa
selected10=seleted_nonzero(Y-quantreg_nfta_a15$QTE*alpha,control,tau = 0.15,penal = seq(0.1,10,0.22))
selected25=seleted_nonzero(Y-quantreg_nfta_a25$QTE*alpha,control,tau = 0.25,penal = seq(0.1,10,0.22))
selected50=seleted_nonzero(Y-quantreg_nfta_a50$QTE*alpha,control,tau = 0.5,penal = seq(0.1,10,0.22))
selected75=seleted_nonzero(Y-quantreg_nfta_a75$QTE*alpha,control,tau = 0.75,penal = seq(0.1,10,0.22))
selected90=seleted_nonzero(Y-quantreg_nfta_a85$QTE*alpha,control,tau = 0.85,penal = seq(0.1,10,0.22))
select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q15","q25","q50","q75","q85")
write.table(select, file = "quantreg_data/selected_nfta.csv", sep = ",",row.names = FALSE)
Y2=(Y-mean(Y))/sd(Y)
selected10=hqreg_seleted_nonzero(Y2-unlist(hqreg_nfta_a15[1])[1]*alpha,control,tau = 0.15)
selected25=hqreg_seleted_nonzero(Y2-unlist(hqreg_nfta_a25[1])[1]*alpha,control,tau = 0.25)
selected50=hqreg_seleted_nonzero(Y2-unlist(hqreg_nfta_a50[1])[1]*alpha,control,tau = 0.5)
selected75=hqreg_seleted_nonzero(Y2-unlist(hqreg_nfta_a75[1])[1]*alpha,control,tau = 0.75)
selected90=hqreg_seleted_nonzero(Y2-unlist(hqreg_nfta_a85[1])[1]*alpha,control,tau = 0.85)
select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q15","q25","q50","q75","q85")
write.table(select, file = "hqreg_data/selected_nfta.csv", sep = ",",row.names = FALSE)




