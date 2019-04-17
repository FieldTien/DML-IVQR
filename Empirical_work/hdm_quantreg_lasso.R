set.seed(2019)
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

control=cbind(inc,age,fsize,ira,hmort,hequity,hval,educ
              ,inc^2,age^2,fsize^2,ira^2,hmort^2,hequity^2,hval^2,educ^2
              ,inc^3,age^3,fsize^3,ira^3,hmort^3,hequity^3,hval^3,educ^3
              ,age*fsize,age*ira,age*educ,age*hmort,age*hequity,age*hval,age*inc
              ,fsize*ira,fsize*educ,fsize*hmort,fsize*hequity,fsize*hval,fsize*inc
              ,ira*educ,ira*hmort,ira*hequity,ira*hval,ira*inc
              ,educ*hmort,educ*hequity,educ*hval,educ*inc
              ,hmort*hequity,hmort*hval,hmort*inc
              ,hval*inc
)
control=apply(control,2,normal)
control=cbind(control,marr,db,hown,nohs,hs,smcol,male)
colnames(control) = c('inc','age','fsize','ira','hmort','hequity'
                      ,'inc^2','age^2','fsize^2','ira^2','hmort^2','hequity^2','hval^2','educ^2'
                      ,'inc^3','age^3','fsize^3','ira^3','hmort^3','hequity^3','hval^3','educ^3'
                      ,'age*fsize','age*ira','age*educ','age*hmort','age*hequity','age*hval','age*inc'
                      ,'fsize*ira','fsize*educ','fsize*hmort','fsize*hequity','fsize*hval','fsize*inc'
                      ,'ira*educ','ira*hmort','ira*hequity','ira*hval','ira*inc'
                      ,'educ*hmort','educ*hequity','educ*hval','educ*inc'
                      ,'hmort*hequity','hmort*hval','hmort*inc'
                      ,'hval*inc','hval','educ','marr','db','hown','nohs','hs','smcol','male')
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



a10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3400        tw 2800
a15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3100        tw 3000    
a20=hdm_rq_parallel(y,alpha,control,z,0.20,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3100        tw 3100
a25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3400        tw 3600
a30=hdm_rq_parallel(y,alpha,control,z,0.30,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3300        tw 4400
a35=hdm_rq_parallel(y,alpha,control,z,0.35,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 3700        tw 5100 
a40=hdm_rq_parallel(y,alpha,control,z,0.40,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 4700        tw 5700
a45=hdm_rq_parallel(y,alpha,control,z,0.45,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 5100        tw 6500
a50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)       # nfta 6100        tw 7400 
a55=hdm_rq_parallel(y,alpha,control,z,0.55,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 6800        tw 8400
a60=hdm_rq_parallel(y,alpha,control,z,0.6,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)       # nfta 8600        tw 9200
a65=hdm_rq_parallel(y,alpha,control,z,0.65,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 10300       tw 9700
a70=hdm_rq_parallel(y,alpha,control,z,0.70,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 12300       tw 10900
a75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 13700       tw 12900                              
a80=hdm_rq_parallel(y,alpha,control,z,0.80,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 18400       tw 14100                                 
a85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 17200       tw 15900                                
a90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,25000,100),seq(0.1,8,0.2),8)      # nfta 20800       tw 12900


grid=seq(0,25000,100)
tw10=c(matrix(unlist(a10[2], use.names=FALSE)))
tw15=c(matrix(unlist(a15[2], use.names=FALSE)))
tw20=c(matrix(unlist(a20[2], use.names=FALSE)))
tw25=c(matrix(unlist(a25[2], use.names=FALSE)))
tw30=c(matrix(unlist(a30[2], use.names=FALSE)))
tw35=c(matrix(unlist(a35[2], use.names=FALSE)))
tw40=c(matrix(unlist(a40[2], use.names=FALSE)))
tw45=c(matrix(unlist(a45[2], use.names=FALSE)))
tw50=c(matrix(unlist(a50[2], use.names=FALSE)))
tw55=c(matrix(unlist(a55[2], use.names=FALSE)))
tw60=c(matrix(unlist(a60[2], use.names=FALSE)))
tw65=c(matrix(unlist(a65[2], use.names=FALSE)))
tw70=c(matrix(unlist(a70[2], use.names=FALSE)))
tw75=c(matrix(unlist(a75[2], use.names=FALSE)))
tw80=c(matrix(unlist(a80[2], use.names=FALSE)))
tw85=c(matrix(unlist(a85[2], use.names=FALSE)))
tw90=c(matrix(unlist(a90[2], use.names=FALSE)))

tw=cbind(grid,tw10,grid,tw15,grid,tw20,grid,tw25,grid,tw30,grid,tw35,grid,tw40,grid,tw45,grid,tw50,grid,tw55,grid,tw60,grid,tw65,grid,tw70,grid,tw75,grid,tw80,grid,tw85,grid,tw90)
colnames(tw) = c('grid10','tw10','grid15','tw15','grid20','tw20','grid25','tw25','grid30','tw30','grid35','tw35','grid40','tw40','grid45','tw45'
                 ,'grid50','tw50','grid55','tw55','grid60','tw60','grid65','tw65','grid70','tw70','grid75','tw75','grid80','tw80','grid85','tw85','grid90','tw90')


write.table(tw,"quantreg_data/twci_quantreg.csv",sep = ",",row.names = FALSE)

#------------------------------------------------------------------------nfta------------------------------------------------------------------------------------

y=data$net_tfa

nfta10=hdm_rq_parallel(y,alpha,control,z,0.10,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)   
nfta15=hdm_rq_parallel(y,alpha,control,z,0.15,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta15 0.049*63522.5=3112.6025              tw15 0.026*111529.7=2899.772
nfta20=hdm_rq_parallel(y,alpha,control,z,0.20,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta20 0.047*63522.5=2985.5575              tw20 0.026*111529.7=2899.772
nfta25=hdm_rq_parallel(y,alpha,control,z,0.25,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)     # nfta25 0.049*63522.5=3112.6025              tw25 0.033*111529.7=3680.48
nfta30=hdm_rq_parallel(y,alpha,control,z,0.30,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta30 0.05*63522.5=3176.125                tw30 0.04*111529.7=4461.188
nfta35=hdm_rq_parallel(y,alpha,control,z,0.35,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta35 0.058*63522.5= 3684.305              tw35 0.047*111529.7=5241.896
nfta40=hdm_rq_parallel(y,alpha,control,z,0.40,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta40 0.074*63522.5=4700.665               tw40 0.055*111529.7=6134.133
nfta45=hdm_rq_parallel(y,alpha,control,z,0.45,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)      # nfta45 0.086*63522.5=5462.9349              tw45 0.061*111529.7=6803.312
nfta50=hdm_rq_parallel(y,alpha,control,z,0.5,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    # nfta50 0.104*63522.5=6606.34                tw50 0.07*111529.7=7807.079
nfta55=hdm_rq_parallel(y,alpha,control,z,0.55,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)     # nfta55 0.12*63522.5=7559.1775               tw55 0.074*111529.7=8253.198
nfta60=hdm_rq_parallel(y,alpha,control,z,0.6,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    # nfta60 0.144*63522.5=9083.7174              tw60 0.085*111529.7=9480.024
nfta65=hdm_rq_parallel(y,alpha,control,z,0.65,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    # nfta65 0.178*63522.5=11307                  tw65 0.096*111529.7=10706.85
nfta70=hdm_rq_parallel(y,alpha,control,z,0.70,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    # nfta70 0.218*63522.5=13847.905              tw70 0.104*111529.7=11599.09
nfta75=hdm_rq_parallel(y,alpha,control,z,0.75,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #                                             tw75 0.132*111529.7=14721.92
nfta80=hdm_rq_parallel(y,alpha,control,z,0.80,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #                                             tw80 0.152*111529.7 =16952.51    
nfta85=hdm_rq_parallel(y,alpha,control,z,0.85,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8)    #                                             tw85 0.136*111529.7=15168.04
nfta90=hdm_rq_parallel(y,alpha,control,z,0.90,alphaint=seq(0,35000,100),seq(0.1,8,0.2),8) 



grid=seq(0,35000,100)
tw10=c(matrix(unlist(nfta10[2], use.names=FALSE)))
tw15=c(matrix(unlist(nfta15[2], use.names=FALSE)))
tw20=c(matrix(unlist(nfta20[2], use.names=FALSE)))
tw25=c(matrix(unlist(nfta25[2], use.names=FALSE)))
tw30=c(matrix(unlist(nfta30[2], use.names=FALSE)))
tw35=c(matrix(unlist(nfta35[2], use.names=FALSE)))
tw40=c(matrix(unlist(nfta40[2], use.names=FALSE)))
tw45=c(matrix(unlist(nfta45[2], use.names=FALSE)))
tw50=c(matrix(unlist(nfta50[2], use.names=FALSE)))
tw55=c(matrix(unlist(nfta55[2], use.names=FALSE)))
tw60=c(matrix(unlist(nfta60[2], use.names=FALSE)))
tw65=c(matrix(unlist(nfta65[2], use.names=FALSE)))
tw70=c(matrix(unlist(nfta70[2], use.names=FALSE)))
tw75=c(matrix(unlist(nfta75[2], use.names=FALSE)))
tw80=c(matrix(unlist(nfta80[2], use.names=FALSE)))
tw85=c(matrix(unlist(nfta85[2], use.names=FALSE)))
tw90=c(matrix(unlist(nfta90[2], use.names=FALSE)))

tw=cbind(grid,tw10,grid,tw15,grid,tw20,grid,tw25,grid,tw30,grid,tw35,grid,tw40,grid,tw45,grid,tw50,grid,tw55,grid,tw60,grid,tw65,grid,tw70,grid,tw75,grid,tw80,grid,tw85,grid,tw90)
colnames(tw) = c('grid10','tw10','grid15','tw15','grid20','tw20','grid25','tw25','grid30','tw30','grid35','tw35','grid40','tw40','grid45','tw45'
                 ,'grid50','tw50','grid55','tw55','grid60','tw60','grid65','tw65','grid70','tw70','grid75','tw75','grid80','tw80','grid85','tw85','grid90','tw90')


write.table(tw,"quantreg_data/nftaci_quantreg.csv",sep = ",",row.names = FALSE)

Y=data$tw
selected10=seleted_nonzero(Y-a10$QTE*alpha,control,tau = 0.1,penal = seq(0.1,8,.2))
selected25=seleted_nonzero(Y-a25$QTE*alpha,control,tau = 0.25,penal = seq(0.1,8,.2))
selected50=seleted_nonzero(Y-a50$QTE*alpha,control,tau = 0.5,penal = seq(0.1,8,.2))
selected75=seleted_nonzero(Y-a75$QTE*alpha,control,tau = 0.75,penal = seq(0.1,8,.2))
selected90=seleted_nonzero(Y-a90$QTE*alpha,control,tau = 0.9,penal = seq(0.1,8,.2))

select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q10","q25","q50","q75","q90")
write.table(select, file = "quantreg_data/selected_tw.csv", sep = ",",row.names = FALSE)

Y=data$net_tfa
selected10=seleted_nonzero(Y-nfta10$QTE*alpha,control,tau = 0.1,penal = seq(0.1,8,.2))
selected25=seleted_nonzero(Y-nfta25$QTE*alpha,control,tau = 0.25,penal = seq(0.1,8,.2))
selected50=seleted_nonzero(Y-nfta50$QTE*alpha,control,tau = 0.5,penal = seq(0.1,8,.2))
selected75=seleted_nonzero(Y-nfta75$QTE*alpha,control,tau = 0.75,penal = seq(0.1,8,.2))
selected90=seleted_nonzero(Y-nfta90$QTE*alpha,control,tau = 0.9,penal = seq(0.1,8,.2))
select=cbind(c(selected10,rep(NA,100-length(selected10))),c(selected25,rep(NA,100-length(selected25))),c(selected50,rep(NA,100-length(selected50))),c(selected75,rep(NA,100-length(selected75))),c(selected90,rep(NA,100-length(selected90))))
colnames(select)<-c("q10","q25","q50","q75","q90")
write.table(select, file = "quantreg_data/selected_nfta.csv", sep = ",",row.names = FALSE)

