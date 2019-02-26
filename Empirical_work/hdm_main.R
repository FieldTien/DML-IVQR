

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

#-------------------------------------------------------- HDM training ----------------------------------------------------------------------

a10=hdm_parallel(y2,alpha,control,z,0.10,seq(0,0.1,0.001),8)     # nfta10 0.055*63522.5=3493.738               tw10 0.021*111529.7=2342.124
a15=hdm_parallel(y2,alpha,control,z,0.15,seq(0,0.2,0.001),8)     # nfta15 0.049*63522.5=3112.6025              tw15 0.026*111529.7=2899.772
a20=hdm_parallel(y2,alpha,control,z,0.20,seq(0,0.2,0.001),8)     # nfta20 0.047*63522.5=2985.5575              tw20 0.026*111529.7=2899.772
a25=hdm_parallel(y2,alpha,control,z,0.25,seq(0,0.25,0.001),8)    # nfta25 0.049*63522.5=3112.6025              tw25 0.033*111529.7=3680.48
a30=hdm_parallel(y2,alpha,control,z,0.30,seq(0,0.2,0.001),8)     # nfta30 0.05*63522.5=3176.125                tw30 0.04*111529.7=4461.188
a35=hdm_parallel(y2,alpha,control,z,0.35,seq(0,0.2,0.001),8)     # nfta35 0.058*63522.5= 3684.305              tw35 0.047*111529.7=5241.896
a40=hdm_parallel(y2,alpha,control,z,0.40,seq(0,0.2,0.001),8)     # nfta40 0.074*63522.5=4700.665               tw40 0.055*111529.7=6134.133
a45=hdm_parallel(y2,alpha,control,z,0.45,seq(0,0.2,0.001),8)     # nfta45 0.086*63522.5=5462.9349              tw45 0.061*111529.7=6803.312
a50=hdm_parallel(y2,alpha,control,z,0.5,seq(0.05,0.15,0.001),8)  # nfta50 0.104*63522.5=6606.34                tw50 0.07*111529.7=7807.079
a55=hdm_parallel(y2,alpha,control,z,0.55,seq(0.05,0.2,0.001),8)  # nfta55 0.12*63522.5=7559.1775               tw55 0.074*111529.7=8253.198
a60=hdm_parallel(y2,alpha,control,z,0.6,seq(0.05,0.2,0.001),8)   # nfta60 0.144*63522.5=9083.7174              tw60 0.085*111529.7=9480.024
a65=hdm_parallel(y2,alpha,control,z,0.65,seq(0.05,0.2,0.001),8)  # nfta65 0.178*63522.5=11307                  tw65 0.096*111529.7=10706.85
a70=hdm_parallel(y2,alpha,control,z,0.70,seq(0.05,0.25,0.001),8) # nfta70 0.218*63522.5=13847.905              tw70 0.104*111529.7=11599.09
a75=hdm_parallel(y2,alpha,control,z,0.75,seq(0.05,0.25,0.001),8) #                                             tw75 0.132*111529.7=14721.92
a80=hdm_parallel(y2,alpha,control,z,0.80,seq(0.05,0.25,0.001),8) #                                             tw80 0.152*111529.7 =16952.51    
a85=hdm_parallel(y2,alpha,control,z,0.85,seq(0.05,0.25,0.001),8) #                                             tw85 0.136*111529.7=15168.04
a90=hdm_parallel(y2,alpha,control,z,0.90,seq(0.05,0.25,0.001),8) #                                             tw90 0.16*111529.7=17844.75

a65p=hdm_parallel(y2,alpha,control,z,0.65,seq(0.2,0.30,0.001),8)  
a70p=hdm_parallel(y2,alpha,control,z,0.70,seq(0.25,0.30,0.001),8)
a75p=hdm_parallel(y2,alpha,control,z,0.75,seq(0.25,0.30,0.001),8) #nfta75 0.254*63522.5=16134.72 

a80p=hdm_parallel(y2,alpha,control,z,0.80,seq(0.25,0.30,0.001),8) 
a85p=hdm_parallel(y2,alpha,control,z,0.85,seq(0.25,0.30,0.001),8) 
a90p=hdm_parallel(y2,alpha,control,z,0.90,seq(0.25,0.30,0.001),8) 


a80qp=hdm_parallel(y2,alpha,control,z,0.80,seq(0.30,0.45,0.001),8)#nfta80 0.321*63522.5=20390.72
a85qp=hdm_parallel(y2,alpha,control,z,0.85,seq(0.30,0.45,0.001),8)#nfta85 0.409*63522.5=25980.7
a90qp=hdm_parallel(y2,alpha,control,z,0.90,seq(0.30,0.45,0.001),8)#nfta90 0.413*63522.5=26234.79

a85qpp=hdm_parallel(y2,alpha,control,z,0.85,seq(0.45,0.5,0.001),8) 
a90qpp=hdm_parallel(y2,alpha,control,z,0.90,seq(0.45,0.5,0.001),8) 
a90qqpp=hdm_parallel(y2,alpha,control,z,0.90,seq(0.5,0.52,0.001),8)



nfta90=c(matrix(unlist(a90[2], use.names=FALSE)),matrix(unlist(a90p[2], use.names=FALSE))[-1],matrix(unlist(a90qp[2], use.names=FALSE))[-1],matrix(unlist(a90qpp[2], use.names=FALSE))[-1],matrix(unlist(a90qqpp[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.52,0.001),rep(NA,471-length(nfta90))))
nfta90=cbind(grid,nfta90  )

nfta85=c(matrix(unlist(a85[2], use.names=FALSE)),matrix(unlist(a85p[2], use.names=FALSE))[-1],matrix(unlist(a85qp[2], use.names=FALSE))[-1],matrix(unlist(a85qpp[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.5,0.001),rep(NA,471-length(nfta85))))
nfta85=c(nfta85,rep(NA,471-length(nfta85)))
nfta85=cbind(grid,nfta85)

nfta80=c(matrix(unlist(a80[2], use.names=FALSE)),matrix(unlist(a80p[2], use.names=FALSE))[-1],matrix(unlist(a80qp[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.45,0.001),rep(NA,471-length(nfta80))))
nfta80=c(nfta80,rep(NA,471-length(nfta80)))
nfta80=cbind(grid,nfta80)

nfta75=c(matrix(unlist(a75[2], use.names=FALSE)),matrix(unlist(a75p[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.3,0.001),rep(NA,471-length(nfta75))))
nfta75=c(nfta75,rep(NA,471-length(nfta75)))
nfta75=cbind(grid,nfta75)

nfta70=c(matrix(unlist(a70[2], use.names=FALSE)),matrix(unlist(a70p[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.3,0.001),rep(NA,471-length(nfta70))))
nfta70=c(nfta70,rep(NA,471-length(nfta70)))
nfta70=cbind(grid,nfta70)

nfta65=c(matrix(unlist(a65[2], use.names=FALSE)),matrix(unlist(a65p[2], use.names=FALSE))[-1])
grid=c(c(seq(0.05,0.3,0.001),rep(NA,471-length(nfta65))))
nfta65=c(nfta65,rep(NA,471-length(nfta65)))
nfta65=cbind(grid,nfta65)

nfta60=c(matrix(unlist(a60[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a60[2], use.names=FALSE)))))
grid=c(c(seq(0.05,0.2,0.001),rep(NA,471-length(matrix(unlist(a60[2], use.names=FALSE))))))
nfta60=cbind(grid,nfta60)

nfta55=c(matrix(unlist(a55[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a55[2], use.names=FALSE)))))
grid=c(c(seq(0.05,0.2,0.001),rep(NA,471-length(matrix(unlist(a55[2], use.names=FALSE))))))
nfta55=cbind(grid,nfta55)


nfta50=c(matrix(unlist(a50[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a50[2], use.names=FALSE)))))
grid=c(c(seq(0.05,0.15,0.001),rep(NA,471-length(matrix(unlist(a50[2], use.names=FALSE))))))
nfta50=cbind(grid,nfta50)


nfta45=c(matrix(unlist(a45[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a45[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a45[2], use.names=FALSE))))))
nfta45=cbind(grid,nfta45)

nfta40=c(matrix(unlist(a40[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a40[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a40[2], use.names=FALSE))))))
nfta40=cbind(grid,nfta40)


nfta35=c(matrix(unlist(a35[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a35[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a35[2], use.names=FALSE))))))
nfta35=cbind(grid,nfta35)


nfta30=c(matrix(unlist(a30[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a30[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a30[2], use.names=FALSE))))))
nfta30=cbind(grid,nfta30)

nfta25=c(matrix(unlist(a25[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a25[2], use.names=FALSE)))))
grid=c(c(seq(0,0.25,0.001),rep(NA,471-length(matrix(unlist(a25[2], use.names=FALSE))))))
nfta25=cbind(grid,nfta25)

nfta20=c(matrix(unlist(a20[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a20[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a20[2], use.names=FALSE))))))
nfta20=cbind(grid,nfta20)


nfta15=c(matrix(unlist(a15[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a15[2], use.names=FALSE)))))
grid=c(c(seq(0,0.2,0.001),rep(NA,471-length(matrix(unlist(a15[2], use.names=FALSE))))))
nfta15=cbind(grid,nfta15)

nfta10=c(matrix(unlist(a10[2], use.names=FALSE)),rep(NA,471-length(matrix(unlist(a10[2], use.names=FALSE)))))
grid=c(c(seq(0,0.1,0.001),rep(NA,471-length(matrix(unlist(a10[2], use.names=FALSE))))))
nfta10=cbind(grid,nfta10)
nftaci=data.frame(cbind(nfta10,nfta15,nfta20,nfta25,nfta30,nfta35,nfta40,nfta45,nfta50,nfta55,nfta60,nfta65,nfta70,nfta75,nfta80,nfta85,nfta90))
#write.table(nftaci,"nftaci.csv",sep = ",",row.names = FALSE)
write.table(nftaci,"twci.csv",sep = ",",row.names = FALSE)