library(haven)
library(dplyr)
library(hdm)
library(hqreg)
library(quantreg)
library(doSNOW)
library(IVQR)


source('fun_callback.R')
core=4
data_jtpa    <- read_sav("jtpa.sav")
colnames(data_jtpa) <- c("id", "earning", "randomized_offer", "program_participation", "sex", "hsorged", 
                         "black", "hispanic", "married", "wkless13", "afdc", "age2225", "age2629", "age3035"
                         , "age3644", "age4554", "class_tr", "ojt_jsa", "f2sms")


datatranform <- function(data_jtpa,len)
{
    thesholdmarried20=rep(0,len)
    thesholdmarried40=rep(0,len)
    thesholdmarried60=rep(0,len)
    powerthesholdmarried20=rep(0,len)
    powerthesholdmarried40=rep(0,len)
    powerthesholdmarried60=rep(0,len)
    cubicthesholdmarried20=rep(0,len)
    cubicthesholdmarried40=rep(0,len)
    cubicthesholdmarried60=rep(0,len)
    
    thesholdhsorged20=rep(0,len)
    thesholdhsorged40=rep(0,len)
    thesholdhsorged60=rep(0,len)
    powerthesholdhsorged20=rep(0,len)
    powerthesholdhsorged40=rep(0,len)
    powerthesholdhsorged60=rep(0,len)
    cubicthesholdhsorged20=rep(0,len)
    cubicthesholdhsorged40=rep(0,len)
    cubicthesholdhsorged60=rep(0,len)
    
    thesholdwkless1320=rep(0,len)
    thesholdwkless1340=rep(0,len)
    thesholdwkless1360=rep(0,len)
    powerthesholdwkless1320=rep(0,len)
    powerthesholdwkless1340=rep(0,len)
    powerthesholdwkless1360=rep(0,len)
    cubicthesholdwkless1320=rep(0,len)
    cubicthesholdwkless1340=rep(0,len)
    cubicthesholdwkless1360=rep(0,len)   
    for (i in 1:len) {
        thesholdmarried20[i]=(function (x) max(0,x-0.2)) (data_jtpa$married[i])
        thesholdmarried40[i]=(function (x) max(0,x-0.4)) (data_jtpa$married[i])
        thesholdmarried60[i]=(function (x) max(0,x-0.6)) (data_jtpa$married[i])
        powerthesholdmarried20[i]=(function (x) max(0,x-0.2)) (data_jtpa$married[i]^2)
        powerthesholdmarried40[i]=(function (x) max(0,x-0.4)) (data_jtpa$married[i]^2)
        powerthesholdmarried60[i]=(function (x) max(0,x-0.6)) (data_jtpa$married[i]^2)
        cubicthesholdmarried20[i]=(function (x) max(0,x-0.2)) (data_jtpa$married[i]^3)
        cubicthesholdmarried40[i]=(function (x) max(0,x-0.4)) (data_jtpa$married[i]^3)
        cubicthesholdmarried60[i]=(function (x) max(0,x-0.6)) (data_jtpa$married[i]^3)
        
        thesholdhsorged20[i]=(function (x) max(0,x-0.2)) (data_jtpa$hsorged[i])
        thesholdhsorged40[i]=(function (x) max(0,x-0.4)) (data_jtpa$hsorged[i])
        thesholdhsorged60[i]=(function (x) max(0,x-0.6)) (data_jtpa$hsorged[i])
        powerthesholdhsorged20[i]=(function (x) max(0,x-0.2)) (data_jtpa$hsorged[i]^2)
        powerthesholdhsorged40[i]=(function (x) max(0,x-0.4)) (data_jtpa$hsorged[i]^2)
        powerthesholdhsorged60[i]=(function (x) max(0,x-0.6)) (data_jtpa$hsorged[i]^2)
        cubicthesholdhsorged20[i]=(function (x) max(0,x-0.2)) (data_jtpa$hsorged[i]^3)
        cubicthesholdhsorged40[i]=(function (x) max(0,x-0.4)) (data_jtpa$hsorged[i]^3)
        cubicthesholdhsorged60[i]=(function (x) max(0,x-0.6)) (data_jtpa$hsorged[i]^3)
        
        thesholdwkless1320[i]=(function (x) max(0,x-0.2)) (data_jtpa$wkless13[i])
        thesholdwkless1340[i]=(function (x) max(0,x-0.4)) (data_jtpa$wkless13[i])
        thesholdwkless1360[i]=(function (x) max(0,x-0.6)) (data_jtpa$wkless13[i])
        powerthesholdwkless1320[i]=(function (x) max(0,x-0.2)) (data_jtpa$wkless13[i]^2)
        powerthesholdwkless1340[i]=(function (x) max(0,x-0.4)) (data_jtpa$wkless13[i]^2)
        powerthesholdwkless1360[i]=(function (x) max(0,x-0.6)) (data_jtpa$wkless13[i]^2)
        cubicthesholdwkless1320[i]=(function (x) max(0,x-0.2)) (data_jtpa$wkless13[i]^3)
        cubicthesholdwkless1340[i]=(function (x) max(0,x-0.4)) (data_jtpa$wkless13[i]^3)
        cubicthesholdwkless1360[i]=(function (x) max(0,x-0.6)) (data_jtpa$wkless13[i]^3)
        i=i+1
    }    
    control <- cbind(data_jtpa["sex"],data_jtpa["married"],data_jtpa["married"]^2,data_jtpa["married"]^3,data_jtpa["married"]^4,
                     data_jtpa["hsorged"],data_jtpa["hsorged"]^2,data_jtpa["hsorged"]^3,data_jtpa["hsorged"]^4,
                     data_jtpa["wkless13"],data_jtpa["wkless13"]^2,data_jtpa["wkless13"]^3,data_jtpa["wkless13"]^4,
                     data_jtpa["black"],data_jtpa["black"]*data_jtpa["married"],data_jtpa["black"]*data_jtpa["hsorged"],data_jtpa["black"]*data_jtpa["wkless13"],
                     data_jtpa["hispanic"],data_jtpa["hispanic"]*data_jtpa["married"],data_jtpa["hispanic"]*data_jtpa["hsorged"],data_jtpa["hispanic"]*data_jtpa["wkless13"],
                     data_jtpa["afdc"],data_jtpa["afdc"]*data_jtpa["married"],data_jtpa["afdc"]*data_jtpa["hsorged"],data_jtpa["afdc"]*data_jtpa["wkless13"],
                     data_jtpa["age2225"],data_jtpa["age2225"]*data_jtpa["married"],data_jtpa["age2225"]*data_jtpa["hsorged"],data_jtpa["age2225"]*data_jtpa["wkless13"],
                     data_jtpa["age2629"],data_jtpa["age2629"]*data_jtpa["married"],data_jtpa["age2629"]*data_jtpa["hsorged"],data_jtpa["age2629"]*data_jtpa["wkless13"],
                     data_jtpa["age3035"],data_jtpa["age3035"]*data_jtpa["married"],data_jtpa["age3035"]*data_jtpa["hsorged"],data_jtpa["age3035"]*data_jtpa["wkless13"],
                     data_jtpa["age3644"],data_jtpa["age3644"]*data_jtpa["married"],data_jtpa["age3644"]*data_jtpa["hsorged"],data_jtpa["age3644"]*data_jtpa["wkless13"],
                     data_jtpa["age4554"],data_jtpa["age4554"]*data_jtpa["married"],data_jtpa["age4554"]*data_jtpa["hsorged"],data_jtpa["age4554"]*data_jtpa["wkless13"],
                     data_jtpa["class_tr"],data_jtpa["class_tr"]*data_jtpa["married"],data_jtpa["class_tr"]*data_jtpa["hsorged"],data_jtpa["class_tr"]*data_jtpa["wkless13"],
                     data_jtpa["ojt_jsa"],data_jtpa["ojt_jsa"]*data_jtpa["married"],data_jtpa["ojt_jsa"]*data_jtpa["hsorged"],data_jtpa["ojt_jsa"]*data_jtpa["wkless13"],
                     data_jtpa["f2sms"],data_jtpa["f2sms"]*data_jtpa["married"],data_jtpa["f2sms"]*data_jtpa["hsorged"],data_jtpa["f2sms"]*data_jtpa["wkless13"],
                     thesholdmarried20,thesholdmarried40,thesholdmarried60,
                     powerthesholdhsorged20,powerthesholdhsorged40,powerthesholdhsorged60,
                     cubicthesholdmarried20,cubicthesholdmarried40,cubicthesholdmarried60,
                     thesholdhsorged20,thesholdhsorged40,thesholdhsorged60,
                     powerthesholdhsorged20,powerthesholdhsorged40,powerthesholdhsorged60,
                     cubicthesholdhsorged20,cubicthesholdhsorged40,cubicthesholdhsorged60,
                     thesholdwkless1320,thesholdwkless1340,thesholdwkless1360,
                     powerthesholdwkless1320,powerthesholdwkless1340,powerthesholdwkless1360,
                     cubicthesholdwkless1320,cubicthesholdwkless1340,cubicthesholdwkless1360)
    
    
    
    colnames(control) = c('sex','married','married^2','married^3','married^4','hsorged','hsorged^2','hsorged^3','hsorged^4',
                          'wkless13','wkless13^2','wkless13^3','wkless13^4',
                          'black','black*married','black*hsorged','black*wkless13',
                          'hispanic','hispanic*married','hispanic*hsorged','hispanic*wkless13',
                          'afdc','afdc*married','afdc*hsorged','afdc*wkless13',
                          'age2225','age2225*married','age2225*hsorged','age2225*wkless13',
                          'age2629','age2629*married','age2629*hsorged','age2629*wkless13',
                          'age3035','age3035*married','age3035*hsorged','age3035*wkless13',
                          'age3644','age3644*married','age3644*hsorged','age3644*wkless13',
                          'age4554','age4554*married','age4554*hsorged','age4554*wkless13',
                          'class_tr','class_tr*married','class_tr*hsorged','class_tr*wkless13',
                          'ojt_jsa','ojt_jsa*married','ojt_jsa*hsorged','ojt_jsa*wkless13',
                          'f2sms','f2sms*married','f2sms*hsorged','f2sms*wkless13',
                          'thesholdmarried20','thesholdmarried40','thesholdmarried60',
                          'powerthesholdhsorged20','powerthesholdhsorged40','powerthesholdhsorged60',
                          'cubicthesholdmarried20','cubicthesholdmarried40','cubicthesholdmarried60',
                          'thesholdhsorged20','thesholdhsorged40','thesholdhsorged60',
                          'powerthesholdhsorged20','powerthesholdhsorged40','powerthesholdhsorged60',
                          'cubicthesholdhsorged20','cubicthesholdhsorged40','cubicthesholdhsorged60',
                          'thesholdwkless1320','thesholdwkless1340','thesholdwkless1360',
                          'powerthesholdwkless1320','powerthesholdwkless1340','powerthesholdwkless1360',
                          'cubicthesholdwkless1320','cubicthesholdwkless1340','cubicthesholdwkless1360'                      
    )
    control$sex <- NULL
    return(control)
}

male=filter(data_jtpa, sex == 1)
female=filter(data_jtpa, sex == 0)
male_control<-datatranform(male,5102)
female_control=datatranform(female,6102)


Y_male=as.matrix(male["earning"])
D_male=as.matrix(male["program_participation"])
Z_male=as.matrix(male["randomized_offer"])
X_male=as.matrix(male_control)

hdm_ori_male50=hdm_parallel(Y_male,D_male,X_male,Z_male,0.5,seq(1000,1500,100),core) 