import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 




ci=pd.read_csv("nftaci.csv",sep=',')
q10,g10=np.array(ci.nfta10.dropna()),np.array(ci.iloc[:,0].dropna())*63522.5
q25,g25=np.array(ci.nfta25.dropna()),np.array(ci.iloc[:,6].dropna())*63522.5
q50,g50=np.array(ci.nfta50.dropna()),np.array(ci.iloc[:,16].dropna())*63522.5
q75,g75=np.array(ci.nfta75.dropna()),np.array(ci.iloc[:,26].dropna())*63522.5
q90,g90=np.array(ci.nfta90.dropna()),np.array(ci.iloc[:,32].dropna())*63522.5



def ntfa10(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.1 GMM Objective Function")
    plt.xlim(-100,6000)
    plt.ylim(-2.5,50)
    plt.plot([4514.45, 4514.45,], [-5,3.76244,], 'k-', linewidth=1.25)
    plt.plot([2753.23, 2753.23,], [-5,3.76244,], 'k-', linewidth=1.25)
    plt.legend(loc="upper left")
    plt.title("Quantile 0.1 Impact Of P401 On Net Financial Assets")
    plt.savefig('nfta10.png')
ntfa10(g10,q10)    


def ntfa25(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.25 GMM Objective Function")
    plt.xlim(1000,5500)
    plt.ylim(-2.5,50)
    plt.plot([3604.84, 3604.84,], [-5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([2485.89, 2485.89,], [-5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.25 Impact Of P401 On Net Financial Assets")
    plt.savefig('nfta25.png')
ntfa25(g25,q25) 
def ntfa50(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.5 GMM Objective Function")
    plt.xlim(4000,9000)
    plt.ylim(-2.5,50)
    plt.plot([7578.63, 7578.63,], [-5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([5522.18, 5522.18,], [-5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.5 Impact Of P401 On Net Financial Assets")
    plt.savefig('nfta50.png')
ntfa50(g50,q50) 

def ntfa75(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.75 GMM Objective Function")
    plt.xlim(8000,19000)
    plt.ylim(-2.5,50)
    plt.plot([18489.9, 18489.9,], [-2.5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([13987.9, 13987.9,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.75 Impact Of P401 On Net Financial Assets")
    plt.savefig('nfta75.png')
ntfa75(g75,q75) 

def ntfa90(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.9 GMM Objective Function")
    plt.xlim(16000,33000)
    plt.ylim(-.5,10)
    plt.plot([31937.5, 31937.5,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.plot([19657.3, 19657.3,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.9 Impact Of P401 On Net Financial Assets")
    plt.savefig('nfta90.png')     
ntfa90(g90,q90) 

ci=pd.read_csv("twci.csv",sep=',')
q10,g10=np.array(ci.nfta10.dropna()),np.array(ci.iloc[:,0].dropna())*111529.7
q25,g25=np.array(ci.nfta25.dropna()),np.array(ci.iloc[:,6].dropna())*111529.7
q50,g50=np.array(ci.nfta50.dropna()),np.array(ci.iloc[:,16].dropna())*111529.7
q75,g75=np.array(ci.nfta75.dropna()),np.array(ci.iloc[:,26].dropna())*111529.7
q90,g90=np.array(ci.nfta90.dropna()),np.array(ci.iloc[:,32].dropna())*111529.7


def tw10(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.1 GMM Objective Function")
    plt.xlim(0,6000)
    plt.ylim(-5,125)
    plt.plot([3200, 3200,], [-5,3.76882,], 'k-', linewidth=1.25)
    plt.plot([1377.5, 1377.5,], [-5, 3.76882,], 'k-', linewidth=1.25)
    plt.legend(loc="upper left")
    plt.title("Quantile 0.1 Impact Of P401 On Total Wealth")
    plt.savefig('tw10.png')     
tw10(g10,q10) 
def tw25(a10,q25):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q25,label="Quantile 0.25 GMM Objective Function")
    plt.xlim(1000,7000)
    plt.ylim(-5,125)
    plt.plot([4351.88, 4351.88,], [-5, 3.73697,], 'k-', linewidth=1.25)
    plt.plot([2808.87, 2808.87,], [-5, 3.73697,], 'k-', linewidth=1.25)
    plt.legend(loc="upper center")
    plt.title("Quantile 0.25 Impact Of P401 On Total Wealth ")
    plt.savefig('tw25.png')  
tw25(g25,q25) 

def tw50(a10,q50):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q50,label="Quantile 0.5 GMM Objective Function")
    plt.xlim(5500,11000)
    plt.ylim(-2.5,50)
    plt.plot([6314.52, 6314.52,], [-5, 3.73697,], 'k-', linewidth=1.25)
    plt.plot([8981.85, 8981.85,], [-5, 3.80952,], 'k-', linewidth=1.25)
    plt.legend(loc="upper center")
    plt.title("Quantile 0.5 Impact Of P401 On Total Wealth ")
    plt.savefig('tw50.png')
tw50(g50,q50) 
def tw75(a75,q75):
    critical=np.repeat(3.84146,len(a75))
    fig, ax = plt.subplots()
    ax.plot(a75,critical,linestyle='--',label="critical point")
    ax.plot(a75,q75,label="Quantile 0.75 GMM Objective Function")
    plt.xlim(7000,22000)
    plt.ylim(-2.5,100)
    plt.plot([18770.2, 18770.2,], [-5, 3.65666,], 'k-', linewidth=1.25)
    plt.plot([11802.4, 11802.4,], [-5, 3.93398,], 'k-', linewidth=1.25)
    plt.legend(loc="upper right")
    plt.title("Quantile 0.75 Impact Of P401 On Total Wealth ")
    plt.savefig('tw75.png')
tw75(g75,q75) 

def tw90(a75,q75):
    critical=np.repeat(3.84146,len(a75))
    fig, ax = plt.subplots()
    ax.plot(a75,critical,linestyle='--',label="critical point")
    ax.plot(a75,q75,label="Quantile 0.9 GMM Objective Function")
    plt.xlim(6000,28000)
    plt.ylim(-2.5,20)
    plt.plot([9048.39, 9048.39,], [-5, 3.90422,], 'k-', linewidth=1.25)
    plt.plot([25782.3, 25782.3,], [-5, 3.78247,], 'k-', linewidth=1.25)
    plt.legend(loc="upper right")
    plt.title("Quantile 0.9 Impact Of P401 On Total Wealth ")
    plt.savefig('tw90.png')
tw90(g90,q90) 

def mainTW():
    result=np.array([2342,2899,2899,3680,4461,5241,6134,6803,7807,8253,9480,10706,11599,14721,16952,15168,17844])
    x=np.arange(0.1,0.95,0.05)
    fig, ax = plt.subplots()
    ax.plot(x,result, '-o', ms=8, lw=3, alpha=1, mfc='orange',label="Distribution effct of P401 on TW")
    ax.legend(loc="upper left")
    plt.savefig('tw.png')
mainTW()    

def mainNFTA():
    result=np.array([3493,3112,2985,3112,3176,3684,4700,5462,6606,7559,9083,11307,13847,16134,20390,25980,26234])
    x=np.arange(0.1,0.95,0.05)
    fig, ax = plt.subplots()
    ax.plot(x,result, '-o', ms=8, lw=3, alpha=1, mfc='orange',label="Distribution effct of P401 on NFTA")
    ax.legend(loc="upper left")
    plt.savefig('nfta.png')
mainNFTA()   