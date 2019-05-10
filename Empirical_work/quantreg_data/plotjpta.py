import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

ci=pd.read_csv("jpta_male.csv",sep=',')

grid=np.array(ci.grid)
q15,q25,q50,q75,q85=np.array(ci.q15),np.array(ci.q25),np.array(ci.q50),np.array(ci.q75),np.array(ci.q85)

def ntfa10(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.15 GMM Objective Function")
    plt.xlim(-3000,3000)
    plt.ylim(-2.5,50)
    plt.plot([701.613,701.613,], [-5,3.76244,], 'k-', linewidth=1.25)
    plt.plot([-362, -362,], [-5,3.76244,], 'k-', linewidth=1.25)
    plt.legend(loc="upper left")
    plt.title("Quantile 0.15 subsidized training on male trainee earnings")
    #plt.show()
    plt.savefig('jpta_male15.png')
index=np.where(q15>10**5)
q15=np.delete(q15, index)
grid15=np.delete(grid, index)
ntfa10(grid15,q15)   

def ntfa25(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.25 GMM Objective Function")
    plt.xlim(-2000,3000)
    plt.ylim(-2.5,50)
    plt.plot([1477, 1477,], [-5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([-538, -538,], [-5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.25 subsidized training on male trainee earnings")
    #plt.show()
    plt.savefig('jpta_male25.png')
ntfa25(grid,q25) 

def ntfa50(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.5 GMM Objective Function")
    plt.xlim(-3000,6000)
    plt.ylim(-2.5,50)
    plt.plot([-1451, -1451,], [-5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([2919, 2919,], [-5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.5 subsidized training on male trainee earnings")
    #plt.show()
    plt.savefig('jpta_male50.png')
index=np.where(q50>10**5)
q50=np.delete(q50, index)
grid50=np.delete(grid, index)    
ntfa50(grid50,q50) 


def ntfa75(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.75 GMM Objective Function")
    plt.xlim(-3000,8000)
    plt.ylim(-2.5,25)
    plt.plot([-988, -988,], [-2.5,3.92045,], 'k-', linewidth=1.25)
    plt.plot([5867, 5867,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.75 subsidized training on male trainee earnings")
    #plt.show()
    plt.savefig('jpta_male75.png')
ntfa75(grid,q75) 


def ntfa90(a10,q10):
    critical=np.repeat(3.84146,len(a10))
    fig, ax = plt.subplots()
    ax.plot(a10,critical,linestyle='--',label="critical point")
    ax.plot(a10,q10,label="Quantile 0.85 GMM Objective Function")
    plt.xlim(-3000,8000)
    plt.ylim(-2.5,15)
    plt.plot([-50, -50,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.plot([6580, 6580,], [-2.5,3.77841,], 'k-', linewidth=1.25)
    plt.legend(loc="upper")
    plt.title("Quantile 0.85 subsidized training on male trainee earnings")
    #plt.show()
    plt.savefig('jpta_male85.png')     
ntfa90(grid,q85) 


QTE=pd.read_csv("jpta_hdm_QTE.csv",sep=',')
male=np.delete(np.array(QTE.iloc[0]), 0)
female=np.delete(np.array(QTE.iloc[1]), 0)
x=np.arange(0.1,0.95,0.05)
fig, ax = plt.subplots()
ax.plot(x,male, '-o', ms=8, lw=3, alpha=1, mfc='orange',label="Distribution effct of subsidized training on male trainee earnings")
ax.legend(loc="upper left")
plt.savefig('male.png')

