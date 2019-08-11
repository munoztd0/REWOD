#!/usr/bin/env python
# coding: utf-8

"""
Created on Mon Jun 10 14:13:20 2019

@author: David Munoz

takes the condition name as input 
"""

def covariate (cond):
    # data analysis and wrangling
    import pandas as pd
    import numpy as np
    import os
    from pathlib import Path
    
    
    #addpath
    home = str(Path.home())


    #declare variables
    GLM = ("GLM-04")
    s = ("01", "02", "03", "04", "05", "06", "07", "09", "10", "11", "12", "13","14", "15", "16", "17","18", "20", "21", "22","23", "24","25", "26")
    taskDIR = ("PIT")

    df1 = []
    df2 = []
    df3 = []
    df4 = []

    dfsubj = []

    df01 = pd.DataFrame()
    df02 = pd.DataFrame()
    df03 = pd.DataFrame()
    df04 = pd.DataFrame()


    for i in s:
        subj = 'sub-' + i
        covpath = home + '/REWOD/DERIVATIVES/ANALYSIS/' + taskDIR + '/' + GLM + '/' + subj + '/timing/'
        cov_Base = pd.read_table(covpath + GLM + '_task-PIT_CS_Baseline.txt',sep='\t', header=None)
        cov_minus = pd.read_table(covpath + GLM + '_task-PIT_CS_CSm.txt',sep='\t', header=None)
        cov_plus = pd.read_table(covpath + GLM + '_task-PIT_CS_CSp.txt',sep='\t', header=None)

        dfsubj = np.append(dfsubj, i)

        CSp_CSm = cov_plus[2] - cov_minus[2]
        df1 = np.append(df1, CSp_CSm.mean())


        CSp_Baseline = cov_plus[2] - cov_Base[2]
        df2 = np.append(df2, CSp_Baseline.mean())


        CSp_CSmandBaseline = cov_plus[2] - (cov_minus[2] + cov_Base[2])/2
        df3 = np.append(df3, CSp_CSmandBaseline.mean())


        CSm_Baseline = cov_minus[2] - cov_Base[2]
        df4 = np.append(df4, CSm_Baseline.mean())

    def helper(dfXX,dfX):
        dfXX[0] = dfsubj
        dfXX[1] = dfX - dfX.mean()
        dfXX.columns = ['subj', cond]
        dfXX[cond] = dfXX[cond].rank(method='first')


    helper(df01,df1)
    helper(df02,df2)
    helper(df03,df3)
    helper(df04,df4)
        
        
    
    os.chdir(home +'/REWOD/DERIVATIVES/ANALYSIS/' + taskDIR + '/' + GLM + '/group_covariates')
    df01.to_csv('CSp-CSm_' + cond + '_rank.txt',sep='\t', index=False)
    df02.to_csv('CSp-Baseline_' + cond + '_rank.txt',sep='\t', index=False)
    df03.to_csv('CSp-CSm&Baseline_' + cond + '_rank.txt',sep='\t', index=False)
    df04.to_csv('CSm-Baseline_' + cond + '_rank.txt',sep='\t', index=False)
    
    print("covariates done")