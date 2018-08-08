#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  9 12:54:39 2017

@author: Pablo Franco (jpfranco123@gmail.com)

Generates a .csv file with all the instance information from the solvers

@Dependencies:
instanceSelctionFunctions.py
ks-analysis (For sahni-K calculation)

Generated Instance Files:
    1. Solver specific: e.g. mzn-6-dm-1.csv
    2. General Instance metrics: e.g. metrics-6-dm-1.csv

"""

#import pandas as pd
import importlib
import os
import sys
import numpy as np

folder = '/Users/jfranco1/Google Drive/Melbourne/UNIMELB/Research/Complexity Project/'
os.chdir( folder +'KS-IC/Instance Selection/')

import instanceSelctionFunctions as isf
importlib.reload(isf)

sys.path.append(folder+'Code/carstenKSAnalysis/ks-analysis/')
import libstats as ls



nItems=[6]


#Calculate Sahni-K for each optimisation instance
def sahniK(row):
    v=row.valuesArr#[0]
    w=row.weightsArr#[0]
    c=row.capacity#[0]
    solution_items=row.solution#[0]
    instance = ls.Instance(v, w, c, solution_items)
    instance.solution()
    sk=ls.get_sahni_k(instance)
    return sk


### INPUT ###

#folderOut='/Users/jfranco1/Google Drive/Melbourne/UNIMELB/Complexity Project/Data/Simulations and Solutions/'

### INPUT Decision Variant ###
problemIDDec='-dm-1'
folderInputDec= folder + 'Data/Simulations Data/KS decision/'
folderOutDec= folder + 'KS-IC/Data/Simulations/instanceSelectionOutput/decision/'

### INPUT Optimisation Variant ###
problemIDOpt='-rm-1'
folderInputOpt= folder + 'Data/Simulations Data/KS optimisation/'
folderOutOpt= folder + 'KS-IC/Data/Simulations/instanceSelectionOutput/optimisation/'


### DATA UPLOAD ###

# Decision
dataDec=isf.importSolvedInstancesBothSolvers(nItems,folderInputDec,problemIDDec)
dataDec=dataDec[0]


# Adds other relevant measures
dataDec['lnPC'] = np.log(dataDec.nprofit/dataDec.ncapacity)

dataDec['lnPC_IC'] = np.abs(dataDec['lnPC'] - 0.4)

dataDec.to_csv(folderOutDec+'decisionInstancesInfo.csv',index=False)


# Optimisation
dataOpt=isf.importSolvedInstancesBothSolvers(nItems,folderInputOpt,problemIDOpt)
dataOpt=dataOpt[0]
dataOpt.rename(columns={'solution_MZN': 'solution'}, inplace=True)



dataOpt=isf.calculateOptimum(dataOpt)
dataOpt=dataOpt.reset_index(drop=True)
dataOpt['sahniK'] = dataOpt.apply(lambda row: sahniK(row), axis=1)

dataOpt.to_csv(folderOutOpt+'optimisationInstancesInfo.csv',index=False)
