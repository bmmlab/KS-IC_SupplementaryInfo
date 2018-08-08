#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep  3 14:34:03 2017

@author: Pablo Franco (jpfranco123@gmail.com)

Instance Random Selection and order Randomisation (KS-IC project)

@Dependencies:
instanceSelctionFunctions.py

@Input:
Generated Instance Files:
    1. Solver specific: e.g. mzn-6-dm-1.csv
    2. General Instance metrics: e.g. metrics-6-dm-1.csv

@Output:
1. Instance .txt files structured for Knapsack Unity game named in order i1.txt, i2.txt,...
A. Decision: Instances:
weights:[48,34,43,32,20,44]
values:[26,24,34,47,17,11]
capacity:90
profit:100
problemID:98-0.41-0.63
instanceType:1
solution:0

B. Optimistion Instances:
weights:[29,8,19,41,40,32]
values:[47,40,39,44,23,11]
capacity:69
problemID:144-0.41
instanceType:1
profitAtOptimum:126
capacityAtOptimum:56
solutionItems:[1,1,1,0,0,0]

2. param2.txt instance information files with order ransomisation
numberOfTrials:9
numberOfBlocks:2
numberOfInstances:18
instanceRandomization:[10,15,11,1,14,18,4,5,12,3,7,13,6,17,8,16,2,9]


"""

import pandas as pd
import importlib
import os

#Project folder
folder = '/Users/jfranco1/Google Drive/Melbourne/UNIMELB/Research/Complexity Project/'
os.chdir( folder +'KS-IC/Instance Selection/')

#Imports all of the required functions
import instanceSelctionFunctions as isf
importlib.reload(isf)

### General Input ###

#Files to be uploaded with respect to the number of items
nItems=[6]#,15,20,25,30]

#Number of order randomizations (i.e. number of param2.txt files)
nOrderRandomizations=30



### INPUT Decision Variant ###
problemIDDec='-dm-1'
folderInputDec= folder + 'Data/Simulations Data/KS decision/'
folderOutDec= folder + 'KS-IC/Data/Simulations/instanceSelectionOutput/decision/'

#number of bins to allocate ncapacity and nprofits to bins
nbins=20

#Normalized capacity from which to sample instances
nCap=0.4

#Normalized profit from which to sample IN-Phase-Transition instanes
nProf=0.6

#Normalized profit from which to sample OUT-OF-Phase-Transition instanes
nProfNO=0.85
nProfYES=0.35

#How to categorize easy/hard IN-Phase-Transition
quantileLow=0.5
quantileUpper=0.5

#bN blocks of tN trials
#requires tN to be multiple of nTypes
tNDec=24
bNDec=3
nTypesDec=6


### INPUT Optimisation Variant ###
problemIDOpt='-rm-1'
folderInputOpt= folder + 'Data/Simulations Data/KS optimisation/'
folderOutOpt= folder + 'KS-IC/Data/Simulations/instanceSelectionOutput/optimisation/'

#bN blocks of tN trials
#requires tN to be multiple of the number of instances types there are
tNOpt=9
bNOpt=2
#An array with possible types
possibleTypesOpt=[1,3,5]


### Decision Task instance selection

## DATA UPLOAD
dataMZN=isf.importSolvedInstances(nItems,'mzn',folderInputDec,problemIDDec)

#Allocates ncapacity and nprofits to bins (BINS are defined with repect to left edge (i.e. nCap=0.5 means nCap in [0.5,0.5+binSize] ))
### Add instance type ([1,6]) tp data according to the inputs
#nProf: Normalized profit to sample within phase transition
#nCap: Normalized profit to sample within phase transition
#nProfNO: Normalized profit to sample outside the phase transition where there is NO solution
#nProfYes: Normalized profit to sample outside the phase transition where there IS a solution
#quantileLow: Easy instances at (nProf, nCap) are those below the quantileLow
#quantileHigh: Hard instances at (nProf, nCap) are those above the quantileHigh
## OutPut: 1=nProf-easy-NoSolution 2==nProf-easy-Solution 3=nProf-hard-NoSolution
##  3=nProf-hard-Solution 5=nProfNo-NOSolution 6=nProfYES-Solution

####################
## Taking instance categorized according to MZN

dataM=dataMZN[0]
dataM=isf.binCapProf(dataM,nbins)
dataM=isf.addInstanceType(dataM,nCap,nProf,nProfNO,nProfYES,quantileLow,quantileUpper,'propagations')
dataDec=dataM

## SAMPLING of Instances

# Samples randomly from each instance-type sampleSizePerBin
# Output: list of sublists. Each sublist has sampleSizePerBin size with the instances ID
# Sampling is done withOUT replacement
sizePerBin=int(tNDec*bNDec/(nTypesDec+2))
#Total number (Including all blocks) instances per Type
sampleSizePerBin=[sizePerBin,sizePerBin,sizePerBin,sizePerBin,2*sizePerBin,2*sizePerBin]
possibleTypesDec=range(1,nTypesDec+1)
sampleProblems=isf.sampleInstanceProblems3(dataDec,sampleSizePerBin,possibleTypesDec)

#Exports all the instance files in the sampleProblems list
instanceNumber=1
for k in isf.flatten(sampleProblems):
    iw,iv,ic,ip,instanceType,solution=isf.extractInstance(dataDec,k)
    isf.exportInstance(iw,iv,ic,ip,k,instanceType,solution,folderOutDec,instanceNumber)
    instanceNumber=instanceNumber+1

## INSTANCE ORDER GENERATION and param2.txt export

# Generates the instance randomization order for bN blocks of tN trials for nTypes instance types
nInstances=tNDec*bNDec
for i in range(0,nOrderRandomizations):
    instanceOrder=isf.generateInstanceOrder(tNDec, bNDec,sampleSizePerBin)
    isf.exportTaskInfo(tNDec,bNDec,instanceOrder,nInstances,folderOutDec,i) #Exports 'param2.txt' with the required input for the task

sampleProblemsDec=sampleProblems


### Optimisation Task instance selection

### Data Upload
dataMZN=isf.importSolvedInstances(nItems,'mzn',folderInputOpt,problemIDOpt)

### Instance Type Attachment
dataOpt=dataMZN[0]

# Calculates nprofit for the optimization case (i.e. the optimum normalized profit)
dataOpt=isf.calculateOptimum(dataOpt)

# Merges Optimization data and relevant decision columns.
# Aim: Add instance type from decision Problem to Optimization Problem
# Warning: Here each optimization problem is mapped into many decion problems
dataOptDec=isf.mergeOptDec(dataDec, dataOpt)


#Keep only those instances where the nProfit of Decision problem (not binned) is the closest to nprofitOpt s.t. nprofitNoBinDec>nprofitOpt
#This gives us instances that have solution: NO
dataOptDec=isf.removeRepeatedOptInstances2(dataOptDec)


### Sample Instances

nTypesOpt=len(possibleTypesOpt)
sizePerBin=int(tNOpt*bNOpt/(nTypesOpt))
sampleSizePerBin=[sizePerBin,sizePerBin,sizePerBin]
#sampleSizePerBin=int(tNOpt*bNOpt/nTypesOpt)

# Samples randomly from each instance-type sampleSizePerBin
# Output: list of sublists. Each sublist has sampleSizePerBin size with the instances ID
sampleProblems=isf.sampleInstanceProblems3(dataOptDec,sampleSizePerBin,possibleTypesOpt)

#Exports all the instance files in the sampleProblems list
instanceNumber=1
for k in isf.flatten(sampleProblems):
    iw,iv,ic,instanceType,pOpt,cOpt,itemsOpt=isf.extractInstanceOpt(dataOptDec,k)
    isf.exportInstanceOpt(iw,iv,ic,k,instanceType,folderOutOpt,instanceNumber,pOpt,cOpt,itemsOpt)
    instanceNumber=instanceNumber+1

## INSTANCE ORDER GENERATION and param2.txt export

# Generates the instance randomization order for bN blocks of tN trials for nTypes instance types
nInstances=tNOpt*bNOpt
for i in range(0,nOrderRandomizations):
    instanceOrder=isf.generateInstanceOrder(tNOpt, bNOpt,sampleSizePerBin)
    isf.exportTaskInfo(tNOpt,bNOpt,instanceOrder,nInstances,folderOutOpt,i)#Exports 'param2.txt' with the required input for the task

sampleProblemsOpt=sampleProblems
