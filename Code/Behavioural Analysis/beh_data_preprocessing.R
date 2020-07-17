# Preprocesses the behavioural raw data and generates .csv files containing aggreagated information for each of the tasks.
# INPUT: Raw data of the following tasks:
# Knapsack Decision
# Knapsack Optimisation 
# Math/Arithmetic task
# CANTAB tasks
# OUTPUT:
# Knapsack Decision Task:
# KSDec.csv
# KSDec_clean.csv -> Removes non-answers
# Knapsack Optimisation Task:
# KSOpt.csv
# KSOpt_clean.csv -> Removes trials in which time-spent was less than 1s
# KSOpt_time_clean.csv -> KSOpt_clean.csv with the addtional removal of participants that never submitted an answer before time run-out
# KSOpt_clicks.csv-> Data per click. Includes only trials in which there was a click.
# KSOpt_clicks_clean.csv -> Include only those clicks that satisfy the capacity constraint / excludes those trials in which people spent less than one second.
# Math/Arithmetic Task:
# MATH.csv
# MATH_clean.csv -> removes first 3 trials
# CANTAB Task
# CANTAB.csv
# CANTAB_clean.csv -> Adds some measures / filters only completed tasks / Removes some measurements.

# SETUP -------------------------------------------------------------------

## Setting up the basics
library(tidyverse)
#library(ggplot2)
#library(lme4)
#library(stargazer)
#library(knitr)
#library(ggsignif)
#library(plotrix)

#Folder Setup
if(dir.exists("~/Google Drive/My Drive")){
  mother_folder = "~/Google Drive/My Drive/"
} else {
  mother_folder = "~/Google Drive/"
}

project_folder = paste0(mother_folder,"Melbourne/UNIMELB/Research/Complexity Project/")
folder = paste0(project_folder,"KS-IC/Code/Behavioural-Analysis/PNAS")

setwd(folder)
knitr::opts_knit$set(root.dir = folder)

#Input Parameters
#source(paste0(folder,"/Input/Behav_IC/V1.R"))
data_type="Work Data" #Directing to the right folder Raw Data or Work data
experiment="Exp" #e.g. "Pilot","Exp"
versions=c("V4") #c("imV1")

# Import Data Folders
folderData=paste0(project_folder,"KS-IC/Data/Behav/",data_type,"/",experiment,"/")

# Participant Data
fileParticipants = paste0(folderData,"Participants Log.csv")


#Optimisation Data Text files
folderDataOpt=paste0(folderData,"Opt/") 

#Decision Data Text files
folderDataDec=paste0(folderData,"Dec/") 

#Arithmetic Data Text files
folderDataMath=paste0(folderData,"Math/")

#CANTAB Data Text Files
folderDataCantab=paste0(folderData,"CANTAB/")


#Instances Data Text files
# folderInstanceInfo='~/Google Drive/Melbourne/UNIMELB/Complexity Project/Data/Simulations and Solutions/instancesInfo/'
# fileDecInstances = paste0(folderInstanceInfo,'decisionInstancesInfoSubset.csv')
# fileOptInstances = paste0(folderInstanceInfo,'optimisationInstancesInfoSubset.csv')
folderInstanceInfo=paste0(project_folder,"Simulations Data/")
fileDecInstances = paste0(folderInstanceInfo,'KS decision/decisionInstancesInfoSubset.csv')
fileOptInstances = paste0(folderInstanceInfo,'KS optimisation/optimisationInstancesInfoSubset.csv')

fileDecInstancesAll = paste0(folderInstanceInfo,'KS decision/decisionInstancesInfo.csv')

optTaskMaxTime=60

# Output Folder
folder_out = paste0(folderData,"Processed/") 

# Functions
source(paste0(folder, "/DescriptiveFunctions.R"))


# KS Decision ----------------------------------------------------------
#Imports Decision Data
dataAllDec = importTrialInfo(folderDataDec,"dec")

#Adds Phase Transition Dummy Variable to data (1-> in Phase Transition / 0 -> Out of Phase Transition)
dataAllDec$phaseT=as.numeric(dataAllDec$type<=4)

#Adds an experiment version column to the data based on "Participants Log.csv"
partLog0=read.csv(fileParticipants, stringsAsFactors = FALSE)
partLog=data.frame(pID=partLog0$Participant.ID, ExpVersion=partLog0$Experiment.Version,stringsAsFactors = FALSE)
dataAllDec=merge(dataAllDec,partLog, by="pID" )

#Filters to get only the relevant versions
dataAllDec=dplyr::filter(dataAllDec,ExpVersion %in% versions)

#Imports Algorithm-specific complexity measures (MZN porpagations and SAT decisions)
decisionInstanceInfo= read.csv(fileDecInstances, sep=",", stringsAsFactors = FALSE)
names(decisionInstanceInfo)[names(decisionInstanceInfo)=="propagations_MZN"]="propagations"
names(decisionInstanceInfo)[names(decisionInstanceInfo)=="decisions_SAT"]="decisions"
names(decisionInstanceInfo)[names(decisionInstanceInfo)=="problem"]="id"
decisionInstanceInfo=decisionInstanceInfo[,c('id','propagations','decisions')]

#Imports Algorithm-specific complexity measures (All instances with MZN(Gecode))
decInstanceInfoAll = read_csv(fileDecInstancesAll,
                              col_types = cols(weights = col_character(), values = col_character()))

# decInstanceInfoAll = read.csv(fileDecInstancesAll, sep=",", stringsAsFactors = FALSE)

# Generates DataDecProp:= Decision data including expost complexity measures
dataDecProp=merge(dataAllDec,decisionInstanceInfo, by="id")

# Adds sum of weights and sum of values 
dataDecProp=addSumOfValues(dataDecProp)
dataDecProp=addSumOfWeights(dataDecProp)

# Add normalised Capacity and Profit
dataDecProp$nCapacity=dataDecProp$c/dataDecProp$totalWeights
dataDecProp$nProfit=dataDecProp$p /dataDecProp$totalValues

names(dataDecProp)[names(dataDecProp)=="timeSpentAprox"]="timeSpent"

# Add number of solutions (witnesses)
dataDecProp = nSolutions(dataDecProp)

# Add ICexpost
dataInput = dataDecProp
#Add numeric vectors of weights and values to the data Input
dataInput$w_num = purrr::map(dataInput$w,str_to_numVec)
dataInput$v_num = purrr::map(dataInput$v,str_to_numVec)

#Calculate the maximum value acievable given the capacity constraint.
#(i.e. calculate the optimum of the problem in which we ignore the target profit)
rows = 1:nrow(dataInput)
dataInput$opt_profit = purrr::map_dbl(rows,~find_opt_val(dataInput$w_num[[.x]], 
                                                         dataInput$v_num[[.x]], 
                                                         dataInput$c[.x]))

#Calculate distances to optimum and normalise
dataInput$nProfit_opt = dataInput$opt_profit/dataInput$totalValues
dataInput$dist_opt_p = dataInput$p - dataInput$opt_profit
dataInput$n_dist_opt_p = dataInput$dist_opt_p/ dataInput$totalValues
dataInput$ICexpost = abs(dataInput$n_dist_opt_p)#^(0.5)

dataDecProp = dataInput %>% select(-c(w_num,v_num))

##### Save
write_csv(dataDecProp,path = paste0(folder_out ,"KSDec.csv"))

###############  Cleaning Decision Data: ###############
# Filters out those trials in which an answer was not given
nOmitTrials = length(dataDecProp$answer[dataDecProp$answer==2])
NOmitPart= length(unique(dataDecProp$pID[dataDecProp$answer==2]))
dataDecProp_clean=dataDecProp %>% filter(answer!=2)
print(paste(nOmitTrials,"Trials were omitted due to non-answers (from", NOmitPart,"Participants)."))

#Filter additional participants.
#Filter paticipant with 55% accuracy?
participantsToOmit=c()#c("be16")
dataDecProp_clean = dataDecProp_clean %>% filter(!(pID %in% participantsToOmit))
print(paste0(length(participantsToOmit)," participants were omitted in the decision analysis."))

##### Save
write_csv(dataDecProp_clean,path = paste0(folder_out ,"KSDec_clean.csv"))


# KS Optimisation Preprocessing -------------------------------------

dataAllOpt = importTrialInfo(folderDataOpt,"opt")

#Adds an experiment version column to the data based on "Participants Log.csv"
dataAllOpt=merge(dataAllOpt,partLog, by="pID" )

#Filters to get only the relevant versions
dataAllOpt=dplyr::filter(dataAllOpt,ExpVersion %in% versions)

#Adds Phase Transition Dummy Variable to data (1-> in Phase Transition / 0 -> Out of Phase Transition)
dataAllOpt$phaseT=as.numeric(dataAllOpt$type<=4)

#Imports Algorithm-specific complexity measures (MZN porpagations, SAT decisions and Sahni-k)
optInstanceInfo= read.csv(fileOptInstances, sep=",", stringsAsFactors = FALSE)
names(optInstanceInfo)[names(optInstanceInfo)=="propagations_MZN"]="propagations"
names(optInstanceInfo)[names(optInstanceInfo)=="decisions_SAT"]="decisions"
names(optInstanceInfo)[names(optInstanceInfo)=="problem"]="id"
optInstanceInfo=optInstanceInfo[,c('id','propagations','decisions','sahniK')]

#Generates DataOptProp:= Optimisation Task data including expost complexity measures
dataOptProp=merge(dataAllOpt,optInstanceInfo, by="id")

#Adds A column with the number of click away from optimum for each trial
dataOptProp=addItemDistanceFromOpt(dataOptProp)
#Adds sum of weights and sum of values 
dataOptProp=addSumOfValues(dataOptProp)
dataOptProp=addSumOfWeights(dataOptProp)

# Add normalised capacity and profit
dataOptProp$nCapacity=dataOptProp$capacity/dataOptProp$totalWeights
dataOptProp$nProfit=dataOptProp$profitOpt /dataOptProp$totalValues
dataInput$nProfitSel=dataInput$profitSel/dataInput$totalValues

##### Save
write_csv(dataOptProp,path = paste0(folder_out ,"KSOpt.csv"))

###############  Cleaning Optimisation Data: ###############
# Cleaning Optimisation Task Data:
# Filters out the cases in which participants spent less than 1 second in the task
dataOptProp1 = dataOptProp %>% filter(dataOptProp$timeSpent>=1)
numberOfDeletedTrials=dim(dataOptProp)[1]-dim(dataOptProp1)[1]

NOmitPartOpt= length(unique(dataOptProp %>% filter(dataOptProp$timeSpent<1) %>% pull(pID)))

print(paste("Number of Trials Deleted because participants spent less than 1 second in the task:",
            numberOfDeletedTrials,"(from",NOmitPartOpt,"Participants)."))

# Omit those participants that never submitted their answer from the timeSpent analysis
#omitPID = c("be19","be31")
minTimeSpent = dataOptProp1 %>% group_by(pID) %>% summarise(minTime = min(timeSpent)) 
omitPID = minTimeSpent$pID[abs(optTaskMaxTime-minTimeSpent$minTime)<0.1] #Select those participants whose min time spent was optTaskMaxTime
dataOptTime=dataOptProp1[!(dataOptProp1$pID %in% omitPID),]
print(paste0("For time analysis (*effort*) ", length(omitPID)," participants that never submitted their answers were omited."))

##### Save
write_csv(dataOptProp1,path = paste0(folder_out ,"KSOpt_clean.csv"))
write_csv(dataOptTime,path = paste0(folder_out ,"KSOpt_time_clean.csv"))

# KS Click Data (Optimistaion) Preprocessing -------------------------------------

#Imports Data Clicks Data
dataAllClicks = importClicksInfo(folderDataOpt)
dataAllClicks= merge(dataAllClicks,partLog, by="pID")
dataAllClicks=dplyr::filter(dataAllClicks,ExpVersion %in% versions)

timebyNodeData=timeSpentByNodeALL(dataAllClicks,dataOptProp)

# Add propagations of decision problems at each selection
timebyNodeData = join_props_nodes(timebyNodeData,
                                  decInstanceInfoAll)
#timebyNodeData =timebyNodeData1
#Calculate sum of values 
ix = c(1:nrow(timebyNodeData))
vals = purrr::map_dbl(ix,~sum(timebyNodeData$values[[.x]]))
timebyNodeData$totalValues = vals

# Calculate normalised values
timebyNodeData$nProfit = timebyNodeData$nodeValue/timebyNodeData$totalValues
timebyNodeData$nProfit_opt= timebyNodeData$profitOpt/timebyNodeData$totalValues
timebyNodeData$dist_opt_p = timebyNodeData$nodeValue - timebyNodeData$profitOpt
timebyNodeData$n_dist_opt_p = timebyNodeData$dist_opt_p/ timebyNodeData$totalValues

# Calculate ICexpost of each node
timebyNodeData$ICexpost = abs(timebyNodeData$n_dist_opt_p)#^(0.5)

timebyNodeData$ICexpost001 = timebyNodeData$ICexpost^0.01

# Calculate number of better solutions than current selection
#Calculates the sum of weight and values for each feasible node for each instance
allNodesList= w_v_perNode(dataOptProp)

timebyNodeData = nBetterSolutions(timebyNodeData,allNodesList)

# Transform selected items to characters for saving
timebyNodeData$selected_items_str =  as.character(timebyNodeData$selected_items_list)

timebyNodeData = timebyNodeData %>% select(-c(weights,values,selected_items_list))
######## Save
write_csv(timebyNodeData,path = paste0(folder_out ,"KSOpt_clicks.csv"))

###############  Cleaning Click Data: ###############

# Clean click data to include trials that were included in the previous cleaning of Optimisation-trial data
cleaned_trials = dataOptProp1 %>% select(pID,block,trial)
timebyNodeData_clean = left_join(cleaned_trials, timebyNodeData, 
                                 by=c("pID","block","trial"))

# Include only those clicks that satisfy the capacity constraint
timebyNodeData_clean = timebyNodeData_clean %>% filter(capSatisfied)

######## Save
write_csv(timebyNodeData_clean,path = paste0(folder_out ,"KSOpt_clicks_clean.csv"))


# Arithmetic Task ---------------------------------------------------------

# Import Math Data (Mental Arithmetic)
dataMath=importMathInfo(folderDataMath)
dataMath=merge(dataMath,partLog, by="pID" )

# Filter defined versions
dataMath=dplyr::filter(dataMath,ExpVersion %in% versions)

#Generate Correct/Incorrect Variable
dataMath$correct= (dataMath$answer == dataMath$solution)
dataMath$correct[is.na(dataMath$correct)]=FALSE

######## Save
write_csv(dataMath,path = paste0(folder_out,"MATH.csv"))

###############  Cleaning Arithmetic Data: ###############

#Filter First 3 trials of task; they were considered practice trials
dataMath_clean=dataMath %>% filter( trial>= 4 | block != 1 )
write_csv(dataMath_clean,path = paste0(folder_out,"MATH_clean.csv"))


# CANTAB ------------------------------------------------------------------

dataCantab=importCantabInfo(folderDataCantab)
names(dataCantab)[names(dataCantab)=="subject.ID"]="pID"
dataCantab=merge(dataCantab,partLog, by="pID" )
#Subsetting the relevant versions
dataCantab=dplyr::filter(dataCantab,ExpVersion %in% versions)

######## Save
write_csv(dataCantab,path = paste0(folder_out,"CANTAB.csv"))

###############  Cleaning CANTAB Data: ###############

# Subsetting only completed Tests
dataCantabWork=dataCantab[dataCantab$PAL.Recommended.Standard.Extended.Status=="COMPLETED",]
#Variables of interest
vInterest=c("pID","PALFAMS28","PALTEA28","SSPFSL","SWMS","SWMBE4","SWMBE6","SWMBE8","SWMBE12","SWMBE468","SWMTE4","SWMTE6","SWMTE8","SWMTE12","SWMTE468")
dataCantabWork=dataCantabWork[,vInterest]

#Generates new measure for SWM
SWMweights=c(4,3,2,1)
dataCantabWork$SWMTEweightedJP=dataCantabWork$SWMTE4 * SWMweights[1] + dataCantabWork$SWMTE6 * SWMweights[2] +dataCantabWork$SWMTE8 * SWMweights[3] #+ dataCantabWork$SWMTE12 * SWMweights[4]
dataCantabWork$SWMBEweightedJP=dataCantabWork$SWMBE4 * SWMweights[1] + dataCantabWork$SWMBE6 * SWMweights[2] +dataCantabWork$SWMBE8 * SWMweights[3] #+ dataCantabWork$SWMBE12 * SWMweights[4]

#Drops unwanted CANTAB Tasks
dropC=c("SWMBE4","SWMBE6","SWMBE8","SWMBE12","SWMBE468","SWMTE4","SWMTE6","SWMTE8","SWMTE12","SWMTE468")
dataCantabWork=dataCantabWork[,!(names(dataCantabWork) %in% dropC)]

######## Save
write_csv(dataCantabWork,path = paste0(folder_out,"CANTAB_clean.csv"))

