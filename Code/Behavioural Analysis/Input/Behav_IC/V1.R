data_type="Work Data" #Directing to the right folder Raw Data or Work data
experiment="Exp" #e.g. "Pilot","Exp"
versions=c("V4") #c("imV1")

# Import Data Folders
folderData=paste0("~/Google Drive/Melbourne/UNIMELB/Research/Complexity Project/KS-IC/Data/Behav/",data_type,"/",experiment,"/")

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
folderInstanceInfo='~/Google Drive/Melbourne/UNIMELB/Research/Complexity Project/Simulations Data/'
fileDecInstances = paste0(folderInstanceInfo,'KS decision/decisionInstancesInfoSubset.csv')
fileOptInstances = paste0(folderInstanceInfo,'KS optimisation/optimisationInstancesInfoSubset.csv')

fileDecInstancesAll = paste0(folderInstanceInfo,'KS decision/decisionInstancesInfo.csv')

optTaskMaxTime=60

