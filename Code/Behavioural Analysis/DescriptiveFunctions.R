# Behavioural Data Analsysis Functions

library(dplyr)
library(reshape)

se <- function(x) sqrt(var(x)/length(x))


# Map instance types to their corresponding names
typeNameF= function(types){
  typeNames=rep("",length(types))
  i=1
  for (type in types){
    if(type==1){
      typeName= 'In PT - No - Easy'
    }else if(type==2){
      typeName= 'In PT - Yes - Easy'
    }else if(type==3){
      typeName= 'In PT - No - Hard'
    }else if(type==4){
      typeName= 'In PT - Yes - Hard'
    }else if(type==5){
      typeName= 'Out PT - No'
    }else if(type==6){
      typeName= 'Out PT - Yes'
    }else{
      typeName= 'None Defined'
    }
    typeNames[i]=typeName
    i=i+1
  }
  return(typeNames)
}

# Import and merge DATA accross participants Decision + Optimisation (Trial Info + Instance Info)
##Input:
# folderData=folderDataDec
# pFix=c("p1","p2","op1","op2")
# #"dec" or "opt"
# task="dec"
##Output
# dataAll := data with trialInfo and instanceInfo for all aprticipants in the folder
importTrialInfo = function(folderData,task,pFix=c()){
  #Obtain all participant ID's in the folder 
  trialInfoFiles = list.files(folderData,pattern="*TrialInfo.txt$")
  pIDs=sapply(strsplit(trialInfoFiles,"_"),function(x) x[1])
  
  dataAll=data_frame()
  for (pID in pIDs){
    #Import Trialinfo and Instance Info
    trialInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*TrialInfo.txt$"))
    instanceInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*InstancesInfo.txt$"))
    #print(pID)
    trialInfo=read.csv(paste0(folderData,trialInfoFile),skip=1, sep=";", stringsAsFactors = FALSE)
    instanceInfo=read.csv(paste0(folderData,instanceInfoFile),skip=1, sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    #If the instancesInfo file is old and neds fixing, fix it...
    # if (pID %in% pFix){
    #   #Fix InstanceInfo File
    #   instanceInfo=read.csv(paste0(folderData,instanceInfoFile),skip=1, sep=";", header=FALSE, stringsAsFactors = FALSE)
    #   instanceInfo$instanceNumber=as.integer(sapply(strsplit(instanceInfo$V1,":"),function(x) x[2])  )
    #   instanceInfo$type=as.integer(sapply(strsplit(instanceInfo$V7,"="),function(x) x[2])  )
    # }
    
    #If the trialInfo file is old and needs fixing, fix it...
    if (pID %in% pFix){
      #Add correct column to trialInfo if we are in the optimization variant
      if(task=="opt"){
        trialInfo$correct = (trialInfo$profitSel==trialInfo$profitOpt) & (trialInfo$capacitySel <= trialInfo$capacity)
      }
    }
    
    #Merge Both instanceInfo and trialInfo
    trialInfoM=merge(trialInfo,instanceInfo, by="instanceNumber")
    
    if(task=="opt"){
      if(all(trialInfoM$c==trialInfoM$capacity) && all(trialInfoM$pOpt==trialInfoM$profitOpt) && all(trialInfoM$cOpt==trialInfoM$capacityOpt) && 
         all(trialInfoM$itemsOpt.x==trialInfoM$itemsOpt.y)){
        trialInfoM$itemsOpt = trialInfoM$itemsOpt.x
        trialInfoM=trialInfoM %>% select(-one_of("c","cOpt","pOpt","itemsOpt.x","itemsOpt.y"))
      } else{
        stop("Issue when merging instancesInfo and trialInfo for Optimization ")
      }
    }
    #Add Column with participant ID
    trialInfoM=cbind(pID=pID,trialInfoM)
    
    #Bind new participant to dataAll
    dataAll=rbind(dataAll,trialInfoM)
  }
  return(dataAll)
}

##Imports Math Info
importMathInfo = function(folderData){
  trialInfoFiles = list.files(folderData,pattern="*TrialInfo.txt$")
  pIDs=sapply(strsplit(trialInfoFiles,"_"),function(x) x[1])
  
  dataAll=data_frame()
  for (pID in pIDs){
    #Import Trialinfo and Instance Info
    trialInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*TrialInfo.txt$"))
    #print(pID)
    trialInfo=read.csv(paste0(folderData,trialInfoFile),skip=1, sep=";", stringsAsFactors = FALSE)
    
    #Add Column with participant ID   
    trialInfo=cbind(pID=pID,trialInfo)
    
    #Bind new participant to dataAll
    dataAll=rbind(dataAll,trialInfo)
  }
  return(dataAll)
}

##Imports CANTAB Info
importCantabInfo = function(folderData){
  trialInfoFile = list.files(folderData,pattern="^RowBySession")
  trialInfo=read.csv(paste0(folderData,trialInfoFile), sep=",", stringsAsFactors = FALSE)
  
  return(trialInfo)
}

##Imports Clicks Data for the knapsack optimisation task
importClicksInfo = function(folderData){
  clickFiles = list.files(folderData,pattern="*Opt_Clicks.txt$")
  #clicksInfo=read.csv(paste0(folderDataOpt,"be01_04 September, 2017, 16-11_Opt_Clicks.txt"),skip=2, sep=";", stringsAsFactors = FALSE)
  pIDs=sapply(strsplit(clickFiles,"_"),function(x) x[1])
  dataAll=data.frame()
  
  for (pID in pIDs){
    #Import Clicks Info
    clicksFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*Opt_Clicks.txt$"))
    #print(pID)
    clicksInfo=read.csv(paste0(folderData,clicksFile),skip=2, sep=";", stringsAsFactors = FALSE)
    
    #Add Column with participant ID   
    clicksInfo=cbind(pID=pID,clicksInfo)
    
    #Bind new participant to dataAll
    dataAll=rbind(dataAll,clicksInfo)
  }
  
  dataAll$pID=as.character(dataAll$pID)
  return(dataAll)
}




# Main data Description
# #Input
# dataAll := Data for all participants with trialInfo and instanceInfo (output of importTrialInfo)
# #Output
# A list with table with data summarized in different ways
summaryData= function(dataAll){
  #basic Info on correct answers (Opt and Dec)
  mean(dataAll$correct)
  sum(dataAll$correct)
  #Correct answers by Instance Type
  #tapply(dataAll$correct, dataAll$type, mean)
  
  #Correct answers by Instance Type and Participant
  groups1=dataAll %>% group_by(pID,type)
  g1=summarise(groups1, correctP=mean(correct))
  g1=cast(g1,formula=pID~type,value="correctP")
  
  #Correct answers by Participant
  groups2=dataAll %>% group_by(pID)
  g2=summarise(groups2, mean(correct),payoff1AUD=sum(correct))
  
  #Correct answers by Instance Type
  groups3=dataAll %>% group_by(type)
  g3=summarise(groups3, accuracy=mean(correct), se=se(correct))
  
  #Correct answers by Instance
  #groupsI=dataAll %>% group_by(instanceNumber)
  groupsI=dataAll %>% group_by(id)
  gI=summarise(groupsI, correctP=mean(correct))
  
  #For Optimization:
  #Mean Time Spent per instance
  mean(dataAll$timeSpent)
  t1=summarise(groups1, timeSpent=mean(timeSpent))
  t1=cast(t1,formula=pID~type,value="timeSpent")
  
  #Output Info Storage
  summary=list(g1,g2,g3,gI,t1)
  return(summary)
}


#' For KS-Optimisation Analysis: How many items have to be selected/unselected to get to the optimum
#' @param selectedS A string specifying the items selected: eg \"1,0,0,1,1,1\"
#' @param optimumS A string specifying the items in the optimum knapsack: eg \"1,0,0,1,1,1\"
#'
#' @return Number of clicks away from the optimum: i.e. How many items have to be selected/unselected to get to the optimum
#' @export
#'
#' @examples
distanceFromOpt = function(selectedS,optimumS){
  selected=as.numeric(strsplit(selectedS,",")[[1]])
  optimum=as.numeric(strsplit(optimumS,",")[[1]]) 
  itemDistanceFromOpt=sum(abs(selected-optimum))
  return(itemDistanceFromOpt)
}

#' For KS-Optimisation Analysis: Add a colum to the KS Optimisation data representing the number of clicks from the optimum
#'
#' @param dataOptProp Knapsack Data optimisation data
#'
#' @return Input data with one additional column representing the number of clicks from the optimum (see distanceFromOpt())
#' @export
#'
#' @examples
addItemDistanceFromOpt = function(dataOptProp){
  itemDistanceFromOpt=vector(mode="numeric", length=length(dataOptProp$itemsSelected))
  for (i in 1:length(dataOptProp$itemsSelected)){
    #The if statement ensures that if there is more than one solution the distance from optimum is correctly 0
    if(dataOptProp$correct[i]==1){
      itemDistanceFromOpt[i]=0
    } else{
      itemDistanceFromOpt[i]=distanceFromOpt(dataOptProp$itemsSelected[i],dataOptProp$itemsOpt[i])  
    }
    
  }
  dataOptProp$itemDistanceFromOpt=itemDistanceFromOpt
  return(dataOptProp)
}

#' For KS Analysis: Add a colum to the KS data representing the sum of weights
#'
#' @param dataInput Knapsack Data, either decision or optimisation with a 'w' column
#'
#' @return dataInout with one additional column representing the sum of weights (totalWeights)
#' @export
#'
#' @examples
addSumOfWeights = function(dataInput){
  totalWeights=rep(0,length(dataInput$w))
  i=1
  for (weigths in dataInput$w) {
    totalWeights[i]=sum(as.integer(strsplit(weigths,',')[[1]]))
    i=i+1
  }
  dataInput$totalWeights=totalWeights
  return(dataInput)
}

#' For KS Analysis: Add a colum to the KS data representing the sum of weights
#'
#' @param dataInput Knapsack Data, either decision or optimisation with a 'w' column
#'
#' @return dataInout with one additional column representing the sum of weights (totalWeights)
#' @export
#'
#' @examples
addSumOfValues = function(dataInput){
  totalValues=rep(0,length(dataInput$v))
  i=1
  for (values in dataInput$v) {
    totalValues[i]=sum(as.integer(strsplit(values,',')[[1]]))
    i=i+1
  }
  dataInput$totalValues=totalValues
  return(dataInput)
}



#' Generates a list with all possible subsets of a vector of size n
#'
#' @param n
#'
#' @return A list of vectors of possible subsets of a vector of size n. The output vectors are boolean vecotrs of size n (e.g. [False,True,False] for n=3)
#'
#' @examples
powerSet <- function(n) { 
  #n <- length(set)
  masks <- 2^(0:(n-1))
  #lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
  listOfSets=lapply( (1:2^n)-1, function(u) { bitwAnd(u, masks) != 0 } )
  return(listOfSets)
}




#' Adds a column to dataInput (Decision knapsack) with the number of solutions to each instance
#'
#' @param dataInput (Decision instances)
#'
#' @return dataInput with a new column (nSolutions) representting the number of item combinations that satisfy the constraints
#' @export
#'
#' @examples
nSolutions <- function(dataInput){
  #Copy Data Input for later use
  dataInput2=dataInput
  #Extracts a Unique list of instance with weights, value, capacity and profit
  dataInput=dataInput[c('w','v','c','p','id')]
  dataInput=unique(dataInput)
  
  #Generate all the possible subsets of a set of size 6
  p_set=powerSet(6)
  
  #Generate number of solutions for each instance
  dataInput$nSolutions=-1
  nInstances = dim(dataInput)[1]
  for(i in c(1:nInstances)){
    weights=as.integer(strsplit(dataInput$w[i],',')[[1]])
    values=as.integer(strsplit(dataInput$v[i],',')[[1]])
    wCap=dataInput$c[i]
    vProf=dataInput$p[i]
    
    nSolutions=0
    for (subset in p_set){
      if(sum(weights[subset])<=wCap & sum(values[subset])>=vProf){
        nSolutions=nSolutions+1
      }
    }
    dataInput$nSolutions[i]=nSolutions
  }
  
  #Merges the unique-instance-only information dataframe (dataInput) with the Big decision data Frame with all the information
  dataInput2=merge(dataInput2,dataInput,by=c("id","v","w","p","c"))
  
  return(dataInput2)
}


#' Generates a dataframe with all the information by Node (Trial info and Time spent at each selection of items)
#'
#' @param dataAllClicks 
#' @param dataOpt 
#'
#' @return timebyNodeData - Dataframe with all the information by Node for all trials and participants
#' @export
#'
#' @examples
timeSpentByNodeALL <- function(dataAllClicks,dataOpt){
  dataInput = dataAllClicks
  loopingParameters1 = dataInput %>% dplyr::select(c(pID,block,trial)) %>% distinct()
  dataOpt$pID=as.character(dataOpt$pID)
  loopingParameters2 = dataOpt %>% dplyr::select(c(pID,block,trial)) %>% distinct()
  
  loopingParameters=inner_join(loopingParameters1,loopingParameters2)
  
  timebyNodeData=data.frame()
  for( t in c(1:dim(loopingParameters)[1])){
    blockT=loopingParameters$block[t]
    trialT=loopingParameters$trial[t]
    pIDT=loopingParameters$pID[t]
    dataTrial = timeSpentByNode(pIDT,blockT,trialT,dataOpt,dataInput)
    timebyNodeData = rbind(timebyNodeData,dataTrial)
  }
  return(timebyNodeData)
}

#' Generates a dataframe with all the information by Node for one trial of one participant
#'
#' @param pIDT - Participant ID
#' @param blockT - Block
#' @param trialT - Trial
#' @param dataOptTrialInfo - Dataframe with KS Opt trial info (imported .txt files joined for all participants) 
#' @param clicksInfo - Dataframe with KS clicks info (imported .txt files joined for all participants) 
#'
#' @return dataTrial - Dataframe with all the information by Node for one trial for one participant
#' @export
#'
#' @examples
timeSpentByNode <- function(pIDT,blockT,trialT,dataOptTrialInfo,clicksInfo){
  
  #Extract all the relevant information from trial info to the particular trial to be analysed 
  infoTrial = dataOptTrialInfo %>% filter(pID==pIDT,block==blockT,trial==trialT)
  timeSpent= infoTrial$timeSpent
  capacity= infoTrial$capacity
  ws=as.integer(strsplit(infoTrial$w,',')[[1]])
  vs=as.integer(strsplit(infoTrial$v,',')[[1]])
  totalProf=sum(vs)
  totalWeight=sum(ws)
  nCap=capacity/totalWeight
  correct = infoTrial$correct
  profitOpt = infoTrial$profitOpt
  phaseT= infoTrial$phaseT
  id=infoTrial$id
  
  #Filters Clicks by Participant, block and trial
  clicksTrial = clicksInfo %>% filter(pID==pIDT,block==blockT,trial==trialT)
  #Number of columns and rows of the clicks dataframe
  nColumns=dim(clicksTrial)[2]
  nClicks=dim(clicksTrial)[1]
  
  #Generates two empty rows that mark start and end of the trial (time=0 and time=SubmitTime (i.e. timeSpent))
  row=vector(mode="numeric",length=nColumns)
  clicksTrialExt = rbind(row,clicksTrial,row)
  clicksTrialExt$time[nClicks+2]=timeSpent
  
  #Calculates the time at each node by calculating diff() over time column
  timeSpentAtNode = diff(clicksTrialExt$time)
  
  #Removes last row (Was only used to calculate timeSpentAtNode)
  clicksTrialExt = clicksTrialExt[-c(nClicks+2),]
  clicksTrialExt=dplyr::rename(clicksTrialExt,nodeArrivalTime=time)
  
  #Adds all the information of the trial missing to the first row
  clicksTrialExt$trial[1]=trialT
  clicksTrialExt$block[1]=blockT
  clicksTrialExt$pID[1]=pIDT
  
  #Adds all the trial Info to the output data frame
  clicksTrialExt$profitOpt = profitOpt
  clicksTrialExt$correct=correct
  clicksTrialExt = cbind(clicksTrialExt, timeSpentAtNode)
  clicksTrialExt$eventNumber=rank(clicksTrialExt$time)
  clicksTrialExt$numberOfEvents=max(clicksTrialExt$eventNumber)
  clicksTrialExt$capacity=capacity
  clicksTrialExt$phaseT = phaseT
  clicksTrialExt$id = id
  
  #Generates the weight and value at each node
  nodeWeight=vector(mode="numeric",length=nClicks+1)
  nodeValue=vector(mode="numeric",length=nClicks+1)
  nodeWeightCounter=0
  nodeValueCounter=0
  nodeWeight[1]=0
  nodeValue[1]=0
  
  for(i in c(2:(nClicks+1))){
    inOrOut=if (clicksTrialExt$In.1..Out.0.[i]==1) 1 else -1
    nodeWeightCounter= ws[clicksTrialExt$item[i]]*inOrOut + nodeWeightCounter
    nodeValueCounter= vs[clicksTrialExt$item[i]]*inOrOut + nodeValueCounter
    nodeWeight[i] = nodeWeightCounter
    nodeValue[i] = nodeValueCounter
  }
  
  clicksTrialExt = cbind(clicksTrialExt, nodeWeight, nodeValue)
  
  #Restrict analysis to those whose capacity constraint is being satisfied
  #clicksTrialExt = clicksTrialExt %>% filter(nodeWeight<=capacity)
  
  #Adds a column stating if capacity constraint is being satisfied
  clicksTrialExt$capSatisfied = (nodeWeight<=capacity)
  
  #Generate a column with the log (normalised profit / normalised capacity)
  clicksTrialExt$lnNpNc=log((clicksTrialExt$nodeValue/totalProf)/nCap)
  
  return(clicksTrialExt)
}



#' Analysis of a variable (that has one value per participant) and mean KS performance per participant
#'
#' @param summaryVariable is a DataFrame with 2 columns: 1 is the pID, 2 is the performance of the variable of interest 
#' @param KSData: Knapsack data, either optimisation or decision data. Mean Performance is calculated using function summaryData()
#'
#' @return Prints and returns the correlation and the plot.
#' @export
#'
#' @examples
indDiffAnalysis= function(summaryVariable,dataDec){
  summaryListTemp=summaryData(dataDec)
  names(summaryListTemp[[2]])['mean(correct)'==names(summaryListTemp[[2]])]='AccuracyKnapsack'
  varMerge = merge(summaryListTemp[[2]], summaryVariable, by="pID")
  varName=names(varMerge)[4]
  
  print(varName)
  
  cortest = cor.test(varMerge$AccuracyKnapsack,varMerge[[varName]],method="pearson")#spearman is non-parametric
  print(cortest)
  
  ggp= ggplot(varMerge,aes_string(x="AccuracyKnapsack",y=varName))+
    geom_point() +
    geom_smooth(method="lm")+
    labs(x="Knapsack Accuracy")
  
  print(ggp)
  
  return(list("plot"=ggp,"corrTest"=cortest))
  
}



#' Calculates the sum of weight and values for each feasible node for each instance
#'
#' @param dataOptProp: KS Optimisation data
#'
#' @return A list of dataframes one for each instance.
#' Each dataframes contains the sum of weights and values for each possible node; i.e only those nodes that satisfy the capacity constraint
#'
#' @examples
w_v_perNode <- function(dataOptProp){
  #Copy Data Input for later use
  dataInput=dataOptProp
  #Extracts a Unique list of instance with weights, value, capacity and profit
  dataInput=dataInput[c('w','v','capacity','id')]
  dataInput=unique(dataInput)
  
  #Generate all the possible subsets of a set of size 6
  p_set=powerSet(6)
  
  #Geenerated the list of dataframes
  nInstances = dim(dataInput)[1]
  allNodeslist=vector("list", nInstances)
  names(allNodeslist) = dataInput$id
  for(i in c(1:nInstances)){
    weights=as.integer(strsplit(dataInput$w[i],',')[[1]])
    values=as.integer(strsplit(dataInput$v[i],',')[[1]])
    wCap=dataInput$capacity[i]
    
    df=data.frame()
    for (subset in p_set){
      if(sum(weights[subset])<=wCap){
        df=rbind(df,data.frame(sum_w = sum(weights[subset]), sum_v = sum(values[subset])))
      }
    }
    
    allNodeslist[[i]]=df
  }
  
  return(allNodeslist)
}



#' Estimates the number of better solutions at each node 
#' Number of Better solutions:= Number nodes that have greater sum of values and current node and that satisfy the capacity constraint
#'
#' @param timebyNodeData (output of timeSpentByNode)
#' @param allNodesList (Output of w_v_perNode)
#'
#' @return the TimeSpentbyNode Data with the additional column of number of better solutions for each node
#' Output only includes those nodes that satisfy the capacity constraint
#'
#' @examples
nBetterSolutions = function(timebyNodeData,allNodesList){
  timeByNode2=timebyNodeData %>% filter(capSatisfied==TRUE)
  timeByNode2$nBetterSol=0
  for (i in 1:dim(timeByNode2)[1]){
    value=timeByNode2$nodeValue[i]
    id=timeByNode2$id[i]
    timeByNode2$nBetterSol[i]=dim(allNodesList[[id]] %>% filter(sum_v>value))[1]
  }
  
  return(timeByNode2)
}





