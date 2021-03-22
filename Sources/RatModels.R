#library(R.matlab)
#library(SDMTools)
#library(stringr)
#library(eegkit)#Librairie EEG pour l'analyse du LFP
#library("plot3D")
#library(data.tree)
#library(pracma)
#library(sp) #for spatial polygons
#library(doMPI)
library(Rmpi)
library(doParallel)

source("/home/amoongat/Projects/Rats-Credit/Sources/src/ValidationFunc.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/ModelUpdateFunc.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/ModelClasses.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/TurnModel.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel1.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel2.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel3.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel4.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/src/BaseClasses.R")
source("/home/amoongat/Projects/Rats-Credit/Sources/PathModels/utils.R")




#options(error = recover)


#Load SDM and SDL folders (SDM113,SDL101, etc.)

#load("DATA.RData")
load("/home/amoongat/Projects/Rats-Credit/data_journeys.Rdata")

#rats=DATA$Get('name', filterFun = function(x) x$level == 3)
rats = c("rat_101","rat_103","rat_106","rat_112","rat_113","rat_114")
### set dirpath where all plots will be saved 
setwd('..')
path = getwd()
time1 = format(Sys.time(), "%F %H-%M")
names=c('e','f','g','c','d','h','i','j','a','b','k')

#cl <- makeCluster(mpi.universe.size()-1, type='MPI')
#registerDoParallel(cl)

#cl <- startMPIcluster() 
#registerDoMPI(cl)


#worker.nodes = mpi.universe.size()-1
#print(sprintf("worker.nodes=%i",worker.nodes))
#cl <- makeCluster(mpi.universe.size()-1, type='PSOCK')
#registerDoParallel(cl)

#clusterEvalQ(cl, .libPaths("/home/amoongat/R/x86_64-redhat-linux-gnu-library/3.6"))

#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/ModelClasses.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/TurnModel.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel1.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel2.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel3.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel4.R"))
#clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/BaseClasses.R"))

### Loop through the enreg of all 6 rats
for (i in c(2:2)) {
  
  
  
  enregres = enregCombine(donnees_ash[[i]],rats[i])
  allpaths = enregres$allpaths
  boxTimes = enregres$boxTimes
  
  ratdata = populateRatModel(allpaths=allpaths,rat=rats[i],donnees_ash[[i]],TurnModel)
  
  testData = new("TestModels", Models=c("Paths","Hybrid1","Hybrid2","Hybrid3","Hybrid4","Turns"), creditAssignment=c("aca3"))
  
  
  # #### Holdout Validation ########################################
  
  #debug(HoldoutTest)
  #HoldoutTest(ratdata, testData)
  # ##### Model Selection On Acutal Data #########################3
  
  #debug(getModelResults)
  allmodelRes = getModelResults(ratdata,testData,sim=2, cl)
  min_method = getMinimumLikelihood(allmodelRes,testData)
  print(sprintf("%s is best model for %s",min_method,rats[i]))
  save(allmodelRes,file=paste0("allmodelRes_",rats[i],".Rdata"))
}

stopCluster(cl)
