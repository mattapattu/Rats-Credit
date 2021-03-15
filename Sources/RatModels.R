library(R.matlab)
library(SDMTools)
library(stringr)
library(eegkit)#Librairie EEG pour l'analyse du LFP
library("plot3D")
library(data.tree)
library(pracma)
library(sp) #for spatial polygons


setwd("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources")
source("src/ValidateModels.R")
source("src/ModelUpdates.R")
source("src/ModelDesc.R")
source("src/TurnModel.R")
source("src/HybridModel1.R")
source("src/HybridModel2.R")
source("src/HybridModel3.R")
source("src/HybridModel4.R")
source("src/BaseModels.R")
source("PathModels/utils.R")




options(error = recover)


#Load SDM and SDL folders (SDM113,SDL101, etc.)

setwd("C:/Rats-Credits/Data")
load("DATA.RData")
load("data_journeys.RData")

rats=DATA$Get('name', filterFun = function(x) x$level == 3)

### set dirpath where all plots will be saved 
setwd('..')
path = getwd()
time1 = format(Sys.time(), "%F %H-%M")
names=c('e','f','g','c','d','h','i','j','a','b','k')
### Loop through the enreg of all 6 rats
for (i in c(2:6)) {
  
  
  
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
  allmodelRes = getModelResults(ratdata,testData,sim=2)
  min_method = getMinimumLikelihood(allmodelRes,testData)  
  print(sprintf("%s is best mode for %s",min_method,rats[i]))
  save(allmodelRes,file=paste0("allmodelRes_",rats[i],".Rdata"))
}