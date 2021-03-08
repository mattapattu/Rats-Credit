library(R.matlab)
library(SDMTools)
library(stringr)
library(eegkit)#Librairie EEG pour l'analyse du LFP
library("plot3D")
library(data.tree)
library(pracma)
library(sp) #for spatial polygons


setwd("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources")
source("src/Validate.R")
source("src/ModelUpdates.R")
source("src/BaseModels.R")
source("src/ModelDescriptions.R")
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
  
  ratdata = populateRatModel(allpaths=allpaths,rat=rats[i],donnees_ash[[i]])
  
  # #### Holdout Validation ########################################
  
  validateTestData = new("TestData", pathModels=c("aca3Paths"), turnModels=c("aca3Turns"))
  debug(HoldoutTest)
  HoldoutTest(ratdata, validateTestData)
  # ##### Model Selection On Acutal Data #########################3
  
  #mat_res = windowCompare(generated_data,models, sim=2)
  #debug(getModelData)
  
  pathmodels=c("aca","gb","aca2","aca3","sarsa")
  turnmodels=c("acaTurns","gbTurns","aca2Turns","aca3Turns","sarsaTurns")
  #pathmodels=c("aca3")
  #turnmodels=c("aca3Turns")
  #debug(getModelData)
  #res1 = getModelData(generated_data, pathmodels, window = window, sim=2)
  allmodelRes = getModelResults(generated_data,models,sim)
  min_method = getMinimumLikelihood(allmodelRes)  
}