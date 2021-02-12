library(R.matlab)
library(SDMTools)
library(stringr)
library(eegkit)#Librairie EEG pour l'analyse du LFP
library("plot3D")
#library(hashmap)
library(data.tree)
library(pracma)
library(sp) #for spatial polygons


setwd("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources")
source("lib/LoadData/collect.R")
source("lib/LoadData/aca.R")
source("lib/LoadData/func.R")
# source("PathModels/pathModels.R")
# source("PathModels/mseMethods.R")
# source("PathModels/modelData.R")
# source("PathModels/utils.R")
# source("PathModels/models.R")


for (f in list.files(c("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/PathModels"), pattern="*.R", full.names = TRUE)) {
  print(f)
  source(f)
}


for (f in list.files(c("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/TurnModels"), pattern="*.R", full.names = TRUE)) {
  source(f)
}





options(error = recover)


# Load all source files from working directory
#for (f in list.files(pattern="*.R$")) {
#  if(!strcmp(f,"rats-credit.R"))
#    source(f)
#}

#Load SDM and SDL folders (SDM113,SDL101, etc.)

setwd("C:/Rats-Credits/Data")
load("DATA.RData")
load("data_journeys.RData")
# DATA=buildDataTree()
# resalex=leo.boxes()
# boites=alex.mergeBoxes(resalex$boxes)

rats=DATA$Get('name', filterFun = function(x) x$level == 3)

### set dirpath where all plots will be saved 
setwd('..')
path = getwd()
time1 = format(Sys.time(), "%F %H-%M")
dirpath1 = file.path(path,"Results","Plots",time1)
dir.create(dirpath1)
names=c('e','f','g','c','d','h','i','j','a','b','k')
### Loop through the enreg of all 6 rats
for (i in c(2:6)) {
  
  

  #debug(plot.heatmap)
  #plot.heatmap(enreg,rat[i],dirpath1)
  

  #debug(plot.heatmap.paths)
  #plot.heatmap.paths(enreg,rat[i],dirpath1, TRUE)
  
  debug(comparePathModels)
  comparePathModels(donnees_ash[[i]],rats[i], window=20,path)
  
  #debug(compareTurnModels)
  #compareTurnModels(enreg,rat[i], window=20)
   
}