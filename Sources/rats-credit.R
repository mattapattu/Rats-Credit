library(R.matlab)
library(SDMTools)
library(stringr)
library(eegkit)#Librairie EEG pour l'analyse du LFP
library("plot3D")
#library(hashmap)
library(data.tree)
library(pracma)
library(sp) #for spatial polygons


## Path of Sources directory
setwd("C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources")

options(error = recover)


# Load all source files from working directory
for (f in list.files(pattern="*.R$")) {
  if(!strcmp(f,"rats-credit.R"))
    source(f)
}

## Path of Data dir
setwd("C:/Rats-Credits/Data")

DATA=buildDataTree()
resalex=leo.boxes()
boites=alex.mergeBoxes(resalex$boxes)

rat=DATA$Get('name', filterFun = function(x) x$level == 3)

### set dirpath where all plots will be saved 
setwd('..')
path = getwd()
time1 = format(Sys.time(), "%F %H-%M")
dirpath1 = file.path(path,"Results","Plots",time1)
dir.create(dirpath1)

### Loop through the enreg of all 6 rats
for (i in c(1:6)) {
  n=FindNode(DATA,rat[[i]])
  
  ## Get the enreg of rat[[i]]  
  enreg=convert.node.to.enreg(n)
  
  for(ses in c(1:length(enreg))){
    print(sprintf("Rat = %i , Session = %i",i,ses))
    
    if(is.null(enreg[[ses]])){
      print(sprintf("No enreg in  %s session %i",rat[i], ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("No EVENT record in  %s session %i",rat[i], ses))
      next
    }
    
    spolygons=getSpatialPolygons(boites)
    
    ## Add column "Reward" to enreg[[ses]]$POS
    enreg=add.rewards.to.pos(ses,enreg)
    
    ## Add column "boxname" to enreg[[ses]]$POS
    enreg=add.box.to.pos(ses,enreg,spolygons)
    
    ### Add column "boxnames" to enreg[[ses]]$SPIKES
    enreg=add.boxes.to.spikes(ses,enreg)

  }
 
  #plot.reward_proportion(enreg,rat[i])
  
  #debug(plot.task.errors)
  #plot.task.errors(enreg,rat[i],dirpath1)
  
  #debug(plot.heatmap)
  plot.heatmap(enreg,rat[i],dirpath1)
  
  #plot.average.frequency.by.boxes2(enreg,rat[i],dirpath1)
  
}