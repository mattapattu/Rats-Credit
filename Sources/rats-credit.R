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

options(error = recover)


# Load all source files from working directory
for (f in list.files(pattern="*.R$")) {
  if(!strcmp(f,"rats-credit.R"))
    source(f)
}

#Load SDM and SDL folders (SDM113,SDL101, etc.)
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
for (i in c(6:2)) {
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
  #plot.task.errors(enreg,rat[i],dirpath1,pathProb=T)
  
  #debug(plot.heatmap)
  #plot.heatmap(enreg,rat[i],dirpath1)
  
  #plot.average.frequency.by.boxes2(enreg,rat[i],dirpath1)
  
  #debug(plot.heatmap.paths)
  #plot.heatmap.paths(enreg,rat[i],dirpath1, TRUE)
  
  #debug(mle_rl)
  #mle_rl(enreg,rat[i])
  
  #debug(mle_aca)
  #mle_aca(enreg,rat[i])
  
  #debug(mazeACA2)
  #mazeACA2(enreg,rat[i])
  #debug(mse_compare)
  res_mat<-mse_compare(enreg,rat[i])
  
  #mle_rl_softmax(enreg,rat[i])
}