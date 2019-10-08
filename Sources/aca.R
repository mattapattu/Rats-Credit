library(rlist)
library(data.table)



convert.node.to.enreg=function(rat){
  enr=list()
  i=1
  for (s in rat$children) {
    enr[[i]]=list.append(list(POS=s$POS,SPIKES=s$SPIKES,EVENTS=s$EVENTS))
    i=i+1
  }
  return(enr)
}

# Return the path if the neuron belongs to a rightPath
# otherwise an empty string ""
in.right.path=function(ses,spikyBoxes,time,rpath){
  inPath=FALSE
  #Change rightPath,boxName in enreg$SPIKES
  #SPIKES=spks,#temps, tetrode, neuron, rightPath,boxName,boxSensitivity
  string_split <- strsplit(str_sub(rightPath,0,-2), "")[[1]]
  
  for(bx in string_split){
    sb=spikyBoxes[[ses]][[bx]]
    for(t in seq(from=0, to=length(sbx$pass)-1, by=2)){
      if(time  >= sb$pass[t] 
         && time <= sb$pass[t+1]){
        inPath=TRUE
        break
      }
      # if the time is greater than an exit time  
      # there is no need to continue
      else if(time > sb$pass[t+1]){
        break
      }
    }
  }
  return(inPath)
}

# Return the box the neuron belongs to
# "noBox" otherwise
get.box=function(position,spol,time){
  nbx="noBox"
  for(id in getSpPPolygonsIDSlots(spol)){
    coord=spolygons[id,]@polygons[[1]]@Polygons[[1]]@coords
    within=point.in.polygon(position[time,1],position[time,2], coord)
    if(within == 1){
      nbx=convertToLetter(toString(id))
    }
  }
  return(nbx)
}

# spikyBoxes$h$spikes (time,tetrod neuron rightPath boxName boxSensitivity)
set.boxes.to.neurons=function(spikyBoxes,enreg,spolygons,rightPath){
  #neurons=spikyBoxes[[ses]][[bx]]$spikes[,3][(spikyBoxes[[ses]][[bx]]$spikes[,3]!=0)]
  
  for(idx in 0:length(enreg$SPIKES[,1])){
      #n=enreg$SPIKES[idx,3]
      nbx=get.box(enreg$POS,spolygons,time)
      enreg$SPIKES[idx,"boxName"]   = nbx
      
      time=enreg$SPIKES[idx,1]
      
      if(in.right.path(ses,spikyBoxes,time)){
        enreg$SPIKES[idx,"rightPath"] = rightPath
      }
    }
    # }
  return(enreg)
}
#Change in enreg$PATHS
#pth=list("trial"=1, "path"="", "boxNm"="", "activity"="")
set.activity.to.boxes=function(ses,spikyBoxes,enreg,rightPath){
  #h: spikyBoxes$h$duration (total duration), 
  #spikyBoxes$h$pass (column i: entering time, i+1: exiting time),
  #spikyBoxes$h$spikes (time,tetrod neuron rightPath boxName boxSensitivity)
  
  neuronThruBoxes=list()
  string_split <- strsplit(str_sub(rightPath,0,-2), "")[[1]]
  #bxs=replicate(length(string_split),0)
  #The indexes of the vector are the right path boxes
  #names(bxs)=string_split
  
  count =0
  #For each box
  for(bx in string_split){
    #Get the index of neurons
    #neurons=spikyBoxes[[nm]]$spikes[,3][(spikyBoxes[[nm]]$spikes[,3]!=0)]
    #For each trial
    trial=1
    sb=spikyBoxes[[ses]][[bx]]
    for(t in seq(from=1, to=length(sb$pass)-1, by=2) ){
      
      duration=sb$pass[t+1]-sb$pass[t]
      neurons=sb$spikes[,3][(sb$spikes[,3]!=0)]
      a=length(neurons)/duration
      
      #pth=list("trial"=1, "path"="", "boxNm"="", "activity"="")
      #print("Appending")
      #print(enreg[[ses]])
      #enreg[[ses]][4] <- PATHS
      #enreg[[ses]] <- c(enreg[[ses]],PATHS)
      if(count==0){
        enreg[[ses]]$PATHS <-append(enreg[[ses]]$PATHS, c("trial"=trial, "path"=rightPath, "boxNm"=bx,"activity"=a))
      }else{
        values <- list(trial, rightPath, bx,a)
#        print(values)
        enreg[[ses]]$PATHS <- mapply(append, enreg[[ses]]$PATHS, values, SIMPLIFY = FALSE)
        #enreg[[ses]]$PATHS.append("trial"=trial, "path"=rightPath, "boxNm"=bx,"activity"=a)
      }
      trial=trial+1
      count = count+1
      
#      enreg[[ses]]$PATHS <-append(enreg[[ses]]$PATH, c("trial"=1, "path"=rightPath, "boxNm"=bx,"activity"=a))
      
    }
    
  }
  df1 <- data.frame(sapply(enreg[[ses]]$PATHS,c))
  enreg[[ses]]$PATHS = list(df1)
  print(enreg[[ses]]$PATHS, pruneMethod = NULL)
  return(enreg)
}

change.tree.node=function(rat,animalNb,tree,enreg,ses){
  for(rec in enreg){
    #temps, tetrode, neurone, remove last voltage values
    spks=rec$SPIKES[,1:3] 
    vb=logical(length(spks[,1])) #add a vector of booleans
    vs=vector(mode="character", length=length(spks[,1])) #add a vector of strings
    spks=cbind(spks,vs,vs,vb)
    colnames(spks) <- c("time","tetrod","neuron","rightPath","boxName","boxSensitivity")
    #replace child from rat with new data enreg: 
    rat$RemoveChild(paste(animalNb,"_session_",ses,sep=""))
    rat$AddChild(paste(animalNb,"_session_",ses,sep=""),
                           #set fields (capital letters)
                           POS=rec$POS,#temps, x,y
                           EVENTS=rec$EVENT,#temps, evts
                           SPIKES=rec$SPIKES,#temps, tetrode, neurone, rightPath,boxName,boxSensitivity
                           PATHS=rec$PATHS,#trial, path, boxNm, activity
                           LFP=rec$LFP)#temps, amplitude
  }
  return(tree)
}

# Compute the neuron activity in each box of a right path
# a list where the indexes are the neuron numbers
# and for each neuron there is a vector of activity in boxes
add.neuron.in.path=function(tree,ses, rightPath,myboxes, Enreg,ratNb){
  short=shortcut(Enreg, ses, myboxes)
  spolygons=getSpatialPolygons(myboxes)
  #For each box of the right path, eg h: spikyBoxes$h$duration (total duration), 
  #spikyBoxes$h$pass (column i: entering time, i+1: exiting time),
  #spikyBoxes$h$spikes (time,tetrod neuron rightPath boxName boxSensitivity)
  spikyBoxes=getBoxesInPath(ses,Enreg,spolygons,short,rightPath)
  
  enreg=set.activity.to.boxes(ses,spikyBoxes,Enreg,rightPath)
  
  
  ## 1) By space - Do neurons encode according to boxes ? 
  ##    Info required : Activity of every neuron for every box
  ## 2) By time - Do neurons fire after fixed time ?
  ##    Info required : Activity of every neuron at each elapsed time  
  ## By distance - Do neurons fire after a fixed distance is covered ?
  ##    Info required : Activity if every neuron after fixed distance is covered
  
  
  
  enreg=set.boxes.to.neurons(ses,spikyBoxes,Enreg,spolygons,rightPath)
  
  
  
  #tree$Set(spatialPolygons = boites,filterFun = function(x) x$level == 1)
  
  return(enreg)
}

# Compute the nb of spikes for each neuron in the 
# boxes for a right path and store it 
set.neurons.to.boxes=function(tree,rightPath,boites){
  # rightPath='abcdefg'
  # For each rat
  #cat("tree:")
  #print(tree)
  #cat("RightPath:")
  #print(rightPath)
  rat=tree$Get('name', filterFun = function(x) x$level == 3)
  for (i in length(rat)) {
    n=FindNode(tree,rat[[i]])
    enreg=convert.node.to.enreg(n)
    for(s in 1:length(enreg)){
      enreg=add.neuron.in.path(tree,s,rightPath,boites,enreg,i)
      tree=change.tree.node(rat,i,tree,enreg,s)
    }
  }
  return(tree)
}