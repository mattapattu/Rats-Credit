library(rlist)
library(data.table)
library(data.tree)


###TO DO : Linearization
## Classify neuron activity :
## 1) By space (Allocentric firing) - Do the same neurons fire everytime when the rat is in the same box for different trials? 
##    How to : Dissociate each trial and calulate the average Pearson correlation coefficient of cell activity between each trial.
##    Info required : For each trial get the activity of the neurons in  every box.
## 2) By time - Do neurons fire after fixed time ?
##    How to : Sum up the spike-time series for each neuron and average the total firing activity in bins of size 1s    
##    Info required : Spike-time series of neurons for every session  
## 3) By distance - Do neurons fire when the rat travels a fixed distance ?
##    How to : Sum up the total distance travelled by the rat and average the cell activity in bins of size 10 cm . 
##    Info required : Neuron activity as a function of distance covered

get_str_recur <- function(x,text2,y){
  
  text <- paste0(text2,"Element[",y,"] is a List of length ",length(x), " --> ")
  
  for (i in (1:(length(x)))){
    subs <- x[[i]]
    if (is.list(subs)){
      get_str_recur(subs,text,i)
      
    }else{
      print(paste0(text," Element [",i,"] is a ",class(subs)," of length ",length(subs)))
    }
  }
}

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


add.box.to.pos=function(ses,enreg,spolygons){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,boxname="")
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,trial="")
  prev_nbx="NoBox"
  prev_2_nbx="unknown"
  count = 0
  neg_displacement = 0
  trial=1
  for(idx in 1:length(enreg[[ses]]$POS[,1])){
    #n=enreg$SPIKES[idx,3]
    #time=enreg[[ses]]$POS[[idx,1]]
    #nbx=get.box(enreg[[ses]]$POS,spolygons,time)
    nbx=get.box(enreg,ses,spolygons,idx)
    enreg[[ses]]$POS[idx,"boxname"] = nbx
    if(nbx=="noBox"){
      next
    }
    else if(prev_nbx != nbx){
      if(nbx==prev_2_nbx){
        #print(sprintf("Rat moves backwards from %s to %s in trial %i at time %s", prev_nbx, nbx, trial, enreg[[ses]]$POS[idx, 1]))
        neg_displacement = neg_displacement+1
        
      }
      #print(sprintf("New box reached after %i recordings is %s",count, nbx))
      #### New trial starts if prev box is e or i 
      if(prev_nbx=="e"|| prev_nbx=="i" ){
          #print(sprintf("Total negative displacement for trial %i is %i", trial, neg_displacement))
          trial=trial+1
          neg_displacement = 0
      }
      count = 0
      prev_2_nbx = prev_nbx
      prev_nbx = nbx
      #print(nbx)
    }else{
      count = count+1
    }
    enreg[[ses]]$POS[idx,"trial"] = trial
  }
  #debug(add.rewards.to.pos)
  enreg=add.rewards.to.pos(ses,enreg)
  #debug(add.boxes.to.spikes)
  enreg=add.boxes.to.spikes(ses,enreg)
  
  #print(enreg[[ses]]$POS)
  # capture.output(summary(enreg[[ses]]$POS), file = "/home/ajames/Output.txt")
  #write.table(as.data.frame(enreg[[ses]]$POS),file=sprintf("POS_session%i.csv",ses), quote=F,sep=",",row.names=F)
  # write.table(as.data.frame(enreg[[ses]]$EVENTS),file=sprintf("POS_session%i",ses), quote=F,sep=",",row.names=F)
   
   print("Returning enreg from add.boxes.to.spikes")
  return(enreg)
  
}

add.rewards.to.pos=function(ses,enreg){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,Reward=0)
  
  for(idx in 1:length(enreg[[ses]]$EVENTS[,1])){
    #browser()
    if(enreg[[ses]]$EVENTS[idx,2] == 49){
      #print(sprintf("Event = 49 for index %i",idx))
      index = min(which(as.numeric(enreg[[ses]]$POS[,1]) >= enreg[[ses]]$EVENTS[idx,1]))
      #print(sprintf("%f,%i",enreg[[ses]]$EVENTS[idx,1],index))
      enreg[[ses]]$POS[index,"Reward"] = 49
      
    }else if(enreg[[ses]]$EVENTS[idx,2] == 51){
      #print(sprintf("Event = 51 for index %i",idx))
      index = min(which(as.numeric(enreg[[ses]]$POS[,1]) >= enreg[[ses]]$EVENTS[idx,1]))
      #print(sprintf("%f,%i",enreg[[ses]]$EVENTS[idx,1],index))
      enreg[[ses]]$POS[index,"Reward"] = 51
    }
  }
  print("Returning enreg from add.rewards.to.pos")
  return(enreg)
}

## Use POS data to add boxes to spikes
## Spikes of neuron 0 are ignored
add.boxes.to.spikes=function(ses,enreg){
  #enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,Spikes=0)
  enreg[[ses]]$SPIKES = cbind(enreg[[ses]]$SPIKES,trial=-1)
  
  for(idx in 1:length(enreg[[ses]]$SPIKES[,1])){
    if(enreg[[ses]]$SPIKES[idx,"neuron"] != "0"){
      index = min(which(as.numeric(enreg[[ses]]$POS[,1]) >= as.numeric(enreg[[ses]]$SPIKES[idx,1])))
      index = index-1
      #enreg[[ses]]$POS[index,"boxname"] = as.numeric(enreg[[ses]]$POS[index,"Spikes"]) +1 
      enreg[[ses]]$SPIKES[idx,"boxName"] = enreg[[ses]]$POS[index,"boxname"]
      enreg[[ses]]$SPIKES[idx,"trial"]  =  enreg[[ses]]$POS[index,"trial"]
      #enreg[[ses]]$POS[index,"Spikes"] = paste(enreg[[ses]]$POS[index,"Spikes"],enreg[[ses]]$SPIKES[idx,1],sep=" ")
      #print(enreg[[ses]]$POS[index,"Spikes"])
    }
  }
  return(enreg)
}


# Return the box the neuron belongs to
# "noBox" otherwise
get.box=function(enreg,ses,spolygons,index){
  nbx="noBox"
  for(id in getSpPPolygonsIDSlots(spolygons)){
    coord=spolygons[id,]@polygons[[1]]@Polygons[[1]]@coords
    within=point.in.polygon(enreg[[ses]]$POS[index,2],enreg[[ses]]$POS[index,3], coord[,1],coord[,2])
    if(within == 1){
      nbx=convertToLetter(toString(id))
      break
    }
  }
  return(nbx)
}

# spikyBoxes$h$spikes (time,tetrod neuron rightPath boxName boxSensitivity)
#enreg=set.boxes.to.neurons(ses,spikyBoxes,Enreg,spolygons,rightPath)
set.boxes.to.neurons=function(ses,spikyBoxes,enreg,spolygons,rightPath){
  #neurons=spikyBoxes[[ses]][[bx]]$spikes[,3][(spikyBoxes[[ses]][[bx]]$spikes[,3]!=0)]
  
  for(idx in 1:length(enreg[[ses]]$SPIKES[,1])){
      #n=enreg$SPIKES[idx,3]
      time=enreg[[ses]]$SPIKES[[idx,1]]
      #nbx=get.box(enreg[[ses]]$POS,spolygons,time)
      nbx=get.box(enreg,ses,spolygons,time)
      
      enreg[[ses]]$SPIKES[idx,"boxName"]   = nbx
      
     
      if(in.right.path(ses,spikyBoxes,time)){
        enreg[[ses]]$SPIKES[idx,"rightPath"] = rightPath
      }
    }
  
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
    for(t in seq(from=1, to=length(sb$pass)-1, by=2)){
      
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
  #print(enreg[[ses]]$PATHS, pruneMethod = NULL)
  return(enreg)
}

change.tree.node=function(rat,animalNb,tree,enreg,ses){
  animalNb  = gsub("rat_", "", animalNb)
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
  #dt <- FromListSimple(spikyBoxes)
  #print(dt)
  
  #get_str_recur(spikyBoxes,"",0)
  
  #print(spikyBoxes)
  
  enreg=add.box.to.pos(ses,Enreg,spolygons)
  # enreg=set.activity.to.boxes(ses,spikyBoxes,Enreg,rightPath)
  # enreg=set.boxes.to.neurons(ses,spikyBoxes,Enreg,spolygons,rightPath)

  #tree$Set(spatialPolygons = boites,filterFun = function(x) x$level == 1)
  print("Returning enreg from add.neuron.in.path")
  return(enreg)
}

# Compute the nb of spikes for each neuron in the 
# boxes for a right path and store it 
set.neurons.to.boxes=function(tree,rightPath,boites){
  # rightPath='abcdefg'
  # For each rat
  rat=tree$Get('name', filterFun = function(x) x$level == 3)
  for (i in length(rat)) {
    n=FindNode(tree,rat[[i]])
    enreg=convert.node.to.enreg(n)
    #print(enreg)
    for(ses in length(enreg)){
      print(sprintf("Rat = %i , Session = %i",i,ses))
      enreg=add.neuron.in.path(tree,ses,rightPath,boites,enreg,i)
      plot.spikes.by.boxes(ses,enreg)
      tree=change.tree.node(n,rat[i],tree,enreg,ses)
    }
  }
  return(tree)
}