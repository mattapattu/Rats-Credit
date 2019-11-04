library(rlist)
library(data.table)
library(data.tree)
library(igraph)


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
    #TRIALS = as.matrix(list(trial,Neuron1,Neuron2,Neuron3,Neuron4,Neuron5,Neuron6))
    
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
 
  pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),2]),as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),3])))
  # plot(spolygons)
  # points(pts, pch=16, cex=.5,col="red")

  ### SEt boxname for all points strcitly inside boxes
  for(id in getSpPPolygonsIDSlots(spolygons)){
    coord=spolygons[id,]@polygons[[1]]@Polygons[[1]]@coords
    l <- which(point.in.polygon(enreg[[ses]]$POS[,2],enreg[[ses]]$POS[,3], coord[,1],coord[,2])==1)
    nbx=convertToLetter(toString(id))
    enreg[[ses]]$POS[l,"boxname"]=nbx
  }

  ################### Align box b to center 
  
  ## All points outside the boxes, assign to closest box
  
  
  edgelist <- read.table(text = "1 2
                                 2 3
                                 3 9
                                 9 10           
                                 11 9
                                 10 4
                                 4 5
                                 4 6
                                 6 7
                                 7 8
                                 8 11
                                 5 1")  
  
  graph <- graph.data.frame(edgelist)


  l <- which(enreg[[ses]]$POS[,"boxname"] == "")
  
  for(i in l){
    spts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[i,2]),as.numeric(enreg[[ses]]$POS[i,3])))
    dist <- gDistance(spts,spolygons,byid=TRUE)
    if(min(dist) > 0) {
      index = max(which(enreg[[ses]]$POS[1:i,"boxname"] != ""))
      #### If l starts with 1,2,3,...., index will be -Infinity
      if(is.finite(index)){ 
        if(as.numeric(enreg[[ses]]$POS[i,1])-as.numeric(enreg[[ses]]$POS[index,1]) < 100){
          bxname = enreg[[ses]]$POS[index,"boxname"]
          neighbours <- V(graph)$name[neighbors(graph, as.character(convertToIndex(bxname)), mode = "total")]
          neighbours <- c(neighbours,convertToIndex(bxname))
          neighbour_dist <- dist[as.numeric(neighbours)]
          newbxname = neighbours[which.min(neighbour_dist)]
          enreg[[ses]]$POS[i,"boxname"]=convertToLetter(newbxname)
          #print(sprintf("New boxname - %s, prev box - %s",newbxname,bxname))
        }
      }else{
        enreg[[ses]]$POS[i,"boxname"] = convertToLetter(as.character(which.min(dist)))
      }
    }
  }
  
  
  borders = gDifference(as(spolygons,"SpatialLines"),as(gUnaryUnion(spolygons),"SpatialLines"), byid=TRUE)
  ### Points on borders and vertex - assign to prev known boxes (rat does not cross a box until it crosses over the boundary)
  for(id in getSpPPolygonsIDSlots(spolygons)){
    coord=spolygons[id,]@polygons[[1]]@Polygons[[1]]@coords
    k <- which(point.in.polygon(enreg[[ses]]$POS[,2],enreg[[ses]]$POS[,3], coord[,1],coord[,2])==2 | point.in.polygon(enreg[[ses]]$POS[,2],enreg[[ses]]$POS[,3], coord[,1],coord[,2])==3)
    if(length(k) > 0){
      spts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[k,2]),as.numeric(enreg[[ses]]$POS[k,3])))
      b<-gIntersects(borders,spts,byid=TRUE)
      p_sharedborder <- which(rowSums(1*b)>0)
      if(length(p_sharedborder)>0){
        dist <- gDistance(spts[p_sharedborder[1]],spolygons,byid=TRUE)
        boxes = which(dist==0)
        large<-which(enreg[[ses]]$POS[,"boxname"] == convertToLetter(as.character(boxes[1])) | enreg[[ses]]$POS[,"boxname"] == convertToLetter(as.character(boxes[2])))
        y <- large[findInterval(k,large,all.inside=TRUE)]
        enreg[[ses]]$POS[k,"boxname"] = enreg[[ses]]$POS[y,"boxname"]
      }
      p_nosharedborder <- which(rowSums(1*b)==0)
      if(length(p_nosharedborder)>0){
        dist <- gDistance(spts[p_nosharedborder[1]],spolygons,byid=TRUE)
        idx = which(dist==0)
        nbx=convertToLetter(toString(idx))
        enreg[[ses]]$POS[k,"boxname"]=nbx
     }
    }
  }
  

  g <- enreg[[ses]]$POS[,"boxname"]
  enreg[[ses]]$POS[,"trial"] = cumsum(c(1,as.numeric((g[seq_along(g)-1]=="i"| g[seq_along(g)-1]=="e") & g[-1] != g[-length(g)])))
  
  print("Returning enreg from add.boxes.to.pos")
  return(enreg)
  
}

add.rewards.to.pos=function(ses,enreg){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,Reward=0)
  
   for(idx in 1:length(enreg[[ses]]$EVENTS[,1])){
    #browser()
    if(enreg[[ses]]$EVENTS[idx,2] == 49){
      #print(sprintf("Event = 49 for index %i",idx))
      #index = min(which(as.numeric(enreg[[ses]]$POS[,1]) >= enreg[[ses]]$EVENTS[idx,1]))
      index = which.min(abs(as.numeric(enreg[[ses]]$POS[,1]) - enreg[[ses]]$EVENTS[idx,1]))
      
      #print(sprintf("%f,%i",enreg[[ses]]$EVENTS[idx,1],index))
      enreg[[ses]]$POS[index,"Reward"] = 49
      
    }else if(enreg[[ses]]$EVENTS[idx,2] == 51){
      #print(sprintf("Event = 51 for index %i",idx))
      #index = min(which(as.numeric(enreg[[ses]]$POS[,1]) >= enreg[[ses]]$EVENTS[idx,1]))
      index = which.min(abs(as.numeric(enreg[[ses]]$POS[,1]) - enreg[[ses]]$EVENTS[idx,1]))
      #print(sprintf("%f,%i",enreg[[ses]]$EVENTS[idx,1],index))
      enreg[[ses]]$POS[index,"Reward"] = 51
    }
  }
  print("Returning enreg from add.rewards.to.pos")
  return(enreg)
}

add.dist.to.pos=function(ses,enreg){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,distance=0)
  all_dist <-(diff(as.numeric(enreg[[ses]]$POS[,2]))^2 + diff(as.numeric(enreg[[ses]]$POS[,3]))^2)^0.5
  enreg[[ses]]$POS[2:length(enreg[[ses]]$POS[,1]),"distance"] = all_dist
  s1 <- which(all_dist > 10)
  s2 <- which(abs(as.numeric(enreg[[ses]]$POS[s1,1])-as.numeric(enreg[[ses]]$POS[s1+1,1]))>50)
  
  trials <- rle(enreg[[ses]]$POS[,"trial"])
  for(i in s1[s2]){
    j=0
    curr_box = enreg[[ses]]$POS[i,"boxname"]
    index = min(which(enreg[[ses]]$POS[(i+1):length(enreg[[ses]]$POS[,1]),"boxname"] != curr_box))
    index = i+1+index
    next_box = enreg[[ses]]$POS[index,"boxname"]
    if(curr_box %in% c('d','e','c','h','i','g','a','k') && next_box %in% c('d','e','c','h','i','g','a','k') ){
      j=3 
    }else{
      j=2
    }
    dist2 <- numeric()
    for (t in trials$value){
      k <- which((enreg[[ses]]$POS[,j] >= enreg[[ses]]$POS[i,j] & enreg[[ses]]$POS[,j] <= enreg[[ses]]$POS[i+1,j]) & enreg[[ses]]$POS[,"trial"] == t)
      if(length(k)==0){
        k <- which((enreg[[ses]]$POS[,j] >= enreg[[ses]]$POS[i+1,j] & enreg[[ses]]$POS[,j] <= enreg[[ses]]$POS[i,j]) & enreg[[ses]]$POS[,"trial"] == t)
      }
      dist2 <- c(dist2,sum((diff(as.numeric(enreg[[ses]]$POS[k,2]))^2 + diff(as.numeric(enreg[[ses]]$POS[k,3]))^2 )^0.5))
    }
    av_displacement = sum(dist2)/length(trials$values)
    enreg[[ses]]$POS[i+1,"distance"] = av_displacement
    #print(sprintf("Setting index %i, distance %f",i+1,av_displacement))
  }
  #write.table(as.data.frame(enreg[[ses]]$POS),file=sprintf("POS_session%i.csv",ses), quote=F,sep=",",row.names=F)
  
  enreg[[ses]]$POS[,"distance"] = cumsum(as.numeric(enreg[[ses]]$POS[,"distance"]))
  print("Returning enreg from add.dist.to.pos")
  return(enreg)
}

## Use POS data to add boxes to spikes
## Spikes of neuron 0 are ignored
  add.boxes.to.spikes=function(ses,enreg){
 # print("Inside1")
  #enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,Spikes=0)
  enreg[[ses]]$SPIKES = cbind(enreg[[ses]]$SPIKES,trial=-1)
  enreg[[ses]]$SPIKES = cbind(enreg[[ses]]$SPIKES,distance=0)
  trial = 0
  index=1
  total_spikes = length(enreg[[ses]]$SPIKES[,1])
  total_pos = length(enreg[[ses]]$POS[,1])
  i <- findInterval(as.numeric(enreg[[ses]]$SPIKES[,1]),as.numeric(enreg[[ses]]$POS[,1]))
  x <- 1:length(enreg[[ses]]$SPIKES[,1])
  i[i == 0] <- 1
  enreg[[ses]]$SPIKES[x,"boxName"] = enreg[[ses]]$POS[i[x],"boxname"]
  enreg[[ses]]$SPIKES[x,"trial"] =  enreg[[ses]]$POS[i[x],"trial"]
  enreg[[ses]]$SPIKES[x,"distance"] =  enreg[[ses]]$POS[i[x],"distance"]
  
  print("Returning enreg from add.boxes.to.spikes")
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
    trial=1
    sb=spikyBoxes[[ses]][[bx]]
    for(t in seq(from=1, to=length(sb$pass)-1, by=2)){
      
      duration=sb$pass[t+1]-sb$pass[t]
      neurons=sb$spikes[,3][(sb$spikes[,3]!=0)]
      a=length(neurons)/duration
      
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
  #print(paste("rat",animalNb,sep=","))
  animalNb  = gsub("rat_", "", animalNb)
  print(paste("rat",animalNb,sep=","))
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
  #short=shortcut(Enreg, ses, myboxes)
  #print(myboxes)
  #spolygons=getSpatialPolygons(myboxes)
  
  #For each box of the right path, eg h: spikyBoxes$h$duration (total duration), 
  #spikyBoxes$h$pass (column i: entering time, i+1: exiting time),
  #spikyBoxes$h$spikes (time,tetrod neuron rightPath boxName boxSensitivity)
  #spikyBoxes=getBoxesInPath(ses,Enreg,spolygons,short,rightPath)
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

alignBoxes=function(enreg,ses,spolygons,boites){
  
  pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),2]),as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),3])))
  xy <- coordinates(pts)
  centre_reward_points <- apply(xy, 2, mean)
  centroid_box_e <- coordinates(gCentroid(spolygons[1]))
  shiftx = centroid_box_e[1]-centre_reward_points[1]
  shifty = centroid_box_e[2] - centre_reward_points[2]
  lb=length(boites)
  boxes = boites

 
  # spolygons=getSpatialPolygons(boxes)
  # pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[,2]),as.numeric(enreg[[ses]]$POS[,3])))
  # plot(spolygons)
  # points(pts, pch=16, cex=.5,col="blue")
  
  h <- which(as.numeric(enreg[[ses]]$POS[,2]) > (boites[[10]][1,1]-shiftx) & as.numeric(enreg[[ses]]$POS[,2]) < (boites[[10]][1,3]-shiftx) & as.numeric(enreg[[ses]]$POS[,3]) > (boites[[10]][2,3]-shifty) & as.numeric(enreg[[ses]]$POS[,3]) < (boites[[10]][2,1]-shifty))
  pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[h,2]),as.numeric(enreg[[ses]]$POS[h,3])))
  xy <- coordinates(pts)
  centre_b_points <- apply(xy, 2, mean)
  lb=length(boites)
  for(r in 1:lb)
  {
    boxes[[r]]=rbind(boites[[r]][1,]-shiftx,boites[[r]][2,]-shifty)
  }
  spolygons=getSpatialPolygons(boxes)
  centroid_box_b <- coordinates(gCentroid(spolygons[10]))
  shift <- centroid_box_b-centre_b_points
  
  shiftx = shiftx+shift[1]
  shifty = shifty+shift[2]
  # boxes = boites
  # for(r in 1:lb)
  # {
  #   boxes[[r]]=rbind(boites[[r]][1,]-shiftx,boites[[r]][2,]-shifty)
  # }
  # 
  # spolygons=getSpatialPolygons(boxes)
  # pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[,2]),as.numeric(enreg[[ses]]$POS[,3])))
  # plot(spolygons)
  # points(pts, pch=16, cex=.5,col="blue")
  
  print("Returning from alignBoxes")
  
  return(c(shiftx,shifty))
}

plot.rewards=function(enreg){
  reward_49 <- numeric()
  reward_51 <- numeric()
  i_visits <- numeric()
  e_visits <- numeric()
  proportion_e <- numeric()
  proportion_i <- numeric()
  duration <- numeric()
  average_spike_freq <- numeric()
  for(ses in 1:length(enreg)){
    if(sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "49"))) == 0 && sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "51"))) == 0){
      #rewards <- c(rewards,(sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49))+sum(as.numeric(enreg[[ses]]$EVENTS[,2]==51))))
      reward_49 <- c(reward_49,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49)))
      reward_51 <- c(reward_51,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==51)))
      e_visits <- c(e_visits,1)
      i_visits <- c(i_visits,1)
      len = length(enreg[[ses]]$POS[,1])
      tot_time = as.numeric(enreg[[ses]]$POS[len,1])-as.numeric(enreg[[ses]]$POS[1,1])
      duration <- c(duration,tot_time)
      
      total_spikes = length(as.numeric(enreg[[ses]]$SPIKES[,"neuron"])!= 0)
      nb_neurons = max(as.numeric(enreg[[ses]]$SPIKES[,"neuron"]))
      average_spike_freq <- c(average_spike_freq,total_spikes/(tot_time*nb_neurons))
      
    }else{
      #rewards <- c(rewards,(sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49))+sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49))))
      reward_49 <- c(reward_49,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49)))
      reward_51 <- c(reward_51,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==51)))
      r <- rle(enreg[[ses]]$POS[,"boxname"])
      e_visits <- c(e_visits,sum(as.numeric(r$values=="e")))
      i_visits <- c(i_visits,sum(as.numeric(r$values=="i")))
      
      len = length(enreg[[ses]]$POS[,1])
      tot_time = as.numeric(enreg[[ses]]$POS[len,1])-as.numeric(enreg[[ses]]$POS[1,1])
      duration <- c(duration,tot_time)
      
      total_spikes = length(as.numeric(enreg[[ses]]$SPIKES[,"neuron"])!= 0)
      nb_neurons = max(as.numeric(enreg[[ses]]$SPIKES[,"neuron"]))
      average_spike_freq <- c(average_spike_freq,total_spikes/(tot_time*nb_neurons))
    }
  }
  rewards = reward_49+reward_51
  proportion_rewards <- rewards/(e_visits+i_visits)
  proportion_e <- reward_49/e_visits
  proportion_i <- reward_51/i_visits
  par(mfrow=c(3,2))

  plot(1:length(enreg),reward_49,col='red',type='l',xlab="Session",ylab="Rewards")
  lines(1:length(enreg),reward_51,col='blue',type='l',lty=2)
  legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2,cex=0.5,bty = "n")
    
  plot(1:length(enreg),proportion_e,col='red',type='l',xlab="Session",ylab="Proportion of rewarded i/e visits")
  lines(1:length(enreg),proportion_i,col='blue',type='l',lty=2)
  #legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2,cex=0.5,bty = "n")
  
  plot(1:length(enreg),proportion_rewards,col='red',type='l',xlab="Session",ylab="Total proportion of rewarded i/e visits")
  plot(1:length(enreg),duration,type="l",xlab="Session",ylab="Duration")
  plot(1:length(enreg),average_spike_freq,type="l",xlab="Session",ylab="Average spike freq")
  # plot(1:length(enreg),reward_49,col='red',type='b',xlab="Session",ylab="Rewards")
  # lines(1:length(enreg),reward_51,col='blue',type='b',lty=2)
  # legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2)
  print("Returning enreg from plot.rewards")
}

# Compute the nb of spikes for each neuron in the 
# boxes for a right path and store it 
set.neurons.to.boxes=function(tree,rightPath,boites){
  # rightPath='abcdefg'
  # For each rat
  rat=tree$Get('name', filterFun = function(x) x$level == 3)
  for (i in c(1)) {
    n=FindNode(tree,rat[[i]])
    #debug(convert.node.to.enreg)
    enreg=convert.node.to.enreg(n)
    #print(enreg)

    for(ses in c(1:length(enreg))){
      print(sprintf("Rat = %i , Session = %i",i,ses))
      boxes=boites
      spolygons=getSpatialPolygons(boxes)
      enreg=add.rewards.to.pos(ses,enreg)
      
      if(sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "49"))) == 0 && sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "51"))) == 0){
        print(sprintf("No rewards in this session, removing from tree"))
        animalNb  = gsub("rat_", "", rat[i])
        n$RemoveChild(paste(animalNb,"_session_",ses,sep=""))
        next
      }
      
      ### Before adding boxes, shift POS if first POS recording is negative
      
      if(sum(as.numeric(as.numeric(enreg[[ses]]$POS[,2])< 0)) > 0 || sum(as.numeric(as.numeric(enreg[[ses]]$POS[,3])< 0)) > 0){
        #debug(alignBoxes)
        shift=alignBoxes(enreg,ses,spolygons,boites)
        shiftx=shift[1]
        shifty=shift[2]
        # shiftx=130
        # shifty=129.5
        print(sprintf("Shifx=%f,shifty=%f",shiftx,shifty))
        lb=length(boxes)
        # enreg[[ses]]$POS[,2] = enreg[[ses]]$POS[,2]+shiftx
        # enreg[[ses]]$POS[,3] = enreg[[ses]]$POS[,3]+shifty
        for(r in 1:lb)
          {
          boxes[[r]]=rbind(boxes[[r]][1,]-shiftx,boxes[[r]][2,]-shifty)
          }
        
        spolygons=getSpatialPolygons(boxes)
        # pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[,2]),as.numeric(enreg[[ses]]$POS[,3])))
        # plot(spolygons)
        # points(pts, pch=16, cex=.5,col="red")
        # 
        }
      
      #print.plot.journeys(DATA,FindNode(tree,"Experiment in Marseille"),boites)

      #debug(add.box.to.pos)
      enreg=add.box.to.pos(ses,enreg,spolygons)
      
      #debug(add.dist.to.pos)
      enreg=add.dist.to.pos(ses,enreg)
      
      #debug(add.boxes.to.spikes)
      enreg=add.boxes.to.spikes(ses,enreg)
      
      
      #debug(plot.spikes.by.boxes.by.session)
      ##plot.spikes.by.boxes.by.session(rat[i],enreg,ses)
      #debug(plot.average.frequency.by.boxes)
      #plot.average.frequency.by.boxes(rat[i],enreg,ses)
      #debug(plot.average.frequency.by.boxes2)
      #plot.average.frequency.by.boxes2(rat[i],enreg,ses)
      #debug(plot.spikes.by.time)
      #plot.spikes.by.time(rat[i],enreg,ses)
      #debug(plot.spikes.by.distance)
      #plot.spikes.by.distance(rat[i],enreg,ses)
    }
    #debug(plot.spikes.by.boxes)
    #plot.spikes.by.boxes.by.rat(rat[i],enreg)
    #debug(change.tree.node)
    tree=change.tree.node(n,rat[i],tree,enreg,ses)
    debug(plot.rewards)
    plot.rewards(enreg)
  }
  return(tree)
}