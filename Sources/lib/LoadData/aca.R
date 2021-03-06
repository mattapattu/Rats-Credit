library(rlist)
library(data.table)
library(data.tree)
library(igraph)



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
  for (s in rat$children) {
    sessionNb=as.numeric(str_match(s$name,"_session_(\\d+)")[,2])
    enr[[sessionNb]]=list.append(list(POS=s$POS,SPIKES=s$SPIKES,EVENTS=s$EVENTS))
    #TRIALS = as.matrix(list(trial,Neuron1,Neuron2,Neuron3,Neuron4,Neuron5,Neuron6))
    
  }
  return(enr)
}



add.box.to.pos=function(ses,enreg,spolygons){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,boxname="")
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,trial="")
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,Session=ses)
 
  #pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),2]),as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),3])))
  # plot(spolygons)
  # points(pts, pch=16, cex=.5,col="red")

  ### Set boxname for all points strcitly inside boxes
  for(id in getSpPPolygonsIDSlots(spolygons)){
    coord=spolygons[id,]@polygons[[1]]@Polygons[[1]]@coords
    l <- which(point.in.polygon(enreg[[ses]]$POS[,2],enreg[[ses]]$POS[,3], coord[,1],coord[,2])==1)
    #nbx=convertToLetter(toString(id))
    nbx=id
    enreg[[ses]]$POS[l,"boxname"]=nbx
  }
  #print(sprintf("boxname1=%s",enreg[[ses]]$POS[4698,"boxname"]))
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
    index = max(which(enreg[[ses]]$POS[1:i,"boxname"] != ""))
    #### If l starts with 1,2,3,...., index will be -Infinity
    if(is.finite(index)){ 
      enreg[[ses]]$POS[i,"boxname"] = enreg[[ses]]$POS[index,"boxname"]
    }
  }
  #print(sprintf("boxname1=%s",enreg[[ses]]$POS[4698,"boxname"]))
  
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
        #nbx=convertToLetter(toString(id))
        nbx=toString(id)
        neighbours <- neighbors(graph, convertToIndex(id), mode = "total")
        neighbor_pos<-which(enreg[[ses]]$POS[,"boxname"] == nbx | enreg[[ses]]$POS[,"boxname"] == convertToLetter(as.character(neighbours[1])) | enreg[[ses]]$POS[,"boxname"] == convertToLetter(as.character(neighbours[2])))
        y <- neighbor_pos[findInterval(k,neighbor_pos,all.inside=TRUE)]
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
  
  l <- which(enreg[[ses]]$POS[,"boxname"] == "")
  for(i in l){
    index = max(which(enreg[[ses]]$POS[1:i,"boxname"] != ""))
    #### If l starts with 1,2,3,...., index will be -Infinity
    if(is.finite(index)){ 
      enreg[[ses]]$POS[i,"boxname"] = enreg[[ses]]$POS[index,"boxname"]
    }
  }
  
  ## If still any boxes are unassigned , name them "unk" and go on with assiging trials
  enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == ""),"boxname"]="unk"
  #print(sprintf("boxname1=%s",enreg[[ses]]$POS[4698,"boxname"]))

  #### If rat is on border and box changes - k,a,k,a,k,a -> assign box as k
  g <- enreg[[ses]]$POS[,"boxname"]
  i<-1:length(g)
  j<-2:length(g)
  k<-3:length(g)
  l <- which(g[j]!=g[k]&g[j]!=g[i])
  n<- l+1
  for(index in n){
    x=index
    repeat{
      x=x-1
      if(!x %in% l){
        #print(sprintf("index=%i,x=%i",index,x))
        enreg[[ses]]$POS[index,"boxname"]=enreg[[ses]]$POS[x,"boxname"]
        break
      }
    }
  }
  
  ## For all "unk" points, (e.g. rat_103,ses33(1:5) - no previous points) - assign to nearest box ( box b in this case)
  unknowns <- which(enreg[[ses]]$POS[,"boxname"] == "unk")
  for(idx in unknowns){
    spts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[idx,2]),as.numeric(enreg[[ses]]$POS[idx,3])))
    boxId = which.min(gDistance(spts,spolygons,byid=TRUE))
    boxname = convertToLetter(toString(boxId))
    enreg[[ses]]$POS[idx,"boxname"] = boxname
  }
                    
  
  # enreg[[ses]]$POS[,"trial"] = cumsum(c(1,as.numeric((g[seq_along(g)-1]=="i"| g[seq_along(g)-1]=="e") & g[-1] != g[-length(g)])))
  #debug(add.trial.to.pos)
  enreg=add.trial.to.pos(enreg,ses)
  
  
  #print("Returning enreg from add.boxes.to.pos")
  return(enreg)
  
}

add.trial.to.pos=function(enreg,ses){
  
  r <- rle(enreg[[ses]]$POS[,"boxname"])
  allpaths <- toString(r$values)
  ## (, j, a)|(, j, b)|(, f, b)|(, f, a) are special cases : Need better fix here
  allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k)|(, j, a)|(, j, b)|(, f, g)|(, f, b)|(, f, a)|(, d, c)|(, h, c)|(, j, i, k,))",perl=TRUE)[[1]]
  start_rle_index=1
  stop_rle_index = 0
  start_pos_index=1
  stop_pos_index = 0
  for(i in 1:length(allpaths)){
    l<-strsplit(allpaths[i],",")[[1]]
    l<-gsub(" ", "", l, fixed = TRUE)
    l <-l[l != ""]
    stop_rle_index=start_rle_index+length(l)-1
    stop_pos_index = sum(r$lengths[start_rle_index:stop_rle_index])+start_pos_index-1
    enreg[[ses]]$POS[start_pos_index:stop_pos_index,"trial"]=i
    # print(sprintf("start_rle_index=%i,stop_rle_index=%i,start_pos_index=%i,stop_pos_index=%i",start_rle_index,stop_rle_index,start_pos_index,stop_pos_index))
    # print(sprintf("actual path is %s",toString(r$values[start_rle_index:stop_rle_index])))
    # print(sprintf("expected path is %s",toString(allpaths[i])))
    start_rle_index=stop_rle_index+1
    start_pos_index = stop_pos_index+1
    
  }
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
  #print("Returning enreg from add.rewards.to.pos")
  return(enreg)
}

add.dist.to.pos=function(ses,enreg,spolygons){
  enreg[[ses]]$POS = cbind(enreg[[ses]]$POS,distance=0)
  all_dist <-(diff(as.numeric(enreg[[ses]]$POS[,2]))^2 + diff(as.numeric(enreg[[ses]]$POS[,3]))^2)^0.5
  enreg[[ses]]$POS[2:length(enreg[[ses]]$POS[,1]),"distance"] = all_dist
  s1 <- which(all_dist > 50)
  s2 <- which(abs(as.numeric(enreg[[ses]]$POS[s1,1])-as.numeric(enreg[[ses]]$POS[s1+1,1]))>50)
  
  ## For POS recordings with displacement > 10 & time gap > 50, estimate new trajectory instead of doing diff.
  
  edgelist <- read.table(text = " e f
                                f g
                                g a
                                a b           
                                k a
                                b c
                                c d
                                c h
                                h i
                                i j
                                j k
                                d e")  
  
  graph <- graph.data.frame(edgelist)
  
  
  for(i in s1[s2]){
    print(sprintf("Large displacement found at index - %i",i))
    
    curr_pt=SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[i,2]),as.numeric(enreg[[ses]]$POS[i,3])))
    next_pt=SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[(i+1),2]),as.numeric(enreg[[ses]]$POS[(i+1),3])))
    
    curr_box = enreg[[ses]]$POS[i,"boxname"]
    next_box = enreg[[ses]]$POS[(i+1),"boxname"]
    
    if(curr_box==next_box){
      ## If displacement in same box, use the previously computed distance
      print(sprintf("Skipping, since current box = next box"))
      next;
    }else{
      
      ### the paths of all trials
      r <- rle(enreg[[ses]]$POS[,"boxname"])
      allpaths <- toString(r$values)
      allpaths<-strsplit(allpaths,"(?<=[ei])",perl=TRUE)[[1]]
      
      ## Find min dist b/w curr_box & next_box
      
      shortestPath <- get.shortest.paths(graph,curr_box,next_box)[[1]][[1]]
      shortestPath <- V(graph)$name[shortestPath]
      ### the trials where shortest path b/w current box and next box is present
      matchedtrials<-grep(paste(shortestPath[1],shortestPath[2],shortestPath[3],sep=", "),allpaths,perl=TRUE, value=FALSE)
      dist2 <- numeric()
      plot(spolygons)
      for (t in matchedtrials){
        all_xpos_t = enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),2]
        all_ypos_t = enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),3]
        spts = SpatialPoints(cbind(as.numeric(all_xpos_t),as.numeric(all_ypos_t)))
        min_start_pt = which.min(gDistance(curr_pt,spts,byid = TRUE))
        min_end_pt = which.min(gDistance(next_pt,spts,byid = TRUE))
        dist_t = sum((diff(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t)[min_start_pt:min_end_pt],2]))^2 + diff(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t)[min_start_pt:min_end_pt],3]))^2 )^0.5)
        print(sprintf("Distance calculated = %f",dist_t))
        spts=SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t)[min_start_pt:min_end_pt],2]),as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t)[min_start_pt:min_end_pt],3])))
        points(spts, pch=16, cex=.5,col = sample(colours(),1)) 
        dist2 <- c(dist2,dist_t)
      }
      if(length(dist2)==0){
        dist2 <- c(dist2,sum((diff(as.numeric(enreg[[ses]]$POS[i:(i+1),2]))^2 + diff(as.numeric(enreg[[ses]]$POS[i:(i+1),3]))^2 )^0.5))
      }
      ## If no trials found, then use the direct distance b/w points as displacement
      av_displacement = sum(dist2)/length(dist2)
      enreg[[ses]]$POS[i+1,"distance"] = av_displacement
    }
  }
  
  
  #write.table(as.data.frame(enreg[[ses]]$POS),file=sprintf("POS_session%i.csv",ses), quote=F,sep=",",row.names=F)
  
  enreg[[ses]]$POS[,"distance"] = cumsum(as.numeric(enreg[[ses]]$POS[,"distance"]))
  #print("Returning enreg from add.dist.to.pos")
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
  #enreg[[ses]]$SPIKES[x,"distance"] =  enreg[[ses]]$POS[i[x],"distance"]
  
  
  
  #print("Returning enreg from add.boxes.to.spikes")
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



change.tree.node=function(rat,animalNb,tree,enreg,ses){
  #print(paste("rat",animalNb,sep=","))
  animalNb  = gsub("rat_", "", animalNb)
  print(paste("rat",animalNb,sep=","))
  for(ses in 1:length(enreg)){
    if(is.null(enreg[[ses]])){
      print(sprintf("No enreg data for %s , ses %i", rat$name,ses))
      next
    }
    #temps, tetrode, neurone, remove last voltage values
    spks=enreg[[ses]]$SPIKES[,1:3] 
    vb=logical(length(spks[,1])) #add a vector of booleans
    vs=vector(mode="character", length=length(spks[,1])) #add a vector of strings
    spks=cbind(spks,vs,vs,vb)
    colnames(spks) <- c("time","tetrod","neuron","rightPath","boxName","boxSensitivity")
    #replace child from rat with new data enreg: 
    rat$RemoveChild(paste(animalNb,"_session_",ses,sep=""))
    rat$AddChild(paste(animalNb,"_session_",ses,sep=""),
                           #set fields (capital letters)
                           POS=enreg[[ses]]$POS,#temps, x,y
                           EVENTS=enreg[[ses]]$EVENT,#temps, evts
                           SPIKES=enreg[[ses]]$SPIKES,#temps, tetrode, neurone, rightPath,boxName,boxSensitivity
                           PATHS=enreg[[ses]]$PATHS,#trial, path, boxNm, activity
                           LFP=enreg[[ses]]$LFP)#temps, amplitude
  }
  return(tree)
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
    if(sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "49"))) + sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "51"))) >= 5){
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

  plot(reward_49,col='red',type='l',xlab="Session",ylab="Rewards")
  lines(reward_51,col='blue',type='l',lty=2)
  legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2,cex=0.5,bty = "n")
    
  plot(proportion_e,col='red',type='l',xlab="Session",ylab="Proportion of rewarded i/e visits")
  lines(proportion_i,col='blue',type='l',lty=2)
  #legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2,cex=0.5,bty = "n")
  
  plot(proportion_rewards,col='red',type='l',xlab="Session",ylab="Total proportion of rewarded i/e visits")
  #plot(duration,type="l",xlab="Session",ylab="Duration")
  #plot(average_spike_freq,type="l",xlab="Session",ylab="Average spike freq")
  # plot(1:length(enreg),reward_49,col='red',type='b',xlab="Session",ylab="Rewards")
  # lines(1:length(enreg),reward_51,col='blue',type='b',lty=2)
  # legend("topleft", legend=c("Reward 49", "Reward 51"),col=c("red", "blue"),lty = 1:2)
  #print("Returning enreg from plot.rewards")
}

plot.actions=function(enreg,spolygons){
  ## plot proportion of correct left turns at a
  ## lines proportion of correct left turns at a
  
  ##plot proportion of correct left turns at a
  ## lines proportion of correct left turns at a
  
  #pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),2]),as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49"),3])))
  # plot(spolygons)
  # points(pts, pch=16, cex=.5,col="red")
  
  
  a_visits <- numeric()
  c_visits <- numeric()
  reward_49 <- numeric()
  reward_51 <- numeric()
  gab <-numeric()
  kab <- numeric()
  bcd <- numeric()
  bch <- numeric()
  
  for(ses in 1:length(enreg)){
    
      if(sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "49"))) + sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "51"))) >= 5){
      #rewards <- c(rewards,(sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49))+sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49))))
      reward_49 <- c(reward_49,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49)))
      trial49 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="49"),"trial"])
      pos_trial49 <- which(as.numeric(enreg[[ses]]$POS[,"trial"]) %in% trial49)
      trial49r <- rle(enreg[[ses]]$POS[pos_trial49,"boxname"])
      out1<-strsplit(toString(trial49r$values),"e",fixed = TRUE)
      diff1 = sum(as.numeric(enreg[[ses]]$EVENTS[,2]==49)) - sum(ifelse(grepl("^, j, k, a, b, c, d, $",out1[[1]]), 1, 0))
      print(sprintf("Errors in 49 reward- %i",diff1))
      
      reward_51 <- c(reward_51,sum(as.numeric(enreg[[ses]]$EVENTS[,2]==51)))
      trial51 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51"),"trial"])
      pos_trial51 <- which(as.numeric(enreg[[ses]]$POS[,"trial"]) %in% trial51)
      trial51r <- rle(enreg[[ses]]$POS[pos_trial51,"boxname"])
      out2<-strsplit(toString(trial51r$values),"i",fixed = TRUE)
      diff2 = sum(as.numeric(enreg[[ses]]$EVENTS[,2]==51)) - sum(ifelse(grepl("^, f, g, a, b, c, h, $",out2[[1]]), 1, 0))
      print(sprintf("Errors in 51 reward- %i",diff2))
      
      
      r <- rle(enreg[[ses]]$POS[,"boxname"])
      a <-which(r$values=="a")
      gab <- length(which(r$values[a-1]=="g" & r$values[a+1]=="b"))
      kab <- length(which(r$values[a-1]=="k" & r$values[a+1]=="b"))
      
      c <-which(r$values=="c")
      bcd <- length(which(r$values[c-1]=="b" & r$values[c+1]=="d"))
      bch <- length(which(r$values[c-1]=="b" & r$values[c+1]=="h"))
      
      
      trial51 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51"),"trial"])
      # pos_trial51 <- which(as.numeric(enreg[[ses]]$POS[,"trial"]) %in% trial51)
      # rtrial51 <- rle(enreg[[1]]$POS[pos_trial49,"boxname"])
      
      a_visits <-  c(a_visits,sum(as.numeric(r$values=="a")))
      c_visits <-  c(c_visits,sum(as.numeric(r$values=="c")))
    }
  }
  
  
  prop_bcd_rewards <- reward_49/bcd  ### Proportion of rewarded bcd visits
  prop_bch_rewards <- reward_51/bch ### Proportion of rewarded bch visits
  par(mfrow=c(2,2))
  
  plot(prop_gab_rewards,col='red',type='l',xlab="Session",ylab="Proportion of rewarded \"gab\" visits")
  plot(prop_gab_rewards,col='red',type='l',xlab="Session",ylab="Proportion of rewarded \"kab\" visits")
  plot(prop_gab_rewards,col='red',type='l',xlab="Session",ylab="Proportion of rewarded \"bcd\" visits")
  plot(prop_gab_rewards,col='red',type='l',xlab="Session",ylab="Proportion of rewarded \"bch\" visits")
  
}


set.neurons.to.boxes=function(tree,rightPath,boites){
  # rightPath='abcdefg'
  # For each rat
    

  rat=tree$Get('name', filterFun = function(x) x$level == 3)
  
  setwd('..')
  path = getwd()
  time1 = format(Sys.time(), "%F %H-%M")
  dirpath1 = file.path(path,"Results","Plots",time1)
  dir.create(dirpath1)
  
  for (i in c(1:6)) {
    n=FindNode(tree,rat[[i]])
    enreg=convert.node.to.enreg(n)
    #print(enreg)
    #spols = list()
    for(ses in c(1:length(enreg))){
      #print(sprintf("Rat = %i , Session = %i",i,ses))
      
      if(is.null(enreg[[ses]])){
        #print(sprintf("No enreg in  %s session %i",rat[i], ses))
        next
      }else if(isempty(enreg[[ses]]$EVENTS)){
        #print(sprintf("No EVENT record in  %s session %i",rat[i], ses))
        next
      }
      
      boxes=boites
      spolygons=getSpatialPolygons(boxes)
      
      enreg=add.rewards.to.pos(ses,enreg)
      
      # if(sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "49"))) == 0 && sum(as.numeric(as.numeric(enreg[[ses]]$EVENTS[,2] == "51"))) == 0){
      #   print(sprintf("No rewards in  %s session %i",rat, ses))
      #   animalNb  = gsub("rat_", "", rat[i])
      #   #n$RemoveChild(paste(animalNb,"_session_",ses,sep=""))
      #   next
      # }
      
      ### Before adding boxes, shift POS if first POS recording is negative
      
      #if(sum(as.numeric(as.numeric(enreg[[ses]]$POS[,2])< 0)) > 0 || sum(as.numeric(as.numeric(enreg[[ses]]$POS[,3])< 0)) > 0){
      #   #debug(alignBoxes)
      #   shift=alignBoxes(enreg,ses,spolygons,boites)
      #   shiftx=shift[1]
      #   shifty=shift[2]
      #   # shiftx=130
      #   # shifty=129.5
      #   print(sprintf("Shifx=%f,shifty=%f",shiftx,shifty))
      #   lb=length(boxes)
      #   # enreg[[ses]]$POS[,2] = enreg[[ses]]$POS[,2]+shiftx
      #   # enreg[[ses]]$POS[,3] = enreg[[ses]]$POS[,3]+shifty
      #   for(r in 1:lb){
      #     boxes[[r]]=rbind(boxes[[r]][1,]-shiftx,boxes[[r]][2,]-shifty)
      #   }
      # 
      #   spolygons=getSpatialPolygons(boxes)
      #   # pts = SpatialPoints(cbind(as.numeric(enreg[[ses]]$POS[,2]),as.numeric(enreg[[ses]]$POS[,3])))
      #   # plot(spolygons)
      #   # points(pts, pch=16, cex=.5,col="red")
      #   #
      # }
      
      #print.plot.journeys(DATA,FindNode(tree,"Experiment in Marseille"),boites)

      
      ### Function to add boxname to POS data
      #debug(add.box.to.pos)
      enreg=add.box.to.pos(ses,enreg,spolygons)
      
      #debug(add.dist.to.pos)
      #enreg=add.dist.to.pos(ses,enreg,spolygons)
      
      #debug(add.boxes.to.spikes)
      ### Function to add boxname to SPIKE data
      enreg=add.boxes.to.spikes(ses,enreg)
      
      
      #debug(plot.spikes.by.boxes.by.session)
      ##plot.spikes.by.boxes.by.session(rat[i],enreg,ses,dirpath)
      #debug(plot.average.frequency.by.boxes)
      #plot.average.frequency.by.boxes(rat[i],enreg,ses,dirpath)
      #debug(plot.average.frequency.by.boxes2)
      #plot.average.frequency.by.boxes2(rat[i],enreg,ses,dirpath)
      #debug(plot.spikes.by.time)
      #plot.spikes.by.time(rat[i],enreg,ses,dirpath)
      #debug(plot.spikes.by.distance)
      #plot.spikes.by.distance(rat[i],enreg,ses,dirpath)
    }
    #debug(change.tree.node)
    #tree=change.tree.node(n,rat[i],tree,enreg,ses)
    #debug(plot.rewards,dirpath)
    #plot.rewards(enreg)
    
    # path = getwd()
    # time1 = format(Sys.time(), "%F %H-%M")
    # dirpath = file.path(path,"Plots",time1)
    # dir.create(dirpath)
    
    
    # dirpath1 = file.path(path,"Plots",time1,"Neuron1")
    # dir.create(dirpath1)
    # dirpath2 = file.path(path,"Plots",time1,"Neuron2")
    # dir.create(dirpath2)
    # dirpath3 = file.path(path,"Plots",time1,"Neuron3")
    # dir.create(dirpath3)
    # dirpath4 = file.path(path,"Plots",time1,"Neuron4")
    # dir.create(dirpath4)
    # dirpath5 = file.path(path,"Plots",time1,"Neuron5")
    # dir.create(dirpath5)
    # dirpath6 = file.path(path,"Plots",time1,"Neuron6")
    # dir.create(dirpath6)
    
    
    #debug(plot.c.turn.event.by.time)
    #plot.c.turn.event.by.time(enreg,dirpath,rat[i])
    # plot.c.turn.event.by.time(enreg,dirpath2,rat[i],"2")
    # plot.c.turn.event.by.time(enreg,dirpath3,rat[i],"3")
    # plot.c.turn.event.by.time(enreg,dirpath4,rat[i],"4")
    # plot.c.turn.event.by.time(enreg,dirpath5,rat[i],"5")
    # plot.c.turn.event.by.time(enreg,dirpath6,rat[i],"6")
    #debug(plot.c.turn.event.by.distance)
    #plot.c.turn.event.by.distance(enreg,dirpath,rat[i])
    # plot.c.turn.event.by.distance(enreg,dirpath2,rat[i],"2")
    # plot.c.turn.event.by.distance(enreg,dirpath3,rat[i],"3")
    # plot.c.turn.event.by.distance(enreg,dirpath4,rat[i],"4")
    # plot.c.turn.event.by.distance(enreg,dirpath5,rat[i],"5")
    # plot.c.turn.event.by.distance(enreg,dirpath6,rat[i],"6")
    
    
    
    
    #plot.reward_proportion(enreg,rat[i])
    
    #debug(plot.task.errors)
    #plot.task.errors(enreg,rat[i],dirpath1)
    
    #debug(plot.heatmap)
    #plot.heatmap(enreg,rat[i],dirpath1)
    
    #plot.average.frequency.by.boxes2(enreg,rat[i],dirpath1)
  }
  return(tree)
}