# Handle incomplete paths in the begining or when recording is lost
updateACAPathNbmse=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      allpaths[i,4] = 1
    }else{
      allpaths[i,4] = 0
    }
    allpaths[i,3] = getPathNumber(allpaths[i,1])
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,5]=1
    }else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
      ## If cannot be estimated, then do by default : assume the trial = Path5
      else if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,5]=2
      }
      
    }else if(i==1){
      if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=2
      }else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      
    }
    
  }
  return(allpaths)
}


genInitValues=function(allpaths,sim){
  H <- matrix(0,2,6)
  twoHundredActions <- allpaths[1:100,c(1,2)]
  if(sim==1){
    stateOne = 0
    stateTwo = 1
    actRange = c(0:5)
  }else{
    stateOne = 1
    stateTwo = 2
    actRange = c(1:6)
  }
  
  stateOneVisits = which(twoHundredActions[,2]==stateOne)
  stateTwoVisits = which(twoHundredActions[,2]==stateTwo)
  for(act in actRange){
    for(state in c(stateOne:stateTwo)){
      if(state == stateOne){
        actCounter = length(which(twoHundredActions[stateOneVisits,1] == act))
        if(sim == 1){
          H[(state+1),(act+1)]= actCounter/length(stateOneVisits)
        }else{
          H[state,act]= actCounter/length(stateOneVisits)
        }
        
        
      }else{
        actCounter = length(which(twoHundredActions[stateTwoVisits,1] == act))
        
        if(sim == 1){
          H[(state+1),(act+1)]= actCounter/length(stateTwoVisits)
        }else{
          H[state,act]= actCounter/length(stateTwoVisits)
        }
      }
      
    }
  }
  
  
  return(H)
}

boxplotMse = function(mat_res, model,rat){
  
  if(model == 1){
    jpeg(paste("boxplot_ACA_",rat,".jpeg",sep=""))
  }else if(model == 2){
    jpeg(paste("boxplot_GB_",rat,".jpeg",sep=""))
  }else if(model == 3){
    jpeg(paste("boxplot_GB_ACA_",rat,".jpeg",sep=""))
  }else if(model == 4){
    jpeg(paste("boxplot_ACA2_",rat,".jpeg",sep=""))
  }else if(model == 5){
    jpeg(paste("boxplot_ACA3_",rat,".jpeg",sep=""))
  }
  
  boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),xaxt="n")
  axis(side=1, at=c(1,2), labels = c("ACA","GB"))
  dev.off()
}

checkValidation=function(mat_res, model,rat){
  ret = TRUE 
  if(model==1){
    if(length(which(mat_res[,7]=="ACA")) < 70){
      print(sprintf("ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==2){
    if(length(which(mat_res[,7]=="GB")) < 70){
      print(sprintf("GB is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==3){
    if(length(which(mat_res[,7]=="ACA_GB")) < 70){
      print(sprintf("GB_ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB_ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==4){
    if(length(which(mat_res[,7]=="ACA2")) < 70){
      print(sprintf("ACA2 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA2 is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==5){
    if(length(which(mat_res[,7]=="ACA3")) < 70){
      print(sprintf("ACA3 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA3 is selected more than 70 times for %s.",rat))
    }
  }
  return(ret)
}


generatePlots=function(rat,allpaths,GBprobMatrix, ACAprobMatrix, ACA2probMatrix, ACA3probMatrix){
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("Prob_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      plot(GBprobMatrix[which(GBprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability",main=paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ))
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      lines(ACAprobMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='green',type='l')
      lines(ACA2probMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='orange',type='l')
      lines(ACA3probMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='red',type='l')
      
      if(act==4||act==10){
        lines(movavg(allpaths[which(ACAprobMatrix[,(act+6*(state-1))]!=0),3],100),col='blue',lty=2)
        legend("bottomright", legend=c("Prob. of reward for GB", "Prob. of reward for GB_ACA","Prob. of reward for ACA", "Mov. Avg of Reward/100 trials"),col=c("black","green","orange","red", "blue"),cex=0.6,lty = c(1,1,1,1,2))
        
      }else{
        lines(movavg(as.numeric(allpaths[,1]== act & allpaths[,2]==state),10),col='blue',lty=2)
        legend("topright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for ACA2","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.6,lty = c(1,1,1,1,2))
        
      }
      dev.off()
    }
  }
  
}

getMovingAverage=function(allpaths,window,sim){
  colLen = length(allpaths[,1])
  movingAvg = numeric(colLen)
  stateOne = 1
  stateTwo = 2
  if(sim == 1){
    stateOne = 0
    stateTwo = 1
  }
  state1Idx <- which(allpaths[,2]==stateOne)
  state2Idx <- which(allpaths[,2]==stateTwo)
  
  for(trial in c(1:colLen)){
    
    action = allpaths[trial,1]
    state = allpaths[trial,2]
    curr_state_idx <- which(allpaths[1:trial,2]==state)
    if(length(curr_state_idx) > window){
      curr_state_idx <- curr_state_idx[(length(curr_state_idx)-window):length(curr_state_idx)]
      
    }
    movingAvg[trial] <- length(which(allpaths[curr_state_idx,1]== action))/length(curr_state_idx)
    
  }
  return(movingAvg)
}

getStartIndex = function(generated_data){
  start_index=0
  l<-which(SMA(generated_data[,3],30)>=0.6)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      start_index=k[[set]][1]
      break
    }
  }
  return(start_index)
}

getEndIndex = function(generated_data){
  end_index=0
  l<-which(SMA(generated_data[,3],30)>=0.95)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      end_index=k[[set]][1]
      break
    }
  }  
  return(end_index)
}

enregCombine=function(enreg,rat){
  allpaths <- matrix("",0,2)
  colnames(allpaths) <- c("Path","Session")
  ### Loop through all enreg[[ses]] of current rat
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      #print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      #print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }else if(rat=="rat_106" && ses==3){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_112" && ses==1){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_113" && ses==13){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }
    
    #last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    #reward49_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    #reward51_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths_ses <- toString(r$values)
    allpaths_ses <-strsplit(allpaths_ses,"(?<=[ei])(?=(, j, k)|(, j, a)|(, j, b)|(, f, g)|(, f, b)|(, f, a)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths_ses <-cbind(allpaths_ses,ses)
    colnames(allpaths_ses) <- c("Path","Session")
    l<-list(allpaths,allpaths_ses)
    allpaths <- rbind(allpaths,allpaths_ses)
    
  }
  
  return(allpaths)
}