library(dplyr)

updateACAPathNb=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0)
  for(i in 1:(length(allpaths[,1]))){
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      allpaths[i,4] = R
    }else{
      allpaths[i,4] = 0
    }
    allpaths[i,3] = getPathNumber(allpaths[i,1])
  }
  return(allpaths)
}




getPathNumber=function(path){
  path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i$",path)){
    pathnb = 1
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 2
  }else if(grepl("^f.*g.*a.*k.*j.*i$",path)){
    pathnb = 3
  }else if(grepl("^j.*i$",path)){
    pathnb = 5
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i$",path)){
    pathnb = 4
  }else if(grepl("^h.*c.*d.*e$",path)){
    pathnb = 1
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 2
  }else if(grepl("^j.*k.*a.*g.*f.*e$",path)){
    pathnb = 3
  }else if(grepl("^f.*e$",path)){
    pathnb = 5
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e$",path)){
    pathnb = 4
  }else if(grepl("^.*e$",path)){
    pathnb = 6
  }else if(grepl("^.*i$",path)){
    pathnb = 6
  }else{
    ## A =7
    pathnb=6
  }
  
  return(pathnb)
}


updateACAPathNb1=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      allpaths[i,4] = R
    }else{
      allpaths[i,4] = 0
    }
    allpaths[i,3] = getPathNumber(allpaths[i,1])
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,5]=1
    }else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }
    ## Why ? ( incomplete paths ?)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
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
    }
    else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }
      else if(grepl("^.*i$",allpaths[i-1,1])){
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
  
  boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),as.numeric(mat_res[,5]), as.numeric(mat_res[,7]), xaxt="n")
  axis(side=1, at=c(1,2,3,4), labels = c("ACA","GB", "ACA2", "ACA3"))
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


generatePlots=function(rat,empiricalProbMatrix, ACAprobMatrix, GBprobMatrix,  SARSAprobMatrix, ACA3probMatrix){
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("Prob_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      plot(GBprobMatrix[which(GBprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability",main=paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ))
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      lines(ACAprobMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='green',type='l')
      lines(SARSAprobMatrix[which(SARSAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='orange',type='l')
      lines(ACA3probMatrix[which(ACA3probMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='red',type='l')
      lines(empiricalProbMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='blue',type='l',lty=2)
      
      if(act==4||act==10){
        legend("bottomright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,2))
        
      }else{
        legend("topright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,2))
        
      }
      dev.off()
    }
  }
  
}

getEmpProbMat=function(allpaths,window,sim){
  colLen = length(allpaths[,1])
  empProbMat = numeric(colLen)
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
    empProbMat[trial] <- length(which(allpaths[curr_state_idx,1]== action))/length(curr_state_idx)
    
  }
  return(empProbMat)
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

getEndIndex = function(generated_data, sim){
  if(sim==1){
    generated_data[,1:2] = generated_data[,1:2] + 1
  }
  end_index1=0
  s1 <- which(generated_data[,2]==1)
  l<-which(SMA(generated_data[s1,3],30)>=0.95)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      end_index1=k[[set]][1]
      break
    }
  }
  
  
  end_index2=0
  s2 <- which(generated_data[,2]==2)
  l<-which(SMA(generated_data[s2,3],30)>=0.95)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      end_index2=k[[set]][1]
      break
    }
  }
  
  if(end_index1==0 && end_index2 ==0){
    end_index = -1
  }else{
    end_index = max(s1[end_index1],s2[end_index2])
  }
  
  #print(sprintf("end_index=%i", end_index))
  
  return(end_index)
}

enregCombine=function(enreg,rat){
  allpaths <- matrix("",0,2)
  colnames(allpaths) <- c("Path","Session")
  boxTimes <- vector()
  boxcount1 = 0
  boxcount2 = 0
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
    #boxtime_ses <- baseModels::getBoxTimes(as.numeric(enreg[[ses]]$POS[,1]), r$lengths)
    #print(sprintf("boxes - times = %i, sess = %i", (length(boxtime_ses)-length(r$values)), ses))
    #boxcount1  = boxcount1 + (length(boxtime_ses))
    #k<-gsub(", ", "", toString(allpaths_ses[,1]), fixed = TRUE)
    #boxcount2  = boxcount2 + nchar(k)                    
    #print(sprintf("boxCount = %i, boxCount_allpaths = %i, sess = %i", boxcount1,boxcount2,ses))
    boxTimes <- c(boxTimes, baseModels::getBoxTimes(as.numeric(enreg[[ses]]$POS[,1]), r$lengths))
  }
  
  return(list("allpaths" = allpaths, "boxTimes" = boxTimes))
}

locate_xtrem <- function (x, last = FALSE)
{
  # use rle to deal with duplicates
  x_rle <- rle(x)
  
  # force the first value to be identified as an extrema
  first_value <- x_rle$values[1] - x_rle$values[2]
  
  # differentiate the series, keep only the sign, and use 'rle' function to
  # locate increase or decrease concerning multiple successive values.
  # The result values is a series of (only) -1 and 1.
  #
  # ! NOTE: with this method, last value will be considered as an extrema
  diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()
  
  # this vector will be used to get the initial positions
  diff_idx <- cumsum(diff_sign_rle$lengths)
  
  # find min and max
  diff_min <- diff_idx[diff_sign_rle$values < 0]
  diff_max <- diff_idx[diff_sign_rle$values > 0]
  
  # get the min and max indexes in the original series
  x_idx <- cumsum(x_rle$lengths)
  if (last) {
    min <- x_idx[diff_min]
    max <- x_idx[diff_max]
  } else {
    min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
    max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
  }
  # just get number of occurences
  min_nb <- x_rle$lengths[diff_min]
  max_nb <- x_rle$lengths[diff_max]
  
  # format the result as a tibble
  bind_rows(
    tibble(Idx = min, Values = x[min], NB = min_nb, Status = "min"),
    tibble(Idx = max, Values = x[max], NB = max_nb, Status = "max")) %>%
    arrange(.data$Idx) %>%
    mutate(Last = last) %>%
    mutate_at(vars(.data$Idx, .data$NB), as.integer)
}


computeActivity = function(probVector, window){
  
  activityVec = numeric(length(c(probVector)))
  for(i in c(2:(length(probVector)))){
    
    if(i <= window){
      if(sum(probVector[1:i]) != 0 && length(unique(round(probVector[1:i],4))) != 1){
        res = locate_xtrem(round(probVector[1:i],4))
        activityVec[i] = sum(abs(diff(res$Values)))
      }
      
    }else{
      
      if(sum(probVector[(i-window):i]) != 0 && length(unique(round(probVector[(i-window):i],4))) != 1){
        res = locate_xtrem(round(probVector[(i-window):i],4))
        activityVec[i] = sum(abs(diff(res$Values)))
      }
      
    }
  }
  
  # if(!all(probVector == 0)){
  #   res = locate_xtrem(round(probVector,4))
  #   activitySum = sum(abs(diff(res$Values)))
  # }else{
  #   activitySum = 0
  # }
  
  return(activityVec)
}

plotData = function(res,rat, ranges){
  
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$acamse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$acamse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA"))
      lines(res$acamse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$acamse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$gbmse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$gbmse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " GB"))
      lines(res$gbmse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$gbmse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca2mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$aca2mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA2"))
      lines(res$aca2mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca2mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca3mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$aca3mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA3"))
      lines(res$aca3mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca3mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  

}


plotData2 = function(res,rat, ranges){
  
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$acamse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$acamse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA"))
      lines(res$acamse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$acamse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$gbmse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$gbmse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " GB"))
      lines(res$gbmse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$gbmse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca2mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$aca2mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA2"))
      lines(res$aca2mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca2mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca3mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$aca3mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA3"))
      lines(res$aca3mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca3mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  
}
