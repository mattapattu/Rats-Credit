plot.task.errors=function(enreg,rat,dirpath1,pathProb){

  procedural_err <-numeric()
  wm_err <- numeric()
  perseverance_err <- numeric()
  unk_err <- numeric()
  type1 <- numeric()
   type2 <- numeric()
  # type1_repeat <- numeric()
  # type2_repeat <- numeric()
  corr_comb <- numeric()
  # corr_comb_repeat <- numeric()
  
  pathProbMatrix49 <- matrix(0,6,length(enreg))
  rownames(pathProbMatrix49)<-c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path")

  pathProbMatrix51 <- matrix(0,6,length(enreg))
  rownames(pathProbMatrix51)<-c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path")
  
  path49CorrCombProbMatrix <- matrix(0,6,length(enreg))
  rownames(path49CorrCombProbMatrix)<-c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path")
  
  path51CorrCombProbMatrix <- matrix(0,6,length(enreg))
  rownames(path51CorrCombProbMatrix)<-c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path")
  
  dirpath2 = file.path(dirpath1,rat)
  dir.create(dirpath2)
  
  dirpath3 = file.path(dirpath1,rat,"allpaths_Rdata")
  dir.create(dirpath3)
  
  dirpath4 = file.path(dirpath1,rat,"prob_Rdata")
  dir.create(dirpath4)
  
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      pathProbMatrix49[,ses] <- NA
      pathProbMatrix51[,ses] <- NA
      path49CorrCombProbMatrix[,ses]<-NA
      path51CorrCombProbMatrix[,ses]<-NA
      
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      pathProbMatrix49[,ses] <- NA
      pathProbMatrix51[,ses] <- NA
      path49CorrCombProbMatrix[,ses]<-NA
      path51CorrCombProbMatrix[,ses]<-NA
      next
    }
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths<-cbind(allpaths, Rewards="0",Error="0",Path="0")
  
    prev49Rewarded= FALSE
    prev51Rewarded=FALSE
    prev49WMerr = FALSE
    prev51WMerr = FALSE
    
    for(trial in 1:length(allpaths[,1])){
      
      allpaths[trial,"Path"] = getPathNb(allpaths[trial,1])
      
      ### If 49 rewarded, update trial with reward and error=0
      if(sum(as.numeric(as.numeric(enreg[[ses]]$POS[,"trial"])==trial & enreg[[ses]]$POS[,"Reward"]==49))==1){
        allpaths[trial,"Rewards"]=49
        prev49Rewarded= TRUE
        prev51Rewarded=FALSE
        prev49WMerr = FALSE
        prev51WMerr = FALSE
      }
      ### If 51 rewarded, update trial with reward and error=0
      else if(sum(as.numeric(as.numeric(enreg[[ses]]$POS[,"trial"])==trial & enreg[[ses]]$POS[,"Reward"]==51))==1){
        allpaths[trial,"Rewards"]=51
        prev49Rewarded= FALSE
        prev51Rewarded=TRUE
        prev49WMerr = FALSE
        prev51WMerr = FALSE
      }
      ### If No reward, update trial with proper error
      else{
        patternsPrEr <- "d.*c.*.*h.*i|h.*c.*.*d.*e|c.*b.*a|a.*k.*j.*i|a.*g.*f.*e"
        ### Check if error is procedural error
        if(grepl(patternsPrEr,allpaths[trial])){
          ### There is procedure error
          allpaths[trial,"Error"]="Procedural Err"
          prev49WMerr = FALSE
          prev51WMerr = FALSE
        }
        ### Check if error is WM 49 error or repeated WM error (Perseverative 49 Err )
        else if(grepl("j.*k.*a.*b.*c.*h.*i",allpaths[trial])){
          
          if(prev51WMerr){
            allpaths[trial,"Error"]="Perseverative Err - 51"
          }else{
            allpaths[trial,"Error"]="WM Err - 51"
          }
          prev49WMerr = FALSE
          prev51WMerr = TRUE
        }
        ### Check if error is WM 51 error or repeated WM error (Perseverative 51 Err )
        else if(grepl("f.*g.*a.*b.*c.*d.*e",allpaths[trial])){
          
          #### If rat re-enters same arm after gettign rewarded in previous trial, set err to WM err
          ### If rat repeats WM error, then set error to Preseverence Err
          if(prev49WMerr){
            allpaths[trial,"Error"]="Perseverative Err - 49"
          }else{
            allpaths[trial,"Error"]="WM Err - 49"
          }
          prev49WMerr = TRUE
          prev51WMerr = FALSE
          
        }else{
          allpaths[trial,"Error"]="Unknown Err"
        }
        prev49Rewarded= FALSE
        prev51Rewarded=FALSE
      }
    }
    
    newlist <- getAllPathsStats(allpaths)
    path_matrix49 <- newlist$path_matrix49
    path_matrix51 <- newlist$path_matrix51
    path_comb_matrix <- newlist$path_comb_matrix
    
    type1 <- c(type1,path_comb_matrix[1,4])
    type2 <- c(type2,path_comb_matrix[4,1])
    # type1_repeat <- c(type1_repeat,newlist[3])
    # type2_repeat <- c(type2_repeat,newlist[4])
    corr_comb <- c(corr_comb,path_comb_matrix[4,4])
    # corr_comb_repeat <- c(corr_comb_repeat,newlist[6])
    
    if(pathProb){
      pathProbMatrix49[,ses] <- path_matrix49[,1]/sum(path_matrix49)
      pathProbMatrix51[,ses] <- path_matrix51[,1]/sum(path_matrix51)
      path49CorrCombProbMatrix[,ses] <- t(path_comb_matrix[4,]/sum(path_comb_matrix))
      path51CorrCombProbMatrix[,ses] <- path_comb_matrix[,4]/sum(path_comb_matrix)
    }
    
    wm_err <-c(wm_err,length(which(allpaths[,"Error"]=="WM Err - 51"|allpaths[,"Error"]=="WM Err - 49"))) 
    perseverance_err <-c(perseverance_err,length(which(allpaths[,"Error"]=="Perseverative Err - 49"|allpaths[,"Error"]=="Perseverative Err - 51"))) 
    procedural_err <- c(procedural_err,length(which(allpaths[,"Error"]=="Procedural Err")))
    unk_err <- c(unk_err ,length(which(allpaths[,"Error"]=="Unknown Err")))

    filename = file.path(dirpath3,paste(rat,'_ses_',ses,'.RData',sep=""))   
    save(allpaths,file=filename)

    filename = file.path(dirpath4,paste(rat,'_probabilities_ses_',ses,'.RData',sep=""))   
    save(path_matrix49,path_matrix51,path_comb_matrix,file=filename)
  }
  

  
  
  #filename = paste(rat,"_task_errors",".jpg",sep="")
  filename=file.path(dirpath1,paste(rat,"_task_errors",".jpg",sep=""))
  jpeg(filename)
  plot(procedural_err,col='black',type='l',ylim=range(0,procedural_err),xlab="Sessions",ylab="Nb of Errors",main=paste(rat,"_errors per session",sep=""))
  lines(wm_err,col='red')
  lines(perseverance_err,col='blue')
  lines(unk_err,col='green')
  legend("topright", legend=c("Nb of procedural errors", "Nb of WM errors","Nb of perseverance errors", "Nb of unknown errors"),lty=c(1,1,1),col=c("black", "red","blue","green"),cex=0.9,bty = "n")
  dev.off()
  
  filename=file.path(dirpath1,paste(rat,"_pathPatterns",".jpg",sep=""))
  jpeg(filename)
  plot(type1,col='black',type='l',ylim=range(0,type1,type2,corr_comb),xlab="Sessions",ylab="Nb of Patterns",main=paste(rat," Path Patterns",sep=""))
  lines(type2,col='red')
  lines(corr_comb,col='blue')
  legend("topright", legend=c("Path1 49 - 51 correct", "Path1 51 - 49 correct","49 corr -51 corr"),lty=c(1,1,1),col=c("black", "red","blue"),cex=0.9,bty = "n")
  dev.off()
  
  # filename=file.path(dirpath1,paste(rat,"_pathPatternRepeats",".jpg",sep=""))
  # jpeg(filename)
  # plot(type1_repeat,col='black',type='l',ylim=range(0,type1_repeat,type2_repeat,corr_comb_repeat),xlab="Sessions",ylab="Nb of Patterns",main=paste(rat," Path Patterns",sep=""))
  # lines(type2_repeat,col='red')
  # lines(corr_comb_repeat,col='blue')
  # legend("topright", legend=c( "Path1 49 - 51 correct repeat","Path1 51 - 49 correct repeat","49 corr -51 corr repeat"),lty=c(1,1,1),col=c("black", "red","blue"),cex=0.9,bty = "n")
  # dev.off()
  
  ###
  filename=file.path(dirpath1,paste(rat,"pathProbAll49",".jpg",sep=""))
  jpeg(filename,width=800,height=800,quality = 100)
  plot(pathProbMatrix49[1,],col='black',type='l',ylim=range(0,1),xlab="Sessions",ylab="Probability of All 49 Paths",main=paste(rat,"_pathProbAll49",sep=""))
  lines(pathProbMatrix49[2,],type='l',col='red',lty=2)
  lines(pathProbMatrix49[3,],type='l',col='blue',lty=3)
  lines(pathProbMatrix49[4,],type='l',col='green',lty=4)
  lines(pathProbMatrix49[5,],type='l',col='coral4',lty=5)
  lines(pathProbMatrix49[6,],type='l',col='violet',lty=6)
  legend("topright", legend=c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path"),lty=c(1,2,3,4,5,6),col=c("black", "red","blue","green","coral4","violet"),cex=0.9,bty = "n")
  dev.off()
  
  filename=file.path(dirpath1,paste(rat,"pathProbAll51",".jpg",sep=""))
  jpeg(filename,width=800,height=800,quality = 100)
  plot(pathProbMatrix51[1,],col='black',type='l',ylim=range(0,1),xlab="Sessions",ylab="Probability of All 51 Paths",main=paste(rat,"_pathProbAll51",sep=""))
  lines(pathProbMatrix51[2,],type='l',col='red',lty=2)
  lines(pathProbMatrix51[3,],type='l',col='blue',lty=3)
  lines(pathProbMatrix51[4,],type='l',col='green',lty=4)
  lines(pathProbMatrix51[5,],type='l',col='coral4',lty=5)
  lines(pathProbMatrix51[6,],type='l',col='violet',lty=6)
  legend("topright", legend=c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path"),lty=c(1,2,3,4,5,6),col=c("black", "red","blue","green","coral4","violet"),cex=0.9,bty = "n")
  dev.off()
  
  filename=file.path(dirpath1,paste(rat,"pathCorr49CombProb",".jpg",sep=""))
  jpeg(filename,width=800,height=800,quality = 100)
  plot(path49CorrCombProbMatrix[1,],col='black',type='l',ylim=range(0,1),xlab="Sessions",ylab="Probability of All Corr 49 Combinations",main=paste(rat,"_path49CorrCombProb",sep=""))
  lines(path49CorrCombProbMatrix[2,],type='l',col='red',lty=2)
  lines(path49CorrCombProbMatrix[3,],type='l',col='blue',lty=3)
  lines(path49CorrCombProbMatrix[4,],type='l',col='green',lty=4)
  lines(path49CorrCombProbMatrix[5,],type='l',col='coral4',lty=5)
  lines(path49CorrCombProbMatrix[6,],type='l',col='violet',lty=6)
  legend("topright", legend=c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path"),lty=c(1,2,3,4,5,6),col=c("black", "red","blue","green","coral4","violet"),cex=0.9,bty = "n")
  dev.off()
  
  filename=file.path(dirpath1,paste(rat,"pathCorr51CombProb",".jpg",sep=""))
  jpeg(filename,width=800,height=800,quality = 100)
  plot(path51CorrCombProbMatrix[1,],col='black',type='l',ylim=range(0,1),xlab="Sessions",ylab="Probability of All Corr 51 Combinations",main=paste(rat,"_path51CorrCombProb",sep=""))
  lines(path51CorrCombProbMatrix[2,],type='l',col='red',lty=2)
  lines(path51CorrCombProbMatrix[3,],type='l',col='blue',lty=3)
  lines(path51CorrCombProbMatrix[4,],type='l',col='green',lty=4)
  lines(path51CorrCombProbMatrix[5,],type='l',col='coral4',lty=5)
  lines(path51CorrCombProbMatrix[6,],type='l',col='violet',lty=6)
  legend("topright", legend=c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path"),lty=c(1,2,3,4,5,6),col=c("black", "red","blue","green","coral4","violet"),cex=0.9,bty = "n")
  dev.off()
  
  print("Exiting ")
  
}
###############################################################################################################################33
################################################################################################################################
getPathNb=function(path){
  path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i",path)){
    pathnb = "Path1 51"
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i",path)){
    pathnb = "Path2 51"
  }else if(grepl("^f.*g.*a.*k.*j.*i",path)){
    pathnb = "Path3 51"
  }else if(grepl("^j.*k.*a.*b.*c.*h.*i",path)){
    pathnb = "WM 51 path"
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i",path)){
    pathnb = "Corr 51 Path"
  }else if(grepl("^h.*c.*d.*e",path)){
    pathnb = "Path1 49"
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e",path)){
    pathnb = "Path2 49"
  }else if(grepl("^j.*k.*a.*g.*f.*e",path)){
    pathnb = "Path3 49"
  }else if(grepl("^f.*g.*a.*b.*c.*d.*e",path)){
    pathnb = "WM 49 Path"
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e",path)){
    pathnb = "Corr 49 Path"
  }else{
    pathnb = "Unknown Path"
  }
  
  return(pathnb)
}
######################################################################################################3
######################################################################################################3
getAllPathsStats=function(allpaths){
  
  l <- toString(allpaths[,"Path"])
  path_comb_matrix <- matrix(0, nrow = 6, ncol = 6)
  colnames(path_comb_matrix)<-c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path")
  rownames(path_comb_matrix)<-c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path")
  
  for(row in rownames(path_comb_matrix)){
    for(col in colnames(path_comb_matrix)){
      regexpr=paste0("((",row,"|",col,"), (",col,"|",row,"), )")
      #print(regexpr)
      output=(unlist(gregexpr(regexpr,l)))
      #print(output)
      if(output[1] != -1){
        len = length(output)
      }else{
        len=0
      }
      #print(len)
      path_comb_matrix[row,col]=len
    }
  }
  
  path_matrix49 <- matrix(0, nrow = 6, ncol = 1)
  rownames(path_matrix49)<-c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path")
  colnames(path_matrix49)<-c("Count")
  
  for(row in rownames(path_matrix49)){
      output=(unlist(gregexpr(row,l)))
      #print(output)
      if(output[1] != -1){
        len = length(output)
      }else{
        len=0
      }
      #print(len)
      path_matrix49[row,1]=len
  }
  
  
  path_matrix51 <- matrix(0, nrow = 6, ncol = 1)
  rownames(path_matrix51)<-c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path")
  colnames(path_matrix51)<-c("Count")
  
  for(row in rownames(path_matrix51)){
    output=(unlist(gregexpr(row,l)))
    #print(output)
    if(output[1] != -1){
      len = length(output)
    }else{
      len=0
    }
    #print(len)
    path_matrix51[row,1]=len
  }
  
  # type1 = (unlist(gregexpr("((Path1 49|Corr 51 Path), (Path1 49|Corr 51 Path), )",l)))
  # if(type1[1] != -1){
  #   type1 = length(type1)
  # }else{
  #   type1=0
  # }
  # type2 = (unlist(gregexpr("((Corr 49 Path|Path1 51), (Corr 49 Path|Path1 51), )",l)))
  # if(type2[1] != -1){
  #   type2 = length(type2)
  # }else{
  #   type2=0
  # }
  # 
  # type1_repeat = (unlist(gregexpr("((Path1 49|Corr 51 Path), (Path1 49|Corr 51 Path), )\\1",l)))
  # if(type1_repeat[1] != -1){
  #   type1_repeat = length(type1_repeat)
  # }else{
  #   type1_repeat=0
  # }
  # 
  # type2_repeat = (unlist(gregexpr("((Corr 49 Path|Path1 51), (Corr 49 Path|Path1 51), )\\1",l)))
  # if(type2_repeat[1] != -1){
  #   type2_repeat = length(type2_repeat)
  # }else{
  #   type2_repeat=0
  # }
  # 
  # corr_comb=(unlist(gregexpr("((Corr 51 Path|Corr 49 Path), (Corr 49 Path|Corr 51 Path), )",l)))
  # if(corr_comb[1] != -1){
  #   corr_comb = length(corr_comb)
  # }else{
  #   corr_comb=0
  # }
  # corr_comb_repeat = (unlist(gregexpr("((Corr 51 Path|Corr 49 Path), (Corr 49 Path|Corr 51 Path), )\\1",l)))
  # if(corr_comb_repeat[1] != -1){
  #   corr_comb_repeat = length(corr_comb_repeat)
  # }else{
  #   corr_comb_repeat=0
  # }
  # 
  # 
  # newlist <- c(type1,type2,type1_repeat,type2_repeat,corr_comb,corr_comb_repeat)
  
  results <- list()
  results$path_matrix49 <- path_matrix49
  results$path_matrix51 <- path_matrix51
  results$path_comb_matrix <- path_comb_matrix
  return(results)
  
}

##########################################################################333
########################################################################

plot.reward_proportion=function(enreg,rat){
  
  if(grepl("103", rat)){
    seslist <- c(1,3,4,25,26,27,52,53,55)
  }else if(grepl("113", rat)){
    seslist <- c(1,2,3,15,16,17)
  }
  
  for(ses in seslist){
    
    if(is.null(enreg[[ses]])){
      next
    }
    if(sum(enreg[[ses]]$EVENTS[,2]==49)+sum(enreg[[ses]]$EVENTS[,2]==51)==0){
      print(sprintf("No rewards in session %i",ses))
      next
    }
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k,)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
    correct_turns <- numeric()
    correct_left <- numeric()
    correct_right <- numeric()
    proportion_correct <- numeric()
    proportion_correct_left <- numeric()
    proportion_correct_right <- numeric()
    reward_proportion <- numeric()
    
    turnLeft=FALSE
    turnRight=FALSE
    rewarded = FALSE
    turnLeft = FALSE
    
    
    
    for(trial in 1:length(allpaths)){
      
      rewarded= FALSE
      turnRight=FALSE
      turnLeft = FALSE
      
      
      if(sum(as.numeric(enreg[[ses]]$POS[,"trial"]==trial & enreg[[ses]]$POS[,"Reward"]!=0))){
        rewarded = TRUE
      }
      ## If turn right
      if(length(grep("a,.*b,.*c.*e",allpaths[trial],value = FALSE))>0){
        turnRight= TRUE
        
        if(rewarded){
          correct_turns<-c(correct_turns,1)
          correct_right<-c(correct_right,1)
          correct_left<-c(correct_left,NA)
        }else{
          correct_turns<-c(correct_turns,0)
          correct_right<-c(correct_right,0)
          correct_left<-c(correct_left,NA)
        }
      }
      ### turn Left
      else if(length(grep("a,.*b,.*c.*i",allpaths[trial],value = FALSE)>0)){
        turnLeft = TRUE
        
        if(rewarded){
          correct_turns<-c(correct_turns,1)
          correct_right<-c(correct_right,NA)
          correct_left<-c(correct_left,1)
        }else{
          correct_turns<-c(correct_turns,0)
          correct_right<-c(correct_right,NA)
          correct_left<-c(correct_left,0)
        }
      }
      ### No turn at C
      # else{
      #   correct_turns<-c(correct_turns,NA)
      #   correct_right<-c(correct_right,NA)
      #   correct_left<-c(correct_left,NA)
      # }
      total_turns =0
      total_correct_left=0
      total_correct_right=0
      
      total_turns = length(correct_turns[!is.na(correct_turns)])
      total_correct_right = sum(correct_right[!is.na(correct_right)])
      total_correct_left = sum(correct_left[!is.na(correct_left)])
      proportion_correct<- c(proportion_correct,(total_correct_left+total_correct_right)/total_turns)
      proportion_correct_left <-  c(proportion_correct_left,(total_correct_left)/length(correct_left[!is.na(correct_left)]))
      proportion_correct_right <-  c(proportion_correct_right,(total_correct_right)/length(correct_right[!is.na(correct_right)]))
      reward_proportion <- c(reward_proportion,(total_correct_left+total_correct_right)/trial)
    }
    
    
    filename = paste(rat,"_reward_proportion_",ses,".jpg",sep="")
    jpeg(filename)
    plot(reward_proportion,col='black',type='l',xlab="Trials",ylab="Proportion of rewarded trials",main=paste(rat,"_reward_proportions_ses ",ses,sep=""))
    dev.off()
    
    filename = paste(rat,"_turn_proportion_",ses,".jpg",sep="")
    jpeg(filename)
    plot(proportion_correct,col='red',type='l',xlab="Trials",ylab="Proportion of correct turns at C",main=paste(rat,"_turn_proportions_,ses ",ses,sep=""))
    lines(proportion_correct_left,col='blue',type='l')
    lines(proportion_correct_right,col='green',type='l')
    legend("topright", legend=c("Proportion of all correct turns at C", "Proportion of correct left turns at C","Proportion of correct right turns at C"),lty=c(1,1,1,1),col=c("red", "blue","green"),cex=0.5,bty = "n")
    dev.off()
  }
  
  
}



########################################################################
##################################################################


get.path.probabilities=function(allpaths){
  
  path_matrix <- matrix(0, nrow = 6, ncol = 6)
  colnames(path_matrix)<-c("Path1 51","Path2 51","Path3 51","Corr 51 Path","WM 51 Path","Unknown Path")
  rownames(path_matrix)<-c("Path1 49","Path2 49","Path3 49","Corr 49 Path","WM 49 Path","Unknown Path")
  
}