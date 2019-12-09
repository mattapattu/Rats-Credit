plot.task.errors=function(enreg,rat,dirpath1){

  procedural_err <-numeric()
  wm_err <- numeric()
  perseverance_err <- numeric()
  unk_err <- numeric()
  
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k,)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths<-cbind(allpaths, Rewards="0",Error="0")
  
    prev49Rewarded= FALSE
    prev51Rewarded=FALSE
    prev49WMerr = FALSE
    prev51WMerr = FALSE
    
    for(trial in 1:length(allpaths[,1])){
      
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
      }
      ### If No reward, update trial with proper error
      else{
        patternsPrEr <- "d.*c.*.*h.*i|h.*c.*.*d.*e|c.*b.*a|k.*j.*i|g.*f.*e"
        ### Check if error is procedural error
        if(grepl(patternsPrEr,allpaths[trial])){
          ### There is procedure error
          allpaths[trial,"Error"]="Procedural Err"
          prev49WMerr = FALSE
          prev51WMerr = FALSE
        }
        ### Check if error is WM 49 error or repeated WM error (Perseverative 49 Err )
        else if(grepl("j.*k.*a.*b.*c.*d.*e,",allpaths[trial]) && prev49Rewarded ){
          allpaths[trial,"Error"]="WM Err - 49"
          if(prev49WMerr){
            allpaths[trial,"Error"]="Perseverative Err - 49"
          }else{
            allpaths[trial,"Error"]="WM Err"
          }
          prev49WMerr = TRUE
          prev51WMerr = FALSE
        }
        ### Check if error is WM 51 error or repeated WM error (Perseverative 51 Err )
        else if(grepl("f.*g.*a.*b.*c.*h.*i,",allpaths[trial]) && prev51Rewarded){
          
          #### If rat re-enters same arm after gettign rewarded in previous trial, set err to WM err
          ### If rat repeats WM error, then set error to Preseverence Err
          if(prev51WMerr){
            allpaths[trial,"Error"]="Perseverative Err - 51"
          }else{
            allpaths[trial,"Error"]="WM Err - 51"
          }
          prev49WMerr = FALSE
          prev51WMerr = TRUE
          
        }else{
          allpaths[trial,"Error"]="Unknown Err"
        }
        prev49Rewarded= FALSE
        prev51Rewarded=FALSE
      }
    }
    
    

    wm_err <-c(wm_err,length(which(allpaths[,"Error"]=="WM Err - 51"|allpaths[,"Error"]=="WM Err - 49"))) 
    perseverance_err <-c(perseverance_err,length(which(allpaths[,"Error"]=="WM Err - 51"|allpaths[,"Error"]=="WM Err - 49"))) 
    procedural_err <- c(procedural_err,length(which(allpaths[,"Error"]=="Procedural Err")))
    unk_err <- c(unk_err ,length(which(allpaths[,"Error"]=="Unknown Err")))
    
    filename = file.path(dirpath1,paste(rat,'_ses_',ses,'.Rdata',sep=""))   
    save(allpaths,file=filename)
  }
  #filename = paste(rat,"_task_errors",".jpg",sep="")
  filename=file.path(dirpath1,paste(rat,"_task_errors",".jpg",sep=""))
  jpeg(filename)
  plot(procedural_err,col='black',type='l',xlab="Sessions",ylab="Nb of Errors",main=paste(rat,"_errors per session",sep=""))
  lines(wm_err,col='red')
  lines(perseverance_err,col='blue')
  lines(err_unk,col='green')
  legend("topright", legend=c("Nb of procedural errors", "Nb of WM errors","Nb of perseverance errors", "Nb of unknown errors"),lty=c(1,1,1),col=c("black", "red","blue","green"),cex=0.7,bty = "n")
  dev.off()
  print("Exiting ")
  
}


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