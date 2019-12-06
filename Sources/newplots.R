plot.reward_proportion=function(enreg,rat){

  if(grepl("103", rat)){
    seslist <- c(1,3,4,25,26,27,52,53,55)
  }else if(grepl("113", rat)){
    seslist <- c(1,2,3,15,16,17)
  }
  
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      next
    }
    if(sum(enreg[[ses]]$EVENTS[,2]==49)+sum(enreg[[ses]]$EVENTS[,2]==51)==0){
      print(sprintf("No rewards in session %i",ses))
      next
    }
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[eib])(?=(, j, k,)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
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


get.procedural.errors=function(enreg,rat){
  
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
    allpaths<-strsplit(allpaths,"(?<=[eib])(?=(, j, k,)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
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