library(rlist)
library(data.table)
library(data.tree)
library(ggplot2)


plot.c.turn.event.by.time=function(enreg,dirpath,rat){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  
  for(ses in c(1:2,8:9,13:14)){
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])",perl=TRUE)[[1]]
    
    correct49trials <- grep("j, k, a, b, c, d, e",allpaths,perl=TRUE, value=FALSE)
    POSrewards49 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="49"),"trial"])
    remove49 <- setdiff(correct49trials,POSrewards49)
    if(length(remove49)>0){
      correct49trials <- correct49trials[! correct49trials %in% remove49]
    }
    
    
    correct51trials <- grep("f, g, a, b, c, h, i",allpaths,perl=TRUE, value=FALSE)
    POSrewards51 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51"),"trial"])
    remove51 <- setdiff(correct51trials,POSrewards51)
    if(length(remove51)>0){
      correct51trials <- correct51trials[! correct51trials %in% remove51]
    }
    
    
    incorrecttrials <- 1:length(allpaths)
    incorrecttrials <- incorrecttrials[-c(correct49trials,correct51trials)]
    allrewards <-as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51" | enreg[[ses]]$POS[,"Reward"]=="49"),"trial"])
    incorrecttrials <- setdiff(incorrecttrials,allrewards)
    
    incorrect49trials <- incorrecttrials[grep("a,.*b,.*c.*e",allpaths[incorrecttrials],value = FALSE)]
    incorrect51trials <- incorrecttrials[grep("a,.*b,.*c.*i",allpaths[incorrecttrials],value = FALSE)]
    
    
    
    filename = file.path(dirpath,paste(rat,"-spike vs time correct 49-ses-",ses,".jpg",sep=""))
    jpeg(filename,width=2000,height=1800,quality=100)
    par(mfrow=c(5,2))
    
    ### 49 correct trials
    x=round(length(correct49trials) / 2)
    for(t in c(correct49trials[1:2],correct49trials[x:(x+1)],correct49trials[(length(correct49trials)-1):(length(correct49trials))])){
      trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
      r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
      ##Is box c visited only once ? Else ignore 
      if(sum(as.numeric(r$values=="c"))==1){
        turn_time_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "d" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
        enter_time_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
        time_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49" & enreg[[ses]]$POS[,"trial"] == t),1])
        if(is.na(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),1])[1])){
          exit_time_at_e  = tail(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),1]),1)
        }else{
          exit_time_at_e  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),1])[1]
        }
        
        times<-which((as.numeric(enreg[[ses]]$SPIKES[,1]) >= enter_time_at_a & as.numeric(enreg[[ses]]$SPIKES[,1]) < exit_time_at_e) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
        breaks=round((exit_time_at_e-enter_time_at_a)/300)
        par(xpd=F)
        hist(as.numeric(enreg[[ses]]$SPIKES[times,1]),breaks=breaks,main=paste("Trial-",t),xlab="time",ylab="Spikes/300 ms")
        abline(v = turn_time_at_c, col="green", lwd=1, lty=2)
        abline(v = time_reward, col="red", lwd=1, lty=2)
      }
      
    }
    dev.off()
    
    
   
    ### 51 correct trials
    filename = file.path(dirpath,paste(rat,"-spike vs time correct 51-ses-",ses,".jpg",sep=""))
    jpeg(filename,width=2000,height=1800,quality=100)
    par(mfrow=c(5,2))
    
    x=round(length(correct51trials) / 2)
    for(t in c(correct51trials[1:2],correct51trials[x:(x+1)],correct51trials[(length(correct51trials)-1):(length(correct51trials))])){
      trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
      r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
      ##Is box c visited only once ? Else ignore 
      if(sum(as.numeric(r$values=="c"))==1){
        turn_time_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "h" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
        enter_time_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
        time_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "51" & enreg[[ses]]$POS[,"trial"] == t),1])
        if(is.na(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),1])[1])){
          exit_time_at_i  = tail(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),1]),1)
        }else{
          exit_time_at_i  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),1])[1]
        }
        
        times<-which((as.numeric(enreg[[ses]]$SPIKES[,1]) >= enter_time_at_a & as.numeric(enreg[[ses]]$SPIKES[,1]) < exit_time_at_i) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
        breaks=round((exit_time_at_i-enter_time_at_a)/300)
        par(xpd=F)
        hist(as.numeric(enreg[[ses]]$SPIKES[times,1]),breaks=breaks,main=paste("Trial-",t),xlab="time",ylab="Spikes/300 ms")
        abline(v = turn_time_at_c, col="green", lwd=2, lty=2)
        abline(v = time_reward, col="red", lwd=2, lty=2)
      }
    }
    dev.off()
  }
  

}


plot.c.turn.event.by.distance=function(enreg,dirpath,rat){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  

  for(ses in c(1:2,8:9,13:14)){
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])",perl=TRUE)[[1]]
    
    correct49trials <- grep("j, k, a, b, c, d, e",allpaths,perl=TRUE, value=FALSE)
    POSrewards49 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="49"),"trial"])
    remove49 <- setdiff(correct49trials,POSrewards49)
    if(length(remove49)>0){
      correct49trials <- correct49trials[! correct49trials %in% remove49]
    }
    
    
    correct51trials <- grep("f, g, a, b, c, h, i",allpaths,perl=TRUE, value=FALSE)
    POSrewards51 <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51"),"trial"])
    remove51 <- setdiff(correct51trials,POSrewards51)
    if(length(remove51)>0){
      correct51trials <- correct51trials[! correct51trials %in% remove51]
    }
    
    
    incorrecttrials <- 1:length(allpaths)
    incorrecttrials <- incorrecttrials[-c(correct49trials,correct51trials)]
    allrewards <-as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]=="51" | enreg[[ses]]$POS[,"Reward"]=="49"),"trial"])
    incorrecttrials <- setdiff(incorrecttrials,allrewards)
    
    incorrect49trials <- incorrecttrials[grep("a,.*b,.*c.*e",allpaths[incorrecttrials],value = FALSE)]
    incorrect51trials <- incorrecttrials[grep("a,.*b,.*c.*i",allpaths[incorrecttrials],value = FALSE)]
    
    
    
    filename = file.path(dirpath,paste(rat,"-spike vs distance correct 49-ses-",ses,".jpg",sep=""))
    jpeg(filename,width=2000,height=1800,quality=100)
    par(mfrow=c(5,2))
    
    ### 49 correct trials
    x=round(length(correct49trials) / 2)
    
      
    for(t in c(correct49trials[1:2],correct49trials[x:(x+1)],correct49trials[(length(correct49trials)-1):(length(correct49trials))])){
      trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
      r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
      ##Is box c visited only once ? Else ignore 
      if(sum(as.numeric(r$values=="c"))==1){
        turn_distance_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "d" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
        enter_distance_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
        distance_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49" & enreg[[ses]]$POS[,"trial"] == t),"distance"])
        if(is.na(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1])){
          exit_distance_at_e  = tail(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),"distance"]),1)
        }else{
          exit_distance_at_e  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1]
        }
        distances<-which((as.numeric(enreg[[ses]]$SPIKES[,"distance"]) >= enter_distance_at_a & as.numeric(enreg[[ses]]$SPIKES[,"distance"]) < exit_distance_at_e) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
        breaks=round((exit_distance_at_e-enter_distance_at_a)/10)
        par(xpd=F)
        hist(as.numeric(enreg[[ses]]$SPIKES[distances,"distance"]),breaks=breaks,main=paste("Trial-",t),xlab="distance",ylab="Spikes/cm")
        abline(v = turn_distance_at_c, col="green", lwd=1, lty=2)
        abline(v = distance_reward, col="red", lwd=1, lty=2)
      }
      
    }
    dev.off()
    
    ### 51 correct trials
    filename = file.path(dirpath,paste(rat,"- spike vs distance correct 51-ses-",ses,".jpg",sep=""))
    jpeg(filename,width=2000,height=1800,quality=100)
    par(mfrow=c(5,2))
    x=round(length(correct51trials) / 2)
   
    for(t in c(correct51trials[1:2],correct51trials[x:(x+1)],correct51trials[(length(correct51trials)-1):(length(correct51trials))])){
      trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
      r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
      ##Is box c visited only once ? Else ignore 
      if(sum(as.numeric(r$values=="c"))==1){
        turn_distance_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "h" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
        enter_distance_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
        distance_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "51" & enreg[[ses]]$POS[,"trial"] == t),"distance"])
        if(is.na(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1])){
          exit_distance_at_i  = tail(as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == t),"distance"]),1)
        }else{
          exit_distance_at_i  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1]
        }
        distances<-which((as.numeric(enreg[[ses]]$SPIKES[,"distance"]) >= enter_distance_at_a & as.numeric(enreg[[ses]]$SPIKES[,"distance"]) < exit_distance_at_i) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
        breaks=round((exit_distance_at_i-enter_distance_at_a)/10)
        par(xpd=F)
        hist(as.numeric(enreg[[ses]]$SPIKES[distances,"distance"]),breaks=breaks,main=paste("Trial-",t),xlab="distance",ylab="Spikes/cm")
        abline(v = turn_distance_at_c, col="green", lwd=2, lty=2)
        abline(v = distance_reward, col="red", lwd=2, lty=2)
      }
    }
    dev.off()
    
    
    # filename = file.path(dirpath,paste(rat,"-incorrect trials-ses-",ses,".jpg",sep=""))
    # jpeg(filename,width=2000,height=1800,quality=100)
    # nrow = ((length(incorrect49trials)+length(incorrect51trials))/2)+1
    # par(mfrow=c(nrow,2))
    # ### incorrect 49 trials
    # for(t in incorrect49trials){
    #   trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
    #   r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
    #   first_trial_index = which(enreg[[ses]]$POS[,"trial"] == t)[1]
    #   
    #   final_d = max(which(r$values=="c"))
    #   jump_d = sum(r$lengths[1:final_d])+1
    #   final_d_index = first_trial_index+jump_d
    #   
    #   final_a = max(which(r$values=="a"))
    #   jump_a = sum(r$lengths[1:final_a])+1
    #   final_a_index = first_trial_index+jump_a
    #   
    #   
    #   turn_distance_at_c = as.numeric(enreg[[ses]]$POS[final_d_index,"distance"])
    #   enter_distance_at_a  = as.numeric(enreg[[ses]]$POS[final_a_index,"distance"])
    #   enter_distance_at_e = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "e" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
    #   exit_distance_at_e  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1]
    #   distances<-which((as.numeric(enreg[[ses]]$SPIKES[,"distance"]) >= enter_distance_at_a & as.numeric(enreg[[ses]]$SPIKES[,"distance"]) < exit_distance_at_e) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
    #   breaks=round((as.numeric(enreg[[ses]]$SPIKES[distances[length(distances)],"distance"])-as.numeric(enreg[[ses]]$SPIKES[distances[1],"distance"]))/100)
    #   par(xpd=F)
    #   hist(as.numeric(enreg[[ses]]$SPIKES[distances,"distance"]),breaks=breaks,main=paste("49 incorrect, Trial-",t),xlab="distance")
    #   abline(v = turn_distance_at_c, col="green", lwd=1, lty=2)
    #   abline(v = enter_distance_at_e, col="red", lwd=1, lty=2)
    #   
    # }
    # 
    # ### incorrect 51 trials
    # for(t in incorrect51trials){
    #   trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
    #   r<-rle(enreg[[ses]]$POS[trial_t,"boxname"])
    #   first_trial_index = which(enreg[[ses]]$POS[,"trial"] == t)[1]
    #   
    #   final_h = max(which(r$values=="c"))
    #   jump_h = sum(r$lengths[1:final_d])+1
    #   final_h_index = first_trial_index+jump_h
    #   
    #   final_a = max(which(r$values=="a"))
    #   jump_a = sum(r$lengths[1:final_a])+1
    #   final_a_index = first_trial_index+jump_a
    #   
    #   turn_distance_at_c = as.numeric(enreg[[ses]]$POS[final_h_index,"distance"])
    #   enter_distance_at_a  = as.numeric(enreg[[ses]]$POS[final_a_index,"distance"])
    #   enter_distance_at_i = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "i" & enreg[[ses]]$POS[,"trial"] == t),"distance"])[1]
    #   exit_distance_at_i  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1)),"distance"])[1]
    #   distances<-which((as.numeric(enreg[[ses]]$SPIKES[,"distance"]) >= enter_distance_at_a & as.numeric(enreg[[ses]]$SPIKES[,"distance"]) < exit_distance_at_i) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
    #   breaks=round((as.numeric(enreg[[ses]]$SPIKES[distances[length(distances)],"distance"])-as.numeric(enreg[[ses]]$SPIKES[distances[1],"distance"]))/100)
    #   par(xpd=F)
    #   hist(as.numeric(enreg[[ses]]$SPIKES[distances,"distance"]),breaks=breaks,main=paste("51 incorrect, Trial-",t),xlab="distance")
    #   abline(v = turn_distance_at_c, col="green", lwd=2, lty=2)
    #   abline(v = enter_distance_at_i, col="red", lwd=2, lty=2)
    # }
    # dev.off()
    
  }

}