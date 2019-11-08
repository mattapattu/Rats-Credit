library(rlist)
library(data.table)
library(data.tree)
library(ggplot2)


plot.c.turn.event=function(enreg){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  
  
  r <- rle(enreg[[ses]]$POS[,"boxname"])
  allpaths <- toString(r$values)
  allpaths<-strsplit(allpaths,"(?<=[ei])",perl=TRUE)[[1]]
  correct49trials <- grep("j, k, a, b, c, d, e",allpaths,perl=TRUE, value=FALSE)
  correct51trials <- grep("f, g, a, b, c, h, i",allpaths,perl=TRUE, value=FALSE)
  
  ### 49 correct trials
  for(t in correct49trials){
    trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
    ##Is box c visited only once ? Else ignore 
    if(sum(as.numeric(r$values=="c"))==1){
      trial_t_box_c <- which(enreg[[ses]]$POS[trial_t,"boxname"]=="c")
      c_time <- as.numeric(enreg[[ses]]$POS[trial_t[trial_t_box_c],1])

      turn_time_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "d" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
      enter_time_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
      time_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "49" & enreg[[ses]]$POS[,"trial"] == t),1])
      exit_time_at_e  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1),1)])[1]
      times<-which((as.numeric(enreg[[ses]]$SPIKES[,1]) >= enter_time_at_a & as.numeric(enreg[[ses]]$SPIKES[,1]) < exit_time_at_e) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
      breaks=(times[length(times)]-times[1])/500
      par(xpd=F)
      hist(as.numeric(enreg[[ses]]$SPIKES[times,1]),breaks=breaks)
      abline(v = turn_time_at_c, col="green", lwd=1, lty=2)
      abline(v = time_reward, col="red", lwd=1, lty=2)
    }
  
  }
  
  
  ### 51 correct trials
  for(t in correct51trials){
    trial_t <-which(enreg[[ses]]$POS[,"trial"]==t)
    ##Is box c visited only once ? Else ignore 
    if(sum(as.numeric(r$values=="c"))==1){
      trial_t_box_c <- which(enreg[[ses]]$POS[trial_t,"boxname"]=="c")
      c_time <- as.numeric(enreg[[ses]]$POS[trial_t[trial_t_box_c],1])

      turn_time_at_c = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "h" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
      enter_time_at_a  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"boxname"] == "a" & enreg[[ses]]$POS[,"trial"] == t),1])[1]
      time_reward = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"] == "51" & enreg[[ses]]$POS[,"trial"] == t),1])
      exit_time_at_i  = as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"trial"] == (t+1),1)])[1]
      times<-which((as.numeric(enreg[[ses]]$SPIKES[,1]) >= enter_time_at_a & as.numeric(enreg[[ses]]$SPIKES[,1]) < exit_time_at_i) & as.numeric(enreg[[ses]]$SPIKES[,"neuron"] != 0))
      breaks=(times[length(times)]-times[1])/500
      par(xpd=F)
      hist(as.numeric(enreg[[ses]]$SPIKES[times,1]),breaks=breaks)
      abline(v = turn_time_at_c, col="green", lwd=1, lty=2)
      abline(v = time_reward, col="red", lwd=1, lty=2)
    }
    
  }
  
  
}