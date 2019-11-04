library(rlist)
library(data.table)
library(data.tree)
library(ggplot2)


plot.events.by.session=function(rat,enreg,ses){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  
  reward_times <- enreg[[ses]]$EVENTS[which(enreg[[ses]]$EVENTS[,2]==49|enreg[[ses]]$EVENTS[,2]==51),1]
  ### spikes 1sec before reward
  
  spike_freq <- list()
  for(t in reward_times){
    start_time = reward_times-1000
    end_time = reward_times+1000
    spike_times <- enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,1]>= start_time & enreg[[ses]]$SPIKES[,1] <= end_time),1]
    
  }
  
}