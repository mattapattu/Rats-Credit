library(rlist)
library(data.table)
library(data.tree)
library(ggplot2)




plot.spikes.by.boxes.by.session=function(rat,enreg,ses){
  
  #### Plot spike activity for all right correct trials (49) in a session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Plot spike activity for all left correct trials (51) in a session for  NEuron1, Neuron2, Neuron3, Total Activity
  #### PLot for all wrong trials (0) in a session   
  
    print(sprintf("Rat = %s , Session = %i",rat,ses))
    #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  
    m <- enreg[[ses]]$POS
    #### For right trials
    right_corr_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    right_corr_trials = right_corr_trials[!is.na(right_corr_trials)]
    # count_a_49 = 0
    # count_b_49 = 0
    # count_c_49 = 0
    # count_d_49 = 0
    # count_e_49 = 0
    # count_f_49 = 0
    # count_g_49 = 0
    # count_h_49 = 0
    # count_i_49 = 0
    # count_j_49 = 0
    # count_k_49 = 0
    
    neuron1_49 = 0
    neuron2_49 = 0
    neuron3_49 = 0
    
    for(trial in right_corr_trials){
      
      ##neuron1 firing activity for each box for each 49 reward trial
      neuron1_49 = neuron1_49+enreg[[ses]]$TRIAL[trial,1,]
      neuron2_49 = neuron2_49+enreg[[ses]]$TRIAL[trial,2,]
      neuron3_49 = neuron3_49+enreg[[ses]]$TRIAL[trial,3,]
      
      
      # count_a_49 = count_a_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "a"])
      # count_b_49 = count_b_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "b"])
      # count_c_49 = count_c_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "c"])
      # count_d_49 = count_d_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "d"])
      # count_e_49 = count_e_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "e"])
      # count_f_49 = count_f_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "f"])
      # count_g_49 = count_g_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "g"])
      # count_h_49 = count_h_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "h"])
      # count_i_49 = count_i_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "i"])
      # count_j_49 = count_j_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "j"])
      # count_k_49 = count_k_49 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "k"])
      
    }
    
    
    # x <- c(count_a_49,count_b_49,count_c_49,count_d_49,count_e_49,count_f_49,count_g_49,count_h_49,count_i_49)
    
    neuron1_49 = neuron1_49/length(right_corr_trials)
    neuron2_49 = neuron2_49/length(right_corr_trials)
    neuron3_49 = neuron3_49/length(right_corr_trials)
    total_49 =  neuron1_49+neuron2_49+neuron3_49/(length(right_corr_trials))
    
    filename = paste(rat,"_reward_49_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    if(length(neuron1_49) !=0) barplot(neuron1_49,main=paste("Neuron 1 event 49,session ",ses))
    if(length(neuron2_49) !=0) barplot(neuron2_49,main=paste("Neuron 2 event 49,session ",ses))
    if(length(neuron3_49) !=0) barplot(neuron3_49,main=paste("Neuron 3 event 49,session ",ses))
    barplot(total_49,main=paste("Total spikes event 49,session ",ses))
    dev.off()
    
    
    ### For Left trials,reward 51
    
    left_corr_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    left_corr_trials = left_corr_trials[!is.na(left_corr_trials)]
    # count_a_51 = 0
    # count_b_51 = 0
    # count_c_51 = 0
    # count_d_51 = 0
    # count_e_51 = 0
    # count_f_51 = 0
    # count_g_51 = 0
    # count_h_51 = 0
    # count_i_51 = 0
    
    neuron1_51 = 0
    neuron2_51 = 0
    neuron3_51 = 0
    
    for(trial in left_corr_trials){
      # count_a_51 = count_a_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "a"])
      # count_b_51 = count_b_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "b"])
      # count_c_51 = count_c_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "c"])
      # count_d_51 = count_d_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "d"])
      # count_e_51 = count_e_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "e"])
      # count_f_51 = count_f_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "f"])
      # count_g_51 = count_g_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "g"])
      # count_h_51 = count_h_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "h"])
      # count_i_51 = count_i_51 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "i"])
       
      ##neuron1 firing activity for each box for each 51 reward trial
      neuron1_51 = neuron1_51+enreg[[ses]]$TRIAL[trial,1,]
      neuron2_51 = neuron2_51+enreg[[ses]]$TRIAL[trial,2,]
      neuron3_51 = neuron3_51+enreg[[ses]]$TRIAL[trial,3,]
    }
    
    
    # x <- c(count_a_51,count_b_51,count_c_51,count_d_51,count_e_51,count_f_51,count_g_51,count_h_51,count_i_51)
    
    neuron1_51 = neuron1_51/length(left_corr_trials)
    neuron2_51 = neuron2_51/length(left_corr_trials)
    neuron3_51 = neuron3_51/length(left_corr_trials)
    total_51 =  neuron1_51+neuron2_51+neuron3_51/length(left_corr_trials)
    
    filename = paste(rat,"_reward_51_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    if(length(neuron1_51) !=0) barplot(neuron1_51,main=paste("Neuron 1 event 51,session ",ses))
    if(length(neuron2_51) !=0) barplot(neuron2_51,main=paste("Neuron 2 event 51,session ",ses))
    if(length(neuron3_51) !=0) barplot(neuron3_51,main=paste("Neuron 3 event 51,session ",ses))
    barplot(total_51,main=paste("Total spikes event 51,session ",ses))
    dev.off()
    
    
     
    ### For Unrewarded trials
    
    last_valid_trial_index = max(which(enreg[[ses]]$POS[,"boxname"] != "noBox"))
    last_vald_trial = as.numeric(enreg[[ses]]$POS[last_valid_trial_index,"trial"])
    all_trials <- 1:last_vald_trial
    unrewarded_trials <-  all_trials[!all_trials %in% left_corr_trials]
    unrewarded_trials <-  unrewarded_trials[!unrewarded_trials %in% right_corr_trials]
    unrewarded_trials = unrewarded_trials[!is.na(unrewarded_trials)]
    
    # count_a_0 = 0
    # count_b_0 = 0
    # count_c_0 = 0
    # count_d_0 = 0
    # count_e_0 = 0
    # count_f_0 = 0
    # count_g_0 = 0
    # count_h_0 = 0
    # count_i_0 = 0
    
    neuron1 = 0
    neuron2 = 0
    neuron3 = 0
    
    for(trial in unrewarded_trials){
      # count_a_0 = count_a_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "a"])
      # count_b_0 = count_b_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "b"])
      # count_c_0 = count_c_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "c"])
      # count_d_0 = count_d_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "d"])
      # count_e_0 = count_e_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "e"])
      # count_f_0 = count_f_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "f"])
      # count_g_0 = count_g_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "g"])
      # count_h_0 = count_h_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "h"])
      # count_i_0 = count_i_0 + length(m[ m[,"trial"]== trial & m[,"boxname"]== "i"])
      
      ##neuron1 firing activity for each box for each 51 reward trial
      neuron1 = neuron1+enreg[[ses]]$TRIAL[trial,1,]
      neuron2 = neuron2+enreg[[ses]]$TRIAL[trial,2,]
      neuron3 = neuron3+enreg[[ses]]$TRIAL[trial,3,]
    }
    
    
    x <- c(count_a_0,count_b_0,count_c_0,count_d_0,count_e_0,count_f_0,count_g_0,count_h_0,count_i_0)
    
    neuron1 = neuron1/length(unrewarded_trials)
    neuron2 = neuron2/length(unrewarded_trials)
    neuron3 = neuron3/length(unrewarded_trials)
    total =  neuron1+neuron2+neuron3/length(unrewarded_trials)
    
    filename = paste(rat,"_no_reward_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    if(length(neuron1) !=0) barplot(neuron1,main=paste("Neuron 1 no rewards,session ",ses))
    if(length(neuron2) !=0) barplot(neuron2,main=paste("Neuron 2 no rewards,session ",ses))
    if(length(neuron3) !=0) barplot(neuron3,main=paste("Neuron 3 no rewards,session ",ses))
    barplot(total,main=paste("Total spikes when no rewards,session ",ses))
    dev.off()

  
}

plot.spikes.by.boxes.by.rat=function(rat,enreg){
  
  #### Plot average for every session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Finally plot the total average per trial 
  for(ses in 1:length(enreg)){
    print(sprintf("Rat = %s , Session = %i",rat,ses))
    #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
    count_a = sum((enreg[[ses]]$POS[,"boxname"]== "a")*1)
    count_b = sum((enreg[[ses]]$POS[,"boxname"]== "b")*1)
    count_c = sum((enreg[[ses]]$POS[,"boxname"]== "c")*1)
    count_d = sum((enreg[[ses]]$POS[,"boxname"]== "d")*1)
    count_e = sum((enreg[[ses]]$POS[,"boxname"]== "e")*1)
    count_f = sum((enreg[[ses]]$POS[,"boxname"]== "f")*1)
    count_g = sum((enreg[[ses]]$POS[,"boxname"]== "g")*1)
    count_h = sum((enreg[[ses]]$POS[,"boxname"]== "h")*1)
    count_i = sum((enreg[[ses]]$POS[,"boxname"]== "i")*1)
    
    x <- c(count_a,count_b,count_c,count_d,count_e,count_f,count_g,count_h,count_i)
    neuron1 = colSums(enreg[[ses]]$TRIAL[,1,])
    #neuron1 = neuron1/x
    
    neuron2 = colSums(enreg[[ses]]$TRIAL[,2,])
    #neuron2 = neuron2/x
    
    neuron3 = colSums(enreg[[ses]]$TRIAL[,3,])
    #neuron3 = neuron3/x
    
    total =  colSums(colSums(enreg[[ses]]$TRIAL[,,]))
    #total = total/x
    
    filename = paste(rat,"_barplot_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    if(length(neuron1) !=0) barplot(neuron1,main=paste("Neuron 1 spikes,session ",ses))
    if(length(neuron2) !=0) barplot(neuron2,main=paste("Neuron 2 spikes,session ",ses))
    if(length(neuron3) !=0) barplot(neuron3,main=paste("Neuron 3 spikes,session ",ses))
    if(length(total) !=0) barplot(total,main=paste("Total spikes,session ",ses))
    dev.off()
  }
  
}

plot.spikes.by.time=function(rat,enreg,ses){
  
  #### Plot average for every session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Finally plot the total average per trial 
  print(sprintf("Rat = %s , Session = %i",rat,ses))
  #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  spiketrain_neuron1 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "1"),"time"])
  spiketrain_neuron2 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "2"),"time"])
  spiketrain_neuron3 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "3"),"time"])
  total = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]!= "0"),"time"])
  
  
  filename = paste(rat,"_spike_vs_time_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  if(length(spiketrain_neuron1) !=0) hist(spiketrain_neuron1,main=paste("Neuron 1 spiketrain,session ",ses),breaks=1000,xlab = "Time")
  if(length(spiketrain_neuron2) !=0) hist(spiketrain_neuron2,main=paste("Neuron 2 spiketrain,session ",ses),breaks=1000,xlab = "Time")
  if(length(spiketrain_neuron3) !=0) hist(spiketrain_neuron3,main=paste("Neuron 3 spiketrain,session ",ses),breaks=1000,xlab = "Time")
  hist(total,main=paste("Total spikes,session ",ses),breaks=1000,xlab = "Time")
  dev.off()
  
  
  filename = paste(rat,"spike_autocorr_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  
  if(length(spiketrain_neuron1) !=0) {
    spiketrain_neuron1_round = round(spiketrain_neuron1)
    spikepulses <- seq(0,0,length.out=tail(spiketrain_neuron1_round,n=1))
    i <- 1:length(spikepulses)
    spikepulses[which(i  %in% spiketrain_neuron1_round)] = 1
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron2) !=0) {
    spiketrain_neuron2_round = round(spiketrain_neuron2)
    spikepulses <- seq(0,0,length.out=tail(spiketrain_neuron2_round,n=1))
    i <- 1:length(spikepulses)
    spikepulses[which(i  %in% spiketrain_neuron2_round)] = 1
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron3) !=0) {
    spiketrain_neuron3_round = round(spiketrain_neuron3)
    spikepulses <- seq(0,0,length.out=tail(spiketrain_neuron3_round,n=1))
    i <- 1:length(spikepulses)
    spikepulses[which(i  %in% spiketrain_neuron3_round)] = 1
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
    
  
  spiketrain_neuron4_round = round(total)
  spikepulses <- seq(0,0,length.out=tail(spiketrain_neuron4_round,n=1))
  i <- 1:length(spikepulses)
  spikepulses[which(i  %in% spiketrain_neuron4_round)] = 1
  xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
  #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
  plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Acf of total activity")
  #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")  
  
  dev.off()
  
  
  
}