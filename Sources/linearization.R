library(rlist)
library(data.table)
library(data.tree)
library(ggplot2)



##### Function to plot average number of spike in every box after each session
plot.spikes.by.boxes.by.session=function(rat,enreg,ses){
  
  #### Plot spike activity for all right correct trials (49) in a session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Plot spike activity for all left correct trials (51) in a session for  NEuron1, Neuron2, Neuron3, Total Activity
  #### PLot for all wrong trials (0) in a session   
  
    print(sprintf("Rat = %s , Session = %i",rat,ses))
    #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  
    m <- enreg[[ses]]$POS
    k <- enreg[[ses]]$SPIKES
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
    # 
    # neuron1_49 = 0
    # neuron2_49 = 0
    # neuron3_49 = 0
    
    # for(trial in right_corr_trials){
    #   
    #   ##neuron1 firing activity for each box for each 49 reward trial
    #   neuron1_49 = neuron1_49+ length(k[ k[,"trial"]== trial & k[,"neuron"]== "1",])
    #   neuron2_49 = neuron2_49+ length(k[ k[,"trial"]== trial & k[,"neuron"]== "2",])
    #   neuron3_49 = neuron3_49+ length(k[ k[,"trial"]== trial & k[,"neuron"]== "3",])
    #   
    #   
    #   count_a_49 = count_a_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "a",][,1]),1)
    #   count_b_49 = count_b_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "b",][,1]),1)
    #   count_c_49 = count_c_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "c",][,1]),1)
    #   count_d_49 = count_d_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "d",][,1]),1)
    #   count_e_49 = count_e_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "e",][,1]),1)
    #   count_f_49 = count_f_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "f",][,1]),1)
    #   count_g_49 = count_g_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "g",][,1]),1)
    #   count_h_49 = count_h_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "h",][,1]),1)
    #   count_i_49 = count_i_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "i",][,1]),1)
    #   count_j_49 = count_j_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "j",][,1]),1)
    #   count_k_49 = count_k_49 + ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "k",][,1]),1)
    #   
    # }
    
     box_a_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "a",])
     box_b_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "b",])
     box_c_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "c",])
     box_d_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "d",])
     box_e_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "e",])
     box_f_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "f",])
     box_g_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "g",])
     box_h_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "h",])
     box_i_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "i",])
     box_j_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "j",])
     box_k_neuron1_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "k",])
     
     neuron1_49 = c(box_a_neuron1_spikes_49,box_b_neuron1_spikes_49,box_c_neuron1_spikes_49,box_d_neuron1_spikes_49,box_e_neuron1_spikes_49,box_f_neuron1_spikes_49,box_g_neuron1_spikes_49,box_h_neuron1_spikes_49,box_i_neuron1_spikes_49,box_j_neuron1_spikes_49,box_k_neuron1_spikes_49)
     
     box_a_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "a",])
     box_b_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "b",])
     box_c_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "c",])
     box_d_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "d",])
     box_e_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "e",])
     box_f_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "f",])
     box_g_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "g",])
     box_h_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "h",])
     box_i_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "i",])
     box_j_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "j",])
     box_k_neuron2_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "k",])
     
     neuron2_49 = c(box_a_neuron2_spikes_49,box_b_neuron2_spikes_49,box_c_neuron2_spikes_49,box_d_neuron2_spikes_49,box_e_neuron2_spikes_49,box_f_neuron2_spikes_49,box_g_neuron2_spikes_49,box_h_neuron2_spikes_49,box_i_neuron2_spikes_49,box_j_neuron2_spikes_49,box_k_neuron2_spikes_49)
     
     
     box_a_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "a",])
     box_b_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "b",])
     box_c_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "c",])
     box_d_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "d",])
     box_e_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "e",])
     box_f_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "f",])
     box_g_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "g",])
     box_h_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "h",])
     box_i_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "i",])
     box_j_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "j",])
     box_k_neuron3_spikes_49 = length(k[ k[,"trial"]== right_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "k",])
     
     neuron3_49 = c(box_a_neuron3_spikes_49,box_b_neuron3_spikes_49,box_c_neuron3_spikes_49,box_d_neuron3_spikes_49,box_e_neuron3_spikes_49,box_f_neuron3_spikes_49,box_g_neuron3_spikes_49,box_h_neuron3_spikes_49,box_i_neuron3_spikes_49,box_j_neuron3_spikes_49,box_k_neuron3_spikes_49)
     
     count_a_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "a",][,1]),1)
     count_b_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "b",][,1]),1)
     count_c_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "c",][,1]),1)
     count_d_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "d",][,1]),1)
     count_e_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "e",][,1]),1)
     count_f_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "f",][,1]),1)
     count_g_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "g",][,1]),1)
     count_h_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "h",][,1]),1)
     count_i_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "i",][,1]),1)
     count_j_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "j",][,1]),1)
     count_k_49 = ifelse(is.matrix(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== right_corr_trials & m[,"boxname"]== "k",][,1]),1)
     
     
    
     x <- c(count_a_49,count_b_49,count_c_49,count_d_49,count_e_49,count_f_49,count_g_49,count_h_49,count_i_49,count_j_49,count_k_49)
     ### Prevent division by 0
     x[x == 0] <- 1

    
    neuron1_49 = neuron1_49/x
    neuron2_49 = neuron2_49/x
    neuron3_49 = neuron3_49/x
    total_49 =  (neuron1_49+neuron2_49+neuron3_49)/3
    
    filename = paste(rat,"_reward_49_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    barplot(neuron1_49,main=paste("Neuron 1 event 49,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron2_49,main=paste("Neuron 2 event 49,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron3_49,main=paste("Neuron 3 event 49,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(total_49,main=paste("Total spikes event 49,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    dev.off()
    
    
    ######### For Left trials,reward 51
    
    left_corr_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    left_corr_trials = left_corr_trials[!is.na(left_corr_trials)]
    
    box_a_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "a",])
    box_b_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "b",])
    box_c_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "c",])
    box_d_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "d",])
    box_e_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "e",])
    box_f_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "f",])
    box_g_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "g",])
    box_h_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "h",])
    box_i_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "i",])
    box_j_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "j",])
    box_k_neuron1_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "1" & k[,"boxName"]== "k",])
    
    neuron1_51 = c(box_a_neuron1_spikes_51,box_b_neuron1_spikes_51,box_c_neuron1_spikes_51,box_d_neuron1_spikes_51,box_e_neuron1_spikes_51,box_f_neuron1_spikes_51,box_g_neuron1_spikes_51,box_h_neuron1_spikes_51,box_i_neuron1_spikes_51,box_j_neuron1_spikes_51,box_k_neuron1_spikes_51)
    
    box_a_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "a",])
    box_b_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "b",])
    box_c_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "c",])
    box_d_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "d",])
    box_e_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "e",])
    box_f_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "f",])
    box_g_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "g",])
    box_h_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "h",])
    box_i_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "i",])
    box_j_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "j",])
    box_k_neuron2_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "2" & k[,"boxName"]== "k",])
    
    neuron2_51 = c(box_a_neuron2_spikes_51,box_b_neuron2_spikes_51,box_c_neuron2_spikes_51,box_d_neuron2_spikes_51,box_e_neuron2_spikes_51,box_f_neuron2_spikes_51,box_g_neuron2_spikes_51,box_h_neuron2_spikes_51,box_i_neuron2_spikes_51,box_j_neuron2_spikes_51,box_k_neuron2_spikes_51)
    
    
    box_a_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "a",])
    box_b_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "b",])
    box_c_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "c",])
    box_d_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "d",])
    box_e_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "e",])
    box_f_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "f",])
    box_g_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "g",])
    box_h_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "h",])
    box_i_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "i",])
    box_j_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "j",])
    box_k_neuron3_spikes_51 = length(k[ k[,"trial"]== left_corr_trials & k[,"neuron"]== "3" & k[,"boxName"]== "k",])
    
    neuron3_51 = c(box_a_neuron3_spikes_51,box_b_neuron3_spikes_51,box_c_neuron3_spikes_51,box_d_neuron3_spikes_51,box_e_neuron3_spikes_51,box_f_neuron3_spikes_51,box_g_neuron3_spikes_51,box_h_neuron3_spikes_51,box_i_neuron3_spikes_51,box_j_neuron3_spikes_51,box_k_neuron3_spikes_51)
    
    count_a_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "a",][,1]),1)
    count_b_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "b",][,1]),1)
    count_c_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "c",][,1]),1)
    count_d_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "d",][,1]),1)
    count_e_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "e",][,1]),1)
    count_f_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "f",][,1]),1)
    count_g_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "g",][,1]),1)
    count_h_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "h",][,1]),1)
    count_i_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "i",][,1]),1)
    count_j_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "j",][,1]),1)
    count_k_51 = ifelse(is.matrix(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== left_corr_trials & m[,"boxname"]== "k",][,1]),1)
    
    x_51 <- c(count_a_51,count_b_51,count_c_51,count_d_51,count_e_51,count_f_51,count_g_51,count_h_51,count_i_51,count_j_51,count_k_51)
    x_51[x_51 == 0] <- 1
    neuron1_51 = neuron1_51/x_51
    neuron2_51 = neuron2_51/x_51
    neuron3_51 = neuron3_51/x_51
    total_51 =  (neuron1_51+neuron2_51+neuron3_51)/3
    
    filename = paste(rat,"_reward_51_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    barplot(neuron1_51,main=paste("Neuron 1 event 51,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron2_51,main=paste("Neuron 2 event 51,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron3_51,main=paste("Neuron 3 event 51,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(total_51,main=paste("Total spikes event 51,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    dev.off()
    
    
     
    ######## For Unrewarded trials
    
    last_valid_trial_index = max(which(enreg[[ses]]$POS[,"boxname"] != "noBox"))
    last_vald_trial = as.numeric(enreg[[ses]]$POS[last_valid_trial_index,"trial"])
    all_trials <- 1:last_vald_trial
    unrewarded_trials <-  all_trials[!all_trials %in% left_corr_trials]
    unrewarded_trials <-  unrewarded_trials[!unrewarded_trials %in% right_corr_trials]
    unrewarded_trials = unrewarded_trials[!is.na(unrewarded_trials)]
    
     box_a_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "a",])
    box_b_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "b",])
    box_c_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "c",])
    box_d_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "d",])
    box_e_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "e",])
    box_f_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "f",])
    box_g_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "g",])
    box_h_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "h",])
    box_i_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "i",])
    box_j_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "j",])
    box_k_neuron1_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "1" & k[,"boxName"]== "k",])
    
    neuron1_0 = c(box_a_neuron1_spikes_0,box_b_neuron1_spikes_0,box_c_neuron1_spikes_0,box_d_neuron1_spikes_0,box_e_neuron1_spikes_0,box_f_neuron1_spikes_0,box_g_neuron1_spikes_0,box_h_neuron1_spikes_0,box_i_neuron1_spikes_0,box_j_neuron1_spikes_0,box_k_neuron1_spikes_0)
    
    box_a_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "a",])
    box_b_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "b",])
    box_c_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "c",])
    box_d_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "d",])
    box_e_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "e",])
    box_f_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "f",])
    box_g_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "g",])
    box_h_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "h",])
    box_i_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "i",])
    box_j_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "j",])
    box_k_neuron2_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "2" & k[,"boxName"]== "k",])
    
    neuron2_0 = c(box_a_neuron2_spikes_0,box_b_neuron2_spikes_0,box_c_neuron2_spikes_0,box_d_neuron2_spikes_0,box_e_neuron2_spikes_0,box_f_neuron2_spikes_0,box_g_neuron2_spikes_0,box_h_neuron2_spikes_0,box_i_neuron2_spikes_0,box_j_neuron2_spikes_0,box_k_neuron2_spikes_0)
    
    
    box_a_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "a",])
    box_b_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "b",])
    box_c_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "c",])
    box_d_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "d",])
    box_e_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "e",])
    box_f_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "f",])
    box_g_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "g",])
    box_h_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "h",])
    box_i_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "i",])
    box_j_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "j",])
    box_k_neuron3_spikes_0 = length(k[ k[,"trial"]== unrewarded_trials & k[,"neuron"]== "3" & k[,"boxName"]== "k",])
    
    neuron3_0 = c(box_a_neuron3_spikes_0,box_b_neuron3_spikes_0,box_c_neuron3_spikes_0,box_d_neuron3_spikes_0,box_e_neuron3_spikes_0,box_f_neuron3_spikes_0,box_g_neuron3_spikes_0,box_h_neuron3_spikes_0,box_i_neuron3_spikes_0,box_j_neuron3_spikes_0,box_k_neuron3_spikes_0)
    
    count_a_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "a",][,1]),1)
    count_b_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "b",][,1]),1)
    count_c_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "c",][,1]),1)
    count_d_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "d",][,1]),1)
    count_e_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "e",][,1]),1)
    count_f_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "f",][,1]),1)
    count_g_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "g",][,1]),1)
    count_h_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "h",][,1]),1)
    count_i_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "i",][,1]),1)
    count_j_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "j",][,1]),1)
    count_k_0 = ifelse(is.matrix(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== unrewarded_trials & m[,"boxname"]== "k",][,1]),1)
    
    
    x_0 <- c(count_a_0,count_b_0,count_c_0,count_d_0,count_e_0,count_f_0,count_g_0,count_h_0,count_i_0,count_j_0,count_k_0)
    x_0[x_0 == 0] <- 1
    
    neuron1 = neuron1_0/x_0
    neuron2 = neuron2_0/x_0
    neuron3 = neuron3_0/x_0
    total =  (neuron1+neuron2+neuron3)/3
    
    filename = paste(rat,"_no_reward_session",ses,".jpg",sep="")
    jpeg(filename)
    par(mfrow=c(2,2))
    barplot(neuron1,main=paste("Neuron 1 no rewards,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron2,main=paste("Neuron 2 no rewards,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(neuron3,main=paste("Neuron 3 no rewards,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    barplot(total,main=paste("Total spikes when no rewards,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"))
    dev.off()

  
}






plot.average.frequency.by.boxes=function(rat,enreg,ses){
  
  #### Plot spike activity for all right correct trials (49) in a session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Plot spike activity for all left correct trials (51) in a session for  NEuron1, Neuron2, Neuron3, Total Activity
  #### PLot for all wrong trials (0) in a session   
  
  print(sprintf("Rat = %s , Session = %i",rat,ses))
  #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  
  m <- enreg[[ses]]$POS
  k <- enreg[[ses]]$SPIKES
  
  ### For All trials
  
  last_valid_trial_index = max(which(enreg[[ses]]$POS[,"boxname"] != "noBox"))
  last_vald_trial = as.numeric(enreg[[ses]]$POS[last_valid_trial_index,"trial"])
  all_trials <- 1:last_vald_trial
  
  
  count_a_0 = 0
  count_b_0 = 0
  count_c_0 = 0
  count_d_0 = 0
  count_e_0 = 0
  count_f_0 = 0
  count_g_0 = 0
  count_h_0 = 0
  count_i_0 = 0
  count_j_0 = 0
  count_k_0 = 0
  
  freq_neuron1 = 0
  freq_neuron2 = 0
  freq_neuron3 = 0
  
  for(trial in all_trials){
    
    ##neuron1 firing activity for each box for  trial
    
    box_a_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "a",])
    box_b_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "b",])
    box_c_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "c",])
    box_d_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "d",])
    box_e_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "e",])
    box_f_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "f",])
    box_g_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "g",])
    box_h_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "h",])
    box_i_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "i",])
    box_j_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "j",])
    box_k_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "k",])
    
    neuron1_0 = c(box_a_neuron1_spikes_0,box_b_neuron1_spikes_0,box_c_neuron1_spikes_0,box_d_neuron1_spikes_0,box_e_neuron1_spikes_0,box_f_neuron1_spikes_0,box_g_neuron1_spikes_0,box_h_neuron1_spikes_0,box_i_neuron1_spikes_0,box_j_neuron1_spikes_0,box_k_neuron1_spikes_0)
    
    box_a_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "a",])
    box_b_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "b",])
    box_c_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "c",])
    box_d_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "d",])
    box_e_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "e",])
    box_f_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "f",])
    box_g_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "g",])
    box_h_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "h",])
    box_i_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "i",])
    box_j_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "j",])
    box_k_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "k",])
    
    neuron2_0 = c(box_a_neuron2_spikes_0,box_b_neuron2_spikes_0,box_c_neuron2_spikes_0,box_d_neuron2_spikes_0,box_e_neuron2_spikes_0,box_f_neuron2_spikes_0,box_g_neuron2_spikes_0,box_h_neuron2_spikes_0,box_i_neuron2_spikes_0,box_j_neuron2_spikes_0,box_k_neuron2_spikes_0)
    
    
    box_a_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "a",])
    box_b_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "b",])
    box_c_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "c",])
    box_d_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "d",])
    box_e_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "e",])
    box_f_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "f",])
    box_g_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "g",])
    box_h_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "h",])
    box_i_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "i",])
    box_j_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "j",])
    box_k_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "k",])
    
    neuron3_0 = c(box_a_neuron3_spikes_0,box_b_neuron3_spikes_0,box_c_neuron3_spikes_0,box_d_neuron3_spikes_0,box_e_neuron3_spikes_0,box_f_neuron3_spikes_0,box_g_neuron3_spikes_0,box_h_neuron3_spikes_0,box_i_neuron3_spikes_0,box_j_neuron3_spikes_0,box_k_neuron3_spikes_0)
    
    count_a_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "a",][,1]),1)
    count_b_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "b",][,1]),1)
    count_c_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "c",][,1]),1)
    count_d_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "d",][,1]),1)
    count_e_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "e",][,1]),1)
    count_f_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "f",][,1]),1)
    count_g_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "g",][,1]),1)
    count_h_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "h",][,1]),1)
    count_i_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "i",][,1]),1)
    count_j_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "j",][,1]),1)
    count_k_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "k",][,1]),1)
    
    x_0 = c(count_a_0,count_b_0,count_c_0,count_d_0,count_e_0,count_f_0,count_g_0,count_h_0,count_i_0,count_j_0,count_k_0)
    x_0[x_0 == 0] <- 1
    
    freq_neuron1 = freq_neuron1 + (neuron1_0*50/(x_0))
    freq_neuron2 = freq_neuron2 + (neuron2_0*50/(x_0))
    freq_neuron3 = freq_neuron3 + (neuron3_0*50/(x_0))
    
    
    # r = rle(m[ m[,"trial"]== trial,"boxname" ])
    # cat(sprintf('trial %i , path: %s',trial,r$values))
    # cat(sprintf('Trial %i, neuron spikes unrewarded_trials:',trial))
    
    # print(neuron1_0)
    # print(neuron2_0)
    # print(neuron3_0)
    # 
    # print(sprintf("Trial %i, neuron frequencies unrewarded_trials:",trial))
    # 
    # 
    # print(freq_neuron1)
    # print(freq_neuron2)
    # print(freq_neuron3)
  }
  
  
  
  freq_neuron1 = freq_neuron1/last_vald_trial
  freq_neuron2 = freq_neuron2/last_vald_trial
  freq_neuron3 = freq_neuron3/last_vald_trial
  total = freq_neuron1+freq_neuron2+freq_neuron3
  
  # print(sprintf("final neuron frequencies unrewarded_trials:"))
  # print(freq_neuron1)
  # print(freq_neuron2)
  # print(freq_neuron3)
  
  filename = paste(rat,"_box_frequency_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  barplot(freq_neuron1,main=paste("Neuron 1 spike freq vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  barplot(freq_neuron2,main=paste("Neuron 2 spike freq vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  barplot(freq_neuron3,main=paste("Neuron 3 spike freq vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  barplot(total,main=paste("Population firing freq vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  dev.off()
  
  
}


###############################33
#################### Calculate average different way

plot.average.frequency.by.boxes2=function(rat,enreg,ses){
  
  #### Plot spike activity for all right correct trials (49) in a session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Plot spike activity for all left correct trials (51) in a session for  NEuron1, Neuron2, Neuron3, Total Activity
  #### PLot for all wrong trials (0) in a session   
  
  print(sprintf("Rat = %s , Session = %i",rat,ses))
  #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  
  pos <- enreg[[ses]]$POS
  spik <- enreg[[ses]]$SPIKES
  
  ### For All trials
  
  last_valid_trial_index = max(which(enreg[[ses]]$POS[,"boxname"] != "noBox"))
  last_vald_trial = as.numeric(enreg[[ses]]$POS[last_valid_trial_index,"trial"])
  all_trials <- 1:last_vald_trial
  
  
  count_a_0 = 0
  count_b_0 = 0
  count_c_0 = 0
  count_d_0 = 0
  count_e_0 = 0
  count_f_0 = 0
  count_g_0 = 0
  count_h_0 = 0
  count_i_0 = 0
  count_j_0 = 0
  count_k_0 = 0
  
  freq_neuron1 = 0
  freq_neuron2 = 0
  freq_neuron3 = 0
  
  count_skip = 0
  
  for(trial in all_trials){
    
    ##neuron1 firing activity for each box for  trial
    
    box_a_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "a",])
    box_b_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "b",])
    box_c_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "c",])
    box_d_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "d",])
    box_e_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "e",])
    box_f_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "f",])
    box_g_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "g",])
    box_h_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "h",])
    box_i_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "i",])
    box_j_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "j",])
    box_k_neuron1_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "1" & spik[,"boxName"]== "k",])
    
    neuron1_0 = c(box_a_neuron1_spikes_0,box_b_neuron1_spikes_0,box_c_neuron1_spikes_0,box_d_neuron1_spikes_0,box_e_neuron1_spikes_0,box_f_neuron1_spikes_0,box_g_neuron1_spikes_0,box_h_neuron1_spikes_0,box_i_neuron1_spikes_0,box_j_neuron1_spikes_0,box_k_neuron1_spikes_0)
    
    box_a_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "a",])
    box_b_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "b",])
    box_c_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "c",])
    box_d_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "d",])
    box_e_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "e",])
    box_f_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "f",])
    box_g_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "g",])
    box_h_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "h",])
    box_i_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "i",])
    box_j_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "j",])
    box_k_neuron2_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "2" & spik[,"boxName"]== "k",])
    
    neuron2_0 = c(box_a_neuron2_spikes_0,box_b_neuron2_spikes_0,box_c_neuron2_spikes_0,box_d_neuron2_spikes_0,box_e_neuron2_spikes_0,box_f_neuron2_spikes_0,box_g_neuron2_spikes_0,box_h_neuron2_spikes_0,box_i_neuron2_spikes_0,box_j_neuron2_spikes_0,box_k_neuron2_spikes_0)
    
    
    box_a_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "a",])
    box_b_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "b",])
    box_c_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "c",])
    box_d_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "d",])
    box_e_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "e",])
    box_f_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "f",])
    box_g_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "g",])
    box_h_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "h",])
    box_i_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "i",])
    box_j_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "j",])
    box_k_neuron3_spikes_0 = length(spik[ spik[,"trial"]== trial & spik[,"neuron"]== "3" & spik[,"boxName"]== "k",])
    
    neuron3_0 = c(box_a_neuron3_spikes_0,box_b_neuron3_spikes_0,box_c_neuron3_spikes_0,box_d_neuron3_spikes_0,box_e_neuron3_spikes_0,box_f_neuron3_spikes_0,box_g_neuron3_spikes_0,box_h_neuron3_spikes_0,box_i_neuron3_spikes_0,box_j_neuron3_spikes_0,box_k_neuron3_spikes_0)
    
    timegaps = diff(as.numeric(pos[pos[,"trial"]== trial,1]))
    
    if(length(timegaps[timegaps > 100])>0){
      count_skip = count_skip+1
      next
    }
    
    
    a <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "a")
    time_a_0 = sum(as.numeric(pos[a+1,1])-as.numeric(pos[a,1]))
    
    b <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "b")
    time_b_0 = sum(as.numeric(pos[b+1,1])-as.numeric(pos[b,1]))
    
    c <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "c")
    time_c_0 = sum(as.numeric(pos[c+1,1])-as.numeric(pos[c,1]))
    
    d <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "d")
    time_d_0 = sum(as.numeric(pos[d+1,1])-as.numeric(pos[d,1]))
    
    e <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "e")
    time_e_0 = sum(as.numeric(pos[e+1,1])-as.numeric(pos[e,1]))
    
    f <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "f")
    time_f_0 = sum(as.numeric(pos[f+1,1])-as.numeric(pos[f,1]))
    
    g <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "g")
    time_g_0 = sum(as.numeric(pos[g+1,1])-as.numeric(pos[g,1]))
    
    h <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "h")
    time_h_0 = sum(as.numeric(pos[h+1,1])-as.numeric(pos[h,1]))
    
    i <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "i")
    time_i_0 = sum(as.numeric(pos[i+1,1])-as.numeric(pos[i,1]))
    
    j <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "j")
    time_j_0 = sum(as.numeric(pos[j+1,1])-as.numeric(pos[j,1]))
    
    k <- which(pos[1:(nrow(pos)-1),"trial"]== trial & pos[1:(nrow(pos)-1),"boxname"]== "k")
    time_k_0 = sum(as.numeric(pos[k+1,1])-as.numeric(pos[k,1]))
    
    
    x_0 = c(time_a_0,time_b_0,time_c_0,time_d_0,time_e_0,time_f_0,time_g_0,time_h_0,time_i_0,time_j_0,time_k_0)
    x_0[x_0 == 0] <- 1
    
    freq_neuron1 = freq_neuron1 + (neuron1_0*1000/(x_0))
    freq_neuron2 = freq_neuron2 + (neuron2_0*1000/(x_0))
    freq_neuron3 = freq_neuron3 + (neuron3_0*1000/(x_0))
    
    
    # r = rle(m[ m[,"trial"]== trial,"boxname" ])
    # cat(sprintf('trial %i , path: %s',trial,r$values))
    # cat(sprintf('Trial %i, neuron spikes unrewarded_trials:',trial))
    
    # print(neuron1_0)
    # print(neuron2_0)
    # print(neuron3_0)
    # 
    # print(sprintf("Trial %i, neuron frequencies unrewarded_trials:",trial))
    # 
    # 
    # print(freq_neuron1)
    # print(freq_neuron2)
    # print(freq_neuron3)
  }
  
  
  
  freq_neuron1 = freq_neuron1/(last_vald_trial-count_skip)
  freq_neuron2 = freq_neuron2/(last_vald_trial-count_skip)
  freq_neuron3 = freq_neuron3/(last_vald_trial-count_skip)
  
  # print(sprintf("final neuron frequencies unrewarded_trials:"))
  # print(freq_neuron1)
  # print(freq_neuron2)
  # print(freq_neuron3)
  
  filename = paste(rat,"_box_frequency2_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  barplot(freq_neuron1,main=paste("Neuron 1 spike freq2 vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  barplot(freq_neuron2,main=paste("Neuron 2 spike freq2 vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  barplot(freq_neuron3,main=paste("Neuron 3 spike freq2 vs box,ses ",ses),names=c("a","b","c","d","e","f","g","h","i","j","k"), ylab = "Hz")
  dev.off()
  
  
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
  if(length(spiketrain_neuron1) !=0) hist(spiketrain_neuron1,main=paste("Neuron 1 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Hz")
  if(length(spiketrain_neuron2) !=0) hist(spiketrain_neuron2,main=paste("Neuron 2 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Hz")
  if(length(spiketrain_neuron3) !=0) hist(spiketrain_neuron3,main=paste("Neuron 3 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Hz")
  hist(total,main=paste("Total spikes,ses ",ses),breaks=1000,xlab = "Time")
  dev.off()


  filename = paste(rat,"spiketime_autocorr_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))

  if(length(spiketrain_neuron1) !=0) {
    spiketrain_neuron1_round = round(spiketrain_neuron1)
    spikepulses <- numeric(max(spiketrain_neuron1_round))
    a <- table(spiketrain_neuron1_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron2) !=0) {
    spiketrain_neuron2_round = round(spiketrain_neuron2)
    spikepulses <- numeric(max(spiketrain_neuron2_round))
    a <- table(spiketrain_neuron2_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron3) !=0) {
    spiketrain_neuron3_round = round(spiketrain_neuron3)
    spikepulses <- numeric(max(spiketrain_neuron3_round))
    a <- table(spiketrain_neuron3_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  
  spiketrain_neuron4_round = round(total)
  spikepulses <- numeric(max(spiketrain_neuron4_round))
  a <- table(spiketrain_neuron4_round)
  spikepulses[as.numeric(names(a))]=a[-1]
  xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
  #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
  plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Acf of total activity")
  #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")

  dev.off()

}



plot.spikes.by.distance=function(rat,enreg,ses){
  
  #### Plot average for every session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Finally plot the total average per trial
  print(sprintf("Rat = %s , Session = %i",rat,ses))
  #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  spiketrain_neuron1 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "1"),"distance"])
  spiketrain_neuron2 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "2"),"distance"])
  spiketrain_neuron3 = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]== "3"),"distance"])
  total = as.numeric(enreg[[ses]]$SPIKES[which(enreg[[ses]]$SPIKES[,"neuron"]!= "0"),"distance"])
  
  
  filename = paste(rat,"_spike_vs_distance_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  if(length(spiketrain_neuron1) !=0) hist(spiketrain_neuron1,main=paste("Neuron 1 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Hz")
  if(length(spiketrain_neuron2) !=0) hist(spiketrain_neuron2,main=paste("Neuron 2 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Hz")
  if(length(spiketrain_neuron3) !=0) hist(spiketrain_neuron3,main=paste("Neuron 3 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Hz")
  hist(total,main=paste("Total spikes,ses ",ses),breaks=1000,xlab = "Distance")
  dev.off()
  
  
  filename = paste(rat,"_spikedist_autocorr_session",ses,".jpg",sep="")
  jpeg(filename)
  par(mfrow=c(2,2))
  
  if(length(spiketrain_neuron1) !=0) {
    spiketrain_neuron1_round = round(spiketrain_neuron1)
    spikepulses <- numeric(max(spiketrain_neuron1_round))
    a <- table(spiketrain_neuron1_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron2) !=0) {
    spiketrain_neuron2_round = round(spiketrain_neuron2)
    spikepulses <- numeric(max(spiketrain_neuron2_round))
    a <- table(spiketrain_neuron2_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  if(length(spiketrain_neuron3) !=0) {
    spiketrain_neuron3_round = round(spiketrain_neuron3)
    spikepulses <- numeric(max(spiketrain_neuron3_round))
    a <- table(spiketrain_neuron3_round)
    spikepulses[as.numeric(names(a))]=a[-1]
    xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
    #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
    plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Neuron 1 spiketrain acf")
    #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  }
  
  
  spiketrain_neuron4_round = round(total)
  spikepulses <- numeric(max(spiketrain_neuron4_round))
  a <- table(spiketrain_neuron4_round)
  spikepulses[as.numeric(names(a))]=a[-1]
  xc_pulses <- acf(spikepulses, lag.max = 1000,plot = FALSE)
  #histogram <-  hist(diff(which(spikepulses==1)), breaks=xc_pulses$lag, plot=FALSE)
  plot(xc_pulses$lag, xc_pulses$acf, col="red",xlab="lag",main="Acf of total activity")
  #lines(histogram$mids, histogram$counts / max(histogram$counts), col="green")
  
  dev.off()
  
}

