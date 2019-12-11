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
  
  neurons <- max(as.numeric(enreg[[ses]]$SPIKES[,"neuron"]))
  
  for(neuron in 1:neurons){
    for(trial in all_trials){
      
      ##neuron1 firing activity for each box for  trial
      
      box_a_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "a",1])
      box_b_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "b",1])
      box_c_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "c",1])
      box_d_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "d",1])
      box_e_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "e",1])
      box_f_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "f",1])
      box_g_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "g",1])
      box_h_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "h",1])
      box_i_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "i",1])
      box_j_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "j",1])
      box_k_neuron1_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "1" & k[,"boxName"]== "k",1])
      
      neuron1_0 = c(box_a_neuron1_spikes_0,box_b_neuron1_spikes_0,box_c_neuron1_spikes_0,box_d_neuron1_spikes_0,box_e_neuron1_spikes_0,box_f_neuron1_spikes_0,box_g_neuron1_spikes_0,box_h_neuron1_spikes_0,box_i_neuron1_spikes_0,box_j_neuron1_spikes_0,box_k_neuron1_spikes_0)
      
      # box_a_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "a",1])
      # box_b_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "b",1])
      # box_c_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "c",1])
      # box_d_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "d",1])
      # box_e_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "e",1])
      # box_f_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "f",1])
      # box_g_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "g",1])
      # box_h_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "h",1])
      # box_i_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "i",1])
      # box_j_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "j",1])
      # box_k_neuron2_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "2" & k[,"boxName"]== "k",1])
      # 
      # neuron2_0 = c(box_a_neuron2_spikes_0,box_b_neuron2_spikes_0,box_c_neuron2_spikes_0,box_d_neuron2_spikes_0,box_e_neuron2_spikes_0,box_f_neuron2_spikes_0,box_g_neuron2_spikes_0,box_h_neuron2_spikes_0,box_i_neuron2_spikes_0,box_j_neuron2_spikes_0,box_k_neuron2_spikes_0)
      # 
      # 
      # box_a_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "a",1])
      # box_b_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "b",1])
      # box_c_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "c",1])
      # box_d_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "d",1])
      # box_e_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "e",1])
      # box_f_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "f",1])
      # box_g_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "g",1])
      # box_h_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "h",1])
      # box_i_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "i",1])
      # box_j_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "j",1])
      # box_k_neuron3_spikes_0 = length(k[ k[,"trial"]== trial & k[,"neuron"]== "3" & k[,"boxName"]== "k",1])
      # 
      # neuron3_0 = c(box_a_neuron3_spikes_0,box_b_neuron3_spikes_0,box_c_neuron3_spikes_0,box_d_neuron3_spikes_0,box_e_neuron3_spikes_0,box_f_neuron3_spikes_0,box_g_neuron3_spikes_0,box_h_neuron3_spikes_0,box_i_neuron3_spikes_0,box_j_neuron3_spikes_0,box_k_neuron3_spikes_0)
      # 
      # count_a_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "a",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "a",][,1]),1)
      # count_b_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "b",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "b",][,1]),1)
      # count_c_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "c",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "c",][,1]),1)
      # count_d_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "d",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "d",][,1]),1)
      # count_e_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "e",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "e",][,1]),1)
      # count_f_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "f",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "f",][,1]),1)
      # count_g_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "g",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "g",][,1]),1)
      # count_h_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "h",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "h",][,1]),1)
      # count_i_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "i",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "i",][,1]),1)
      # count_j_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "j",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "j",][,1]),1)
      # count_k_0 = ifelse(is.matrix(m[ m[,"trial"]== trial & m[,"boxname"]== "k",]),length(m[ m[,"trial"]== trial & m[,"boxname"]== "k",][,1]),1)
      # 
      # x_0 = c(count_a_0,count_b_0,count_c_0,count_d_0,count_e_0,count_f_0,count_g_0,count_h_0,count_i_0,count_j_0,count_k_0)
      # x_0[x_0 == 0] <- 1
      # 
      # freq_neuron1 = freq_neuron1 + (neuron1_0*50/(x_0))
      # freq_neuron2 = freq_neuron2 + (neuron2_0*50/(x_0))
      # freq_neuron3 = freq_neuron3 + (neuron3_0*50/(x_0))
      
      
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
  
  
  
  
}


###############################33
#################### Calculate average different way

plot.average.frequency.by.boxes2=function(enreg,rat,dirpath1){
  
  #### Plot spike activity for all right correct trials (49) in a session for NEuron1, Neuron2, Neuron3, Total Activity
  #### Plot spike activity for all left correct trials (51) in a session for  NEuron1, Neuron2, Neuron3, Total Activity
  #### PLot for all wrong trials (0) in a session   
  
  #index = max(which(as.numeric(enreg[[ses]]$POS[,"trial"]) == trial))
  
  dirpath2 = file.path(dirpath1,rat)
  dir.create(dirpath2)
  
  for(ses in 1:length(enreg)){
    
    dirpath = file.path(dirpath2,paste("Session-",ses,sep=""))
    dir.create(dirpath)
    
    if(is.null(enreg[[ses]])){
      print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }
    
    
    last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    reward49_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    reward51_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
    
    neurons <- max(as.numeric(enreg[[ses]]$SPIKES[,"neuron"]))
    for(neuron in 1:neurons){
      print(sprintf("%s session %i neuron %i",rat, ses,neuron))
      #### Mat is not required, just to get image
      mat <-matrix(0, last_trial, 15)
      colnames(mat) <- c("A","B","B'","C","C'","D","E","E'","F","G","H","I","I'","J","K")
      trialIndex = 1
      ### nSpikes - store spikes in each box for every trial
      nSpikes <- matrix(0, last_trial, 15)
      ### timesinBoxes - store time spend in each box for every trial
      timesinBoxes <- matrix(0, last_trial, 15)
      
      for(t in 1:last_trial){
        
        #### Box a
        a <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "a")
        
        time_a_0 = getTimeSpentInBox(a,enreg,ses)
        #time_a_0 = as.numeric(enreg[[ses]]$POS[a[length(a)],1]) - as.numeric(enreg[[ses]]$POS[a[1],1])
        
        #### If turn right then b, else b'     
        #### If turn right then c, else c'
        if(length(grep("a,.*b,.*c.*e",allpaths[trialIndex],value = FALSE))>0){
          
          b <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "b")
          time_b_0 = getTimeSpentInBox(b,enreg,ses)
          
          b_prime <- NA
          time_b_prime <-NA
          
          
          c <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "c")
          time_c_0 = getTimeSpentInBox(c,enreg,ses)
          
          c_prime <- NA
          time_c_prime <-NA
          
        }else if(length(grep("a,.*b,.*c.*i",allpaths[trialIndex],value = FALSE))>0){
          
          b <- NA
          time_b_0 <-NA
          
          b_prime <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "b")
          time_b_prime = getTimeSpentInBox(b_prime,enreg,ses)
          
          c <- NA
          time_c_0 <-NA
          
          c_prime <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "c")
          time_c_prime = getTimeSpentInBox(c_prime,enreg,ses)
          
        }
        
        
        #### Box d
        d <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "d")
        time_d_0 = getTimeSpentInBox(d,enreg,ses)
        
        #### If reward box e, else box e'
        if(t %in% reward49_trials){
          e <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "e")
          time_e_0 = getTimeSpentInBox(e,enreg,ses)
          
          e_prime <- NA
          time_e_prime <- NA
          
        }else{
          e<- NA
          time_e_0<-NA
          
          e_prime <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "e")
          time_e_prime <- getTimeSpentInBox(e_prime,enreg,ses)
        }
        
        #### Box f
        f <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "f")
        time_f_0 = getTimeSpentInBox(f,enreg,ses)
        #### Box g
        g <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "g")
        time_g_0 = getTimeSpentInBox(g,enreg,ses)
        #### Box h
        h <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "h")
        time_h_0 = getTimeSpentInBox(h,enreg,ses)
        
        #### If reward box i, else box i'
        if(t %in% reward51_trials){
          i <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "i")
          time_i_0 = getTimeSpentInBox(i,enreg,ses)
          
          i_prime <- NA
          time_i_prime <- NA
        }else{
          i<- NA
          time_i_0<-NA
          
          i_prime <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "i")
          time_i_prime <- getTimeSpentInBox(i_prime,enreg,ses)
        }
        
        
        #### Box j
        j <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "j")
        time_j_0 = getTimeSpentInBox(j,enreg,ses)
        #### Box k
        k <- which(enreg[[ses]]$POS[,"trial"] == t & enreg[[ses]]$POS[,"boxname"]== "k")
        time_k_0 = getTimeSpentInBox(k,enreg,ses)
        
        ##### If time_a_0 not null, update nSpikes[a],mat,timesinBoxes[a]
        if(length(time_a_0) >0) {
          
          nSpikes[trialIndex,1]=length(enreg[[ses]]$SPIKES[which( enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "a"),1])
          timesinBoxes[trialIndex,1]=time_a_0
          mat[trialIndex,1]=nSpikes[trialIndex,1]*1000/time_a_0
        }
        
        ##### If right turn & time_b_0 not null, update nSpikes[b],mat,timesinBoxes[b] #### Else if left turn & time_b_prime not null, update nSpikes[b_prime],mat,timesinBoxes[b_prime]
        ##### If right turn & time_c_0 not null, update nSpikes[c],mat,timesinBoxes[c] #### Else if left turn & time_c_prime not null, update nSpikes[c_prime],mat,timesinBoxes[c_prime]
        if(length(grep("a,.*b,.*c.*e",allpaths[trialIndex],value = FALSE))>0){
          if(length(time_b_0) >0) {
            
            nSpikes[trialIndex,2]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "b",1])
            timesinBoxes[trialIndex,2]=time_b_0
            mat[trialIndex,2] = nSpikes[trialIndex,2]*1000/time_b_0
          }
          if(length(time_c_0) >0) {
            
            nSpikes[trialIndex,4]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "c",1])
            timesinBoxes[trialIndex,4]=time_c_0
            mat[trialIndex,4] =nSpikes[trialIndex,4]*1000/time_c_0
          }
        }else if(length(grep("a,.*b,.*c.*i",allpaths[trialIndex],value = FALSE))>0){
          if(length(time_b_prime) >0) {
            nSpikes[trialIndex,3]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "b",1])
            timesinBoxes[trialIndex,3]=time_b_prime
            mat[trialIndex,3] =nSpikes[trialIndex,3]*1000/time_b_prime
          }
          if(length(time_c_prime) >0) {
            nSpikes[trialIndex,5]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "c",1])
            timesinBoxes[trialIndex,5]=time_c_prime
            mat[trialIndex,5] =nSpikes[trialIndex,5]*1000/time_c_prime
          }
        }
        
        
        ##### If time_d_0 not null, update nSpikes[d],mat,timesinBoxes[d]
        if(length(time_d_0) >0) {
          nSpikes[trialIndex,6]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "d",1])
          timesinBoxes[trialIndex,6]=time_d_0
          mat[trialIndex,6] =nSpikes[trialIndex,6]*1000/time_d_0
        }
        
        ##### If reward 49 trial & time_e_0 not null, update nSpikes[e],mat,timesinBoxes[e] #### Else  & time_e_prime not null, update nSpikes[e_prime],mat,timesinBoxes[e_prime]
        if(t %in% reward49_trials){
          if(length(time_e_0) >0) {
            nSpikes[trialIndex,7]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "e",1])
            timesinBoxes[trialIndex,7]=time_e_0
            mat[trialIndex,7] =nSpikes[trialIndex,7]*1000/time_e_0
          }
        }else{
          if(length(time_e_prime) >0) {
            nSpikes[trialIndex,8]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "e",1])
            timesinBoxes[trialIndex,8]=time_e_prime
            mat[trialIndex,8] =nSpikes[trialIndex,8]*1000/time_e_prime
          }
        }
        ##### If time_f_0 not null, update nSpikes[f],mat,timesinBoxes[f]
        if(length(time_f_0) >0) {
          nSpikes[trialIndex,9]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "f",1])
          timesinBoxes[trialIndex,9]=time_f_0
          mat[trialIndex,9] =nSpikes[trialIndex,9]*1000/time_f_0
        }
        ##### If time_g_0 not null, update nSpikes[g],mat,timesinBoxes[g]
        if(length(time_g_0) >0) {
          nSpikes[trialIndex,10]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "g",1])
          timesinBoxes[trialIndex,10]=time_g_0
          mat[trialIndex,10] =nSpikes[trialIndex,10]*1000/time_g_0
        }
        ##### If time_h_0 not null, update nSpikes[h],mat,timesinBoxes[h]
        if(length(time_h_0) >0) {
          nSpikes[trialIndex,11]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "h",1])
          timesinBoxes[trialIndex,11]=time_h_0
          mat[trialIndex,11] =nSpikes[trialIndex,11]*1000/time_h_0
        }
        ##### If reward 51 trial & time_i_0 not null, update nSpikes[i],mat,timesinBoxes[i] #### Else  & time_i_prime not null, update nSpikes[i_prime],mat,timesinBoxes[i_prime]
        if(t %in% reward51_trials) {
          if(length(time_i_0) >0) {
            nSpikes[trialIndex,12]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "i",1])
            timesinBoxes[trialIndex,12]=time_i_0
            mat[trialIndex,12] =nSpikes[trialIndex,12]*1000/time_i_0
          }
        }else{
          if(length(time_i_prime) >0) {
            nSpikes[trialIndex,13]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "i",1])
            timesinBoxes[trialIndex,13]=time_i_prime
            mat[trialIndex,13] =nSpikes[trialIndex,13]*1000/time_i_prime
          }
        }
        ##### If time_j_0 not null, update nSpikes[j],mat,timesinBoxes[j]
        if(length(time_j_0) >0) {
          nSpikes[trialIndex,14]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "j",1])
          timesinBoxes[trialIndex,14]=time_j_0
          mat[trialIndex,14] =nSpikes[trialIndex,14]*1000/time_j_0
        }
        ##### If time_k_0 not null, update nSpikes[k],mat,timesinBoxes[k]
        if(length(time_k_0) >0) {
          nSpikes[trialIndex,15]=length(enreg[[ses]]$SPIKES[ enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron & enreg[[ses]]$SPIKES[,"boxName"]== "k",1])
          timesinBoxes[trialIndex,15]=time_k_0
          mat[trialIndex,15] =nSpikes[trialIndex,15]*1000/time_k_0
        }
        
        trialIndex = trialIndex+1
      }
      
      freq_box_a = sum(nSpikes[,1])/sum(timesinBoxes[,1])
      freq_box_b = sum(nSpikes[,2])/sum(timesinBoxes[,2])
      freq_box_b_prime = sum(nSpikes[,3])/sum(timesinBoxes[,3])
      freq_box_c = sum(nSpikes[,4])/sum(timesinBoxes[,4])
      freq_box_c_prime = sum(nSpikes[,5])/sum(timesinBoxes[,5])
      freq_box_d = sum(nSpikes[,6])/sum(timesinBoxes[,6])
      freq_box_e = sum(nSpikes[,7])/sum(timesinBoxes[,7])
      freq_box_e_prime = sum(nSpikes[,8])/sum(timesinBoxes[,8])
      freq_box_f = sum(nSpikes[,9])/sum(timesinBoxes[,9])
      freq_box_g = sum(nSpikes[,10])/sum(timesinBoxes[,10])
      freq_box_h = sum(nSpikes[,11])/sum(timesinBoxes[,11])
      freq_box_i = sum(nSpikes[,12])/sum(timesinBoxes[,12])
      freq_box_i_prime = sum(nSpikes[,13])/sum(timesinBoxes[,13])
      freq_box_j = sum(nSpikes[,14])/sum(timesinBoxes[,14])
      freq_box_k = sum(nSpikes[,15])/sum(timesinBoxes[,15])
      
      # print(sprintf("final neuron frequencies unrewarded_trials:"))
      # print(freq_neuron1)
      # print(freq_neuron2)
      # print(freq_neuron3)
      
      rownames <- c("A","B","B'","C","C'","D","H","E","E'","I","I'","F","J","K","G")
      freq_neuron <- c(freq_box_a,freq_box_b,freq_box_b_prime,freq_box_c,freq_box_c_prime,freq_box_d,freq_box_h,freq_box_e,freq_box_e_prime,freq_box_i,freq_box_i_prime,freq_box_f,freq_box_j,freq_box_k,freq_box_g)
      freq_neuron  = freq_neuron*1000
      
      filename=file.path(dirpath,paste(rat,"_box_frequency_neuron_",neuron,"_session_",ses,".jpg",sep=""))
      jpeg(filename)
      barplot(freq_neuron,main=paste("Box_frequency_Neuron_",neuron,"_session_",ses),names=rownames, ylab = "Hz")
      dev.off()
      
    }  

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
  if(length(spiketrain_neuron1) !=0) hist(spiketrain_neuron1,main=paste("Neuron 1 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Spike Freq")
  if(length(spiketrain_neuron2) !=0) hist(spiketrain_neuron2,main=paste("Neuron 2 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Spike Freq")
  if(length(spiketrain_neuron3) !=0) hist(spiketrain_neuron3,main=paste("Neuron 3 spike freq vs time,ses ",ses),breaks=1000,xlab = "Time ms", ylab = "Spike Freq")
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
  if(length(spiketrain_neuron1) !=0) hist(spiketrain_neuron1,main=paste("Neuron 1 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Spike Freq")
  if(length(spiketrain_neuron2) !=0) hist(spiketrain_neuron2,main=paste("Neuron 2 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Spike Freq")
  if(length(spiketrain_neuron3) !=0) hist(spiketrain_neuron3,main=paste("Neuron 3 spike freq vs distance,ses ",ses),breaks=1000,xlab = "Distance cm", ylab = "Spike Freq")
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

