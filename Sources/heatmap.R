library(ggplot2)
library(seriation)
library(igraph)

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


plot.heatmap=function(enreg,rat){
  #plot for all rewarded e/i visit trials
  #plot for all unrewarded e/i visit trials
  #plot for all rewarded e visit trials 
  #plot for all rewarded i visit trials
  
  ###spikes 1s before reward, Reward, 1s after reward
  if(grepl("103", rat)){
    seslist <- c(1,3,4,25,26,27,52,53,55)
  }else if(grepl("113", rat)){
    seslist <- c(1,2,3,15,16,17)
  }
  
  for(ses in seslist){
    
    print(sprintf("%s session %i",rat, ses))
    last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    
    reward49_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    reward51_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths <- toString(r$values)
        allpaths<-strsplit(allpaths,"(?<=[eib])(?=(, j, k,)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
    
    neurons <- max(as.numeric(enreg[[ses]]$SPIKES[,"neuron"]))
    for(neuron in 1:neurons){
      mat <-matrix(0, last_trial, 15)
      colnames(mat) <- c("A","B","B'","C","C'","D","E","E'","F","G","H","I","I'","J","K")
      trialIndex = 1
      nSpikes <- matrix(0, last_trial, 15)
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
      
      #debug(groupBoxesForChiSqTest)
      output = groupBoxesForChiSqTest(nSpikes,timesinBoxes,last_trial)
      print(output$groups)
      pvals <-numeric()
      for(i in 1:15){
        
        pvals <- c(pvals,testHomogeneity(output$newSpikes[[i]],output$newTimesinBox[[i]]))
      }
      
      #adjusted_pvals <- p.adjust(pvals, method = "bonferroni", n = length(pvals))
      
      #### initialize tree of pvalues
      
      # pval_graph <- make_empty_graph()
      # alpha_graph <- make_empty_graph()
      
      
      alpha_mat <- matrix(0,50,5)
      colnames(alpha_mat) <- c("Box/Newgroup","Alpha","pval","H0 Rej","Split Further")
      matIndex=0
      ### Check for homogeneity in each box and regroup if non-homogeneous
      final_groups <- list()
      for(i in 1:15){
        pval_alpha <- 0.05/15
        if(i==1){
          matIndex=1 
        }else{
          matIndex=max(which(alpha_mat[,1] != "0"))+1
        }
        # print(sprintf("matIndex=%s at the begining of for loop",matIndex))
        # print(alpha_mat[which(alpha_mat[1] != "0"),])
        alpha_mat[matIndex,1] =  paste("Box",i,  sep="")## Newgroup
        alpha_mat[matIndex,2] = pval_alpha ## Alpha level for H0
        alpha_mat[matIndex,3] = pvals[i] ## Pval of H0 test
        
        final_groups[[i]] <- list()
        ### If the pval for homogenity test for box i is greater than 0.05, then regroup box and test
        if(pvals[i] < pval_alpha){
          alpha_mat[matIndex,4] = "Yes"  ## H0 rejected 
          alpha_mat[matIndex,5] = "Yes"  ## Split Further
          
          ### all trials for box i are not homogeneous
          ### Combine trials by adding chisq boxes
          print(sprintf("For box %i, Adjusted pvalue = %f < pval_alpha = %f",i,pvals[i],pval_alpha))
          print(sprintf("For box %i,H0 can be rejected and box must be regrouped to find homogeneous sub-groups",i))
          #debug(regroupBoxes)
          
          #pval_graph <- pval_graph + vertices(paste("box",i,pval_alpha,sep=""))
          #alpha_graph <- alpha_graph + 
          pval_alpha = pval_alpha/2
          matIndex=matIndex+1 ## Add new row in matrix below the current box
          print(sprintf("matIndex=%s",matIndex))
          newList <- regroupBoxes(output,i,pval_alpha,alpha_mat,matIndex)
          newgroups <- newList$final_group
          alpha_mat <- newList$alpha_mat
          # print(alpha_mat)
          # print(sprintf("Returned from regroup"))
          for(j in 1:length(newgroups)){
            final_groups[[i]] <- list.append(final_groups[[i]],unlist(output$groups[[i]][min(newgroups[[j]]):max(newgroups[[j]])]))
          }
          
        }else{
          alpha_mat[matIndex,4] = "No"  ## H0 rejected 
          alpha_mat[matIndex,5] = "No"  ## Split Further
          
          print(sprintf("For box i=%i,Adjusted pvalue = %f > pval_alpha = %f",i,pvals[i],pval_alpha))
          print(sprintf("For box i=%i,H0 cannot be rejected",i))
          ### all trials for box i are homogeneous
          final_groups[[i]] <- list.append(final_groups[[i]],unlist(output$groups[[i]]))
          
        }
        #print(sprintf("matIndex=%s at the end of for loop",max(which(alpha_mat[,1] != "0"))))
        
      }
      print(final_groups)
      print(alpha_mat[which(alpha_mat[,1] != "0"),])
      plot.heatmap.by.finalgroups(nSpikes,timesinBoxes,final_groups,neuron,ses,rat)
    } 
  }
  print("Returning from plot")
}

################################################################################3
##### Plot Heatmap based on final groups
plot.heatmap.by.finalgroups = function(nSpikes,timesinBoxes,final_groups,neuron,ses,rat){
  total_trials =dim(timesinBoxes)[1]
  total_boxes = dim(timesinBoxes)[2]
  firingrates= matrix(0,total_trials,total_boxes)
  for(i in 1:total_boxes){
    for(j in 1:length(final_groups[[i]])){
      
      nspikes <- sum(nSpikes[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i])
      timeinboxes <- sum(timesinBoxes[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i])
      firingrates[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i]=nspikes*1000/timeinboxes
      firingrates[which(timesinBoxes[,i]==0),i] <- NA
    }
  }
  matrix.seriate(t(firingrates),neuron,ses,rat)
}

############################
### get time spend in each box
getTimeSpentInBox=function(bx_pos,enreg,ses){
  total_time_in_bx=0
  if(length(bx_pos)>1){
    time_bx <- 1
    time_bx <-c(time_bx,which(diff(bx_pos)!=1))
    time_bx <-c(time_bx,(which(diff(bx_pos)!=1)+1))
    time_bx <- c(time_bx,length(bx_pos))
    time_bx <- sort(time_bx)
    for(i in 1:(length(time_bx)/2)){
      total_time_in_bx = total_time_in_bx+as.numeric(enreg[[ses]]$POS[bx_pos[time_bx[2*i]],1]) - as.numeric(enreg[[ses]]$POS[bx_pos[time_bx[2*i-1]],1])
    }
  }
  return(total_time_in_bx)
}


###################################
####### Test for homogeneity in a group 
testHomogeneity=function(newSpikes,newTimesinBox){
  pval=0
 
  if(length(newSpikes)==2){
    pval1 = 2*pbinom(newSpikes[1],size=sum(newSpikes),prob=newTimesinBox[1]/sum(newTimesinBox))
    pval2 = 2*(1-pbinom((newSpikes[1]-1),size = sum(newSpikes),prob=newTimesinBox[1]/sum(newTimesinBox)))
    pval=min(c(pval1,pval2,1))
    
  }else{
    pval <- chisq.test(newSpikes,p=newTimesinBox/sum(newTimesinBox))[[3]]
  }
  return(pval)
}



################################################################
#### Get pvalues for homogeneity in boxes #####################################################
groupBoxesForChiSqTest=function(nSpikes,timesinBoxes,last_trial){
  groups <-list()
  newSpikes <- list()
  newTimesinBox <-list()
  for(i in 1:15){
    #l<-which(timesinBoxes[,i]!=0)
    sum=0
    prevIndex=0
    groups[[i]] <- list()
    newSpikes[[i]] <- numeric()
    newTimesinBox[[i]] <- numeric()
    print(sprintf("i=%i",i))
    for(j in 1:last_trial){
      ### If the box is not visited at all for the complete session, set sum to zero and do not compute (timesinBoxes[j,i]/sum(timesinBoxes[,i]))
      if(sum(timesinBoxes[,i])==0 ){
        sum=0
      }else{
        sum=sum+(nSpikes[j,i]*(timesinBoxes[j,i]/sum(timesinBoxes[,i])))
      }
      
      
      if(j==last_trial && sum <5 && length(groups[[i]]) > 1 ){
        ### If the last group has sum <5 , add the the previous group
        print("Add last batch of trials to the previous group as sum < 5")
        groups[[i]][[length(groups[[i]])]] <- list.append(groups[[i]][[length(groups[[i]])]],c((prevIndex+1):j))
        newSpikes[[i]][[length(newSpikes[[i]])]] = newSpikes[[i]][[length(newSpikes[[i]])]] + sum(nSpikes[(prevIndex+1):j,i])
        newTimesinBox[[i]][[length(newTimesinBox[[i]])]] = newTimesinBox[[i]][[length(newTimesinBox[[i]])]] + sum(timesinBoxes[(prevIndex+1):j,i])
      }
      else if(j==last_trial && sum <5 && length(groups[[i]]) ==0){
        ### Not even 1 group, so cannot do chi-square
        ### Split into 2 groups to do Sepideh's test
        print("Split into 2 groups to do Sepideh's test as there are 0 groups and sum < 5 ")
        groups[[i]] <- list.append(groups[[i]],c(1:round2(length(nSpikes[,i])/2,0)))
        groups[[i]] <- list.append(groups[[i]],c(round2(length(nSpikes[,i])/2 + 1,0): length(nSpikes[,i])))
        
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[1:round2(length(nSpikes[,i])/2,0),i]))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(round2(length(nSpikes[,i])/2 + 1,0)): length(nSpikes[,i]) ,i]))
        
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[1:round2(length(timesinBoxes[,i])/2,0),i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[round2(length(timesinBoxes[,i])/2 + 1,0): length(timesinBoxes[,i]) ,i]))
      }
      else if(j==last_trial && sum <5 && length(groups[[i]]) ==1){
        ### Just 1 group, so cannot do chi-square
        ### Split into 2 groups to do Sepideh's test
        print("Split into 2 groups to do Sepideh's test as there is just 1 group and sum < 5 ")
        groups[[i]] <- list()
        newSpikes[[i]] <- numeric()
        newTimesinBox[[i]] <- numeric()
        groups[[i]] <- list.append(groups[[i]],c(1:round2(length(nSpikes[,i])/2,0)))
        groups[[i]] <- list.append(groups[[i]],c(round2(length(nSpikes[,i])/2 + 1,0): length(nSpikes[,i])))
        
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[1:round2(length(nSpikes[,i])/2,0),i]))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(round2(length(nSpikes[,i])/2 + 1,0)): length(nSpikes[,i]) ,i]))
        
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[1:round2(length(timesinBoxes[,i])/2,0),i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[round2(length(timesinBoxes[,i])/2 + 1,0): length(timesinBoxes[,i]) ,i]))
      }else if(j==last_trial && sum >5 && length(groups[[i]]) ==1){
        ### Just 1 group, so cannot do chi-square
        ### Split into 2 groups to do Sepideh's test
        print("Split into 2 groups to do Sepideh's test as there is just 1 group and sum < 5 ")
        groups[[i]] <- list()
        newSpikes[[i]] <- numeric()
        newTimesinBox[[i]] <- numeric()
        groups[[i]] <- list.append(groups[[i]],c(1:round2(length(nSpikes[,i])/2,0)))
        groups[[i]] <- list.append(groups[[i]],c(round2(length(nSpikes[,i])/2 + 1,0): length(nSpikes[,i])))
        
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[1:round2(length(nSpikes[,i])/2,0),i]))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(round2(length(nSpikes[,i])/2 + 1,0)): length(nSpikes[,i]) ,i]))
        
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[1:round2(length(timesinBoxes[,i])/2,0),i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[round2(length(timesinBoxes[,i])/2 + 1,0): length(timesinBoxes[,i]) ,i]))
      }else if(sum>5){
        print("Add new group as sum > 5")
        sum=0
        groups[[i]] <- list.append(groups[[i]],c((prevIndex+1):j))
        print(sprintf("nspikes= %s",sum(nSpikes[(prevIndex+1):j,i])))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(prevIndex+1):j,i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[(prevIndex+1):j,i]))
        prevIndex =j
      }
    }
    
  }
  
  
  results <- list()
  results$groups <- groups
  results$newSpikes <- newSpikes
  results$newTimesinBox <- newTimesinBox
  return(results)
}




###########################################################
##### Regroup non-homogenous boxes ##########################################3
### 1) Combine all groups into 2 sets
### 2) If either set is homoegenous,keep the set of groups
### 3) If non-homogenous, split in 2 and repeat until group size =1 or pval<0.05

regroupBoxes=function(output,i,pval_alpha,alpha_mat,matIndex){
  ### If groups = 10, combine 5 groups each and test homogenity
  ### If group1 = 5 is not homogenous, split into 2 groups of 2 & 3
  
  
  ### Combine into 2 big groups
  newgroups <-list()
  newspikes <-list()
  newtimesinbox <-list()
  if(length(output$groups[[i]])==2){
    ### New group will have just size 1
    newgroups <- list.append(newgroups,c(1:2))
    newspikes <- list.append(newspikes,c(output$newSpikes[[i]][1:length(output$newSpikes[[i]])]))
    newtimesinbox <- list.append(newtimesinbox,c(output$newTimesinBox[[i]][1:length(output$newTimesinBox[[i]])]))
  }else{
    newgroups <- list.append(newgroups,1:round2(length(output$groups[[i]])/2,0)) 
    newgroups <- list.append(newgroups,round2(length(output$groups[[i]])/2 +1,0):length(output$groups[[i]]))
    newspikes <- list.append(newspikes,c(output$newSpikes[[i]][1:round2(length(output$newSpikes[[i]])/2,0)]))
    newspikes <- list.append(newspikes,c(output$newSpikes[[i]][round2(length(output$newSpikes[[i]])/2 + 1,0): length(output$newSpikes[[i]])]))
    newtimesinbox <- list.append(newtimesinbox,c(output$newTimesinBox[[i]][1:round2(length(output$newTimesinBox[[i]])/2,0)]))
    newtimesinbox <- list.append(newtimesinbox,c(output$newTimesinBox[[i]][round2(length(output$newTimesinBox[[i]])/2 + 1,0): length(output$newTimesinBox[[i]])]))
  }
  
 
  #### Check for homogenity in the 2 big groups -- > with pval still pval_alpha
  ### Verify p-val calculation ???
  final_group <-list()
  
  for(j in 1:length(newgroups)){
    
    #debug(splitAllGroups)
    print(sprintf("For box = %i, split group = %s",i,unlist(newgroups[[j]])))
    output1 <- splitAllGroups(newgroups[[j]],output,i,pval_alpha,alpha_mat,matIndex)
    final_group <- c(final_group,output1$final_group)
    alpha_mat <- output1$alpha_mat
    matIndex=matIndex+1
    
  }

  newList <- list("final_group" = final_group, "alpha_mat" = alpha_mat)
  return(newList)
}

################################################################333
###### Use this function recursively to split until you get good groups
splitAllGroups=function(newgroups,output,i,pval_alpha,alpha_mat,matIndex){
  print(sprintf("Inside split group, i=%i",i))
  
  final_group <- list()
  #for(j in 1:length(newgroups)){
    ## Get nspikes for newgroups[[i]]
    ## Get timeiin boxes for newgroups[[i]]
    if(length(newgroups)==1){
      final_group <- list.append(final_group,unlist(newgroups))
      alpha_mat[matIndex,1] =  paste( unlist(newgroups), collapse=',')## Newgroup
      alpha_mat[matIndex,2] = NA ## Alpha level for H0
      alpha_mat[matIndex,3] = NA ## Pval of H0 test
      alpha_mat[matIndex,4] = NA
      alpha_mat[matIndex,5] = "No"
      
    }else{
      
      nspikes  <- numeric()
      timeinboxes <- numeric()
      nspikes <- output$newSpikes[[i]][min(newgroups):max(newgroups)]
      timeinboxes <- output$newTimesinBox[[i]][min(newgroups):max(newgroups)]
      pval <- testHomogeneity(nspikes,timeinboxes)
      
      
      alpha_mat[matIndex,1] =  paste( unlist(newgroups), collapse=',')## Newgroup
      alpha_mat[matIndex,2] = pval_alpha ## Alpha level for H0
      alpha_mat[matIndex,3] = pval ## Pval of H0 test
      
      if(pval > pval_alpha){
        
        alpha_mat[matIndex,4] = "No"
        alpha_mat[matIndex,5] = "No"
        final_group <- c(final_group,newgroups)
      }else if(lengths(newgroups)>2) {
        
        alpha_mat[matIndex,4] = "Yes"
        alpha_mat[matIndex,5] = "Yes"
        pval_alpha = pval_alpha/2
        matIndex=matIndex+1
        newList <- splitAllGroups(newgroups,output,i,alpha_mat,pval_alpha,matIndex)
        final_group <- newList$final_group
        alpha_mat <- newList$alpha_mat
        #split newgroups[i]
      }else if(length(newgroups)==2) {
        #stop split and add to final group
        
        alpha_mat[matIndex,4] = "Yes"
        alpha_mat[matIndex,5] = "No"
        final_group <- list.append(final_group,c(1))
        final_group <- list.append(final_group,c(2))
      }else{
        ## ?? Required ???
        final_group <- c(final_group,newgroups)
        
      }
    } 
   
  #print(alpha_mat[which(alpha_mat[1] != "0"),])
  newList <- list("final_group" = final_group, "alpha_mat" = alpha_mat)
  return(newList)
}

###########################################################################
#### Plot seriated matrix 
matrix.seriate=function(mat,neuron,ses,rat){

  longData<-melt(t(mat))
  #mat[which(is.nan(mat))]<-0
  #mat[which(is.infinite(mat))] <- 0
  # o<-seriate(mat, method = "BEA_TSP")
   #longData$Var1 <- factor(longData$Var1, (unlist(o[[1]][])))
   #longData$Var2 <- factor(longData$Var2, names(unlist(o[[2]][])))
  # longData<-longData[longData$value!=0,]
  ggplot(longData, aes(x = Var1, y = Var2)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_continuous(low="thistle2", high="darkred", 
                          guide="colorbar",na.value="black") +
    labs(x="Trials", y="Boxes", title=paste('Heatmap_',rat,'_neuron_',neuron,'_ses_',ses,sep="")) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11)) + scale_y_continuous(breaks=c(1:15),labels= c("A","B","B'","C","C'","D","E","E'","F","G","H","I","I'","J","K") )

  
  ggsave(paste('Heatmap_seriated_',rat,'_Neuron_',neuron,'_ses_',ses,'.png',sep=""), device = "png",width = 16, height = 9, dpi = 100)
}