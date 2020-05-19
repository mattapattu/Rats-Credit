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

#### Function to call for plotting heatmap
plot.heatmap.paths=function(enreg,rat,dirpath1,plot){

  dirpath2 = file.path(dirpath1,rat)
  dir.create(dirpath2)

  ### Loop through all enreg[[ses]] of current rat
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
    ### Loop through each neuron in enreg[[ses]] of current rat
    for(neuron in 1:neurons){
      print(sprintf("%s session %i neuron %i",rat, ses,neuron))
      #### Note: Mat is not required to compute heatmap, just added to get image of firingrates initially
      mat <-matrix(0, last_trial, 12)
      colnames(mat) <- c("Path1-49","Path2-49","Path3-49","CorrPath-49","Path5-49'","UnkownPath-49","Path1-51","Path2-51","Path3-51","CorrPath-51","Path5-51","UnkownPath-51")
      trialIndex = 1
      ### nSpikes - store spikes in each box for every trial
      nSpikes <- matrix(0, last_trial, 12)
      ### timesinBoxes - store time spend in each box for every trial
      timesinBoxes <- matrix(0, last_trial, 12)
      
      for(t in 1:last_trial){
        
        pos_trial_t  <- which(enreg[[ses]]$POS[,"trial"] == t)
        time_spent_in_trial = as.numeric(enreg[[ses]]$POS[pos_trial_t[length(pos_trial_t)],1]) - as.numeric(enreg[[ses]]$POS[pos_trial_t[1],1])
        if(length(grep("h.*c.*d.*e",allpaths[t],value = FALSE))>0){
          pathNb=1
        }else if(length(grep("j.*k.*a.*g.*f.*e",allpaths[t],value = FALSE))>0){
          pathNb=2
        }else if(length(grep("h.*c.*b.*a.*g.*f.*e",allpaths[t],value = FALSE))>0){
          pathNb=3
        }else if(length(grep("j.*k.*a.*b.*c.*d.*e",allpaths[t],value = FALSE))>0){
          pathNb=4
        }else if(length(grep("j.*i",allpaths[t],value = FALSE))>0){
          pathNb=5
        }else if(length(grep("d.*c.*h.*i",allpaths[t],value = FALSE))>0){
          pathNb=7
        }else if(length(grep("f.*g.*a.*k.*j.*i",allpaths[t],value = FALSE))>0){
          pathNb=8
        }else if(length(grep("d.*c.*b.*a.*k.*j.*i",allpaths[t],value = FALSE))>0){
          pathNb=9
        }else if(length(grep("f.*g.*a.*b.*c.*h.*i",allpaths[t],value = FALSE))>0){
          pathNb=10
        }else if(length(grep("f.*e",allpaths[t],value = FALSE))>0){
          pathNb=11
        }else if(length(grep(".*i$",allpaths[t],value = FALSE))>0){
          pathNb=6 
        }else if(length(grep(".*e$",allpaths[t],value = FALSE))>0){
          pathNb=12 
        }
          
        nSpikes[t,pathNb]=length(enreg[[ses]]$SPIKES[which( enreg[[ses]]$SPIKES[,"trial"]== t & enreg[[ses]]$SPIKES[,"neuron"]== neuron),1])
        timesinBoxes[t,pathNb]=time_spent_in_trial
      }
        
       
      
      ########  Group boxes for chi-square test ##################################
      #debug(groupPathsForChiSqTest)
      output = groupPathsForChiSqTest(nSpikes,timesinBoxes,1,last_trial)
      #print(output$groups)
      
      pvals <-numeric()
      for(i in 1:12){
        pvals <- c(pvals,testHomogeneityPaths(output$newSpikes[[i]],output$newTimesinBox[[i]]))
      }
      
      #alpha-mat - matrix to store pvals for every grouping      
      alpha_mat <- matrix(0,2000,5)
      colnames(alpha_mat) <- c("Box/Newgroup","Alpha","pval","H0 Rej","Split Further")
      matIndex=0
      ########## Check for homogeneity in each box, regroup using splitAllGroupsPaths if non-homogeneous #################
      # final_groups stores the grouping for current session
      final_groups <- list()
      for(i in 1:12){
        pval_alpha <- 0.05/12
        
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
        if(is.nan(pvals[i])){
          ## Box is unvisited during whole  session, therefore pval is NaN
          alpha_mat[matIndex,4] = "Ignore"  ## H0 rejected 
          alpha_mat[matIndex,5] = "No, "  ## Split Further
          final_groups[[i]] <- unlist(output$groups[[i]])
        }
        else if(pvals[i] < pval_alpha){
          alpha_mat[matIndex,4] = "Yes"  ## H0 rejected 
          alpha_mat[matIndex,5] = "Yes"  ## Split Further
          
          ### all trials for box "i" are not homogeneous
          ### Split box into groups of homogeneous trial using chisq test.
          
          #print(sprintf("For box %i, Adjusted pvalue = %f < pval_alpha = %f",i,pvals[i],pval_alpha))
          #print(sprintf("For box %i,H0 can be rejected and box must be regrouped to find homogeneous sub-groups",i))
          #debug(regroupBoxes)
          
          pval_alpha = pval_alpha/2
          matIndex=max(which(alpha_mat[,1] != "0"))+1 ## Add new row in matrix below the current box
          newList <- splitAllGroupsPaths(nSpikes,timesinBoxes,i,1,last_trial,pval_alpha,alpha_mat)
          newgroups <- newList$final_group
          alpha_mat <- newList$alpha_mat
          final_groups[[i]] <- lapply(rapply(newgroups, enquote, how="unlist"), eval)
          
        }else{
          alpha_mat[matIndex,4] = "No"  ## H0 rejected 
          alpha_mat[matIndex,5] = "No"  ## Split Further
          
          #print(sprintf("For box i=%i,Adjusted pvalue = %f > pval_alpha = %f",i,pvals[i],pval_alpha))
          #print(sprintf("For box i=%i,H0 cannot be rejected",i))
          ### all trials for box i are homogeneous
          final_groups[[i]] <- unlist(output$groups[[i]])
          
        }
        
      }
      #print(final_groups)
      print(alpha_mat[which(alpha_mat[,1] != "0"),])
      newlist =plot.heatmappaths.by.finalgroups(nSpikes,timesinBoxes,final_groups,neuron,ses,rat,dirpath,plot)
      
      filename = file.path(dirpath,paste(rat,'_Neuron_',neuron,'_ses_',ses,'.Rdata',sep=""))
      pval_matrix<-alpha_mat[which(alpha_mat[,1] != "0"),]
      labels = newlist$labels
      firingrates = newlist$firingrates
      save(firingrates,labels,final_groups,pval_matrix,file=filename)
      
    } 
  }
  print("Returning from plot.heatmap")
}

################################################################################3
##### Plot Heatmap based on the final groups
plot.heatmappaths.by.finalgroups = function(nSpikes,timesinBoxes,final_groups,neuron,ses,rat,dirpath,plot){
  total_trials =dim(timesinBoxes)[1]
  total_boxes = dim(timesinBoxes)[2]
  firingrates= matrix(0,total_trials,total_boxes)
  colnames(firingrates) <- c("Path1-49","Path2-49","Path3-49","CorrPath-49","Path5-49","UnkownPath-49","Path1-51","Path2-51","Path3-51","CorrPath-51","Path5-51","UnkownPath-51")
  labels = matrix("",total_trials,total_boxes)
  colnames(labels) <- c("Path1-49","Path2-49","Path3-49","CorrPath-49","Path5-49","UnkownPath-49","Path1-51","Path2-51","Path3-51","CorrPath-51","Path5-51","UnkownPath-51")
  for(i in 1:total_boxes){
    if(typeof(final_groups[[i]])=="integer"){
      nspikes <- sum(nSpikes[min(final_groups[[i]]):max(final_groups[[i]]),i])
      timeinboxes <- sum(timesinBoxes[min(final_groups[[i]]):max(final_groups[[i]]),i])
      if(timeinboxes==0){
        firingrates[min(final_groups[[i]]):max(final_groups[[i]]),i] <- NA
      }else{
        firingrates[min(final_groups[[i]]):max(final_groups[[i]]),i]=nspikes*1000/timeinboxes
      }
      
      #firingrates[which(timesinBoxes[,i]==0),i] <- NA
      labels[which(timesinBoxes[,i]==0),i] <- "-"
    }else{
      for(j in 1:length(final_groups[[i]])){
        nspikes <- sum(nSpikes[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i])
        timeinboxes <- sum(timesinBoxes[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i])
        if(timeinboxes==0){
          firingrates[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i] <- NA
        }else{
          firingrates[min(final_groups[[i]][[j]]):max(final_groups[[i]][[j]]),i]=nspikes*1000/timeinboxes
        }
        
        labels[which(timesinBoxes[,i]==0),i] <- "-"
      }
    }
  }
  #print(firingrates)
  #debug(matrix.seriate)
  if(plot){
    matrix.seriate.paths(t(firingrates),neuron,ses,rat,labels,dirpath)
  }
  #firingrates[which(labels=="-")] <- NA
  
  results <- list()
  results$firingrates <- firingrates
  results$labels <- labels
  
  return(results)
}


###################################
####### Test for homogeneity in a group 
testHomogeneityPaths=function(newSpikes,newTimesinBox){
  pval=0
  options(warn=1)
  if(length(newSpikes)==2){
    ### Do Sepideh's Test as there are only 2 groups
    pval1 = 2*pbinom(newSpikes[1],size=sum(newSpikes),prob=newTimesinBox[1]/sum(newTimesinBox))
    pval2 = 2*(1-pbinom((newSpikes[1]-1),size = sum(newSpikes),prob=newTimesinBox[1]/sum(newTimesinBox)))
    pval=min(c(pval1,pval2,1))
    
  }else{
    ### do ChiSquare Test
    pval <- chisq.test(newSpikes,p=newTimesinBox/sum(newTimesinBox))[[3]]
  }
  options(warn=0)
  
  return(pval)
}



################################################################
#### Get pvalues for homogeneity in boxes #####################################################
groupPathsForChiSqTest=function(nSpikes,timesinBoxes,start_trial,end_trial){
  groups <-list()
  newSpikes <- list()
  newTimesinBox <-list()
  for(i in 1:NCOL(nSpikes)){
    #l<-which(timesinBoxes[,i]!=0)
    sum=0
    prevIndex=start_trial-1
    groups[[i]] <- list()
    newSpikes[[i]] <- numeric()
    newTimesinBox[[i]] <- numeric()
    #print(sprintf("i=%i",i))
    for(j in start_trial:end_trial){
      ### If the box is not visited at all for the complete session, set sum to zero and do not compute (timesinBoxes[j,i]/sum(timesinBoxes[,i]))
      if(sum(timesinBoxes[,i])==0 ){
        sum=0
      }else{
        sum=sum+(sum(nSpikes[start_trial:end_trial,i])*(timesinBoxes[j,i]/sum(timesinBoxes[,i])))
      }
      
      
      if(j==end_trial && sum <5 && length(groups[[i]]) > 1 ){
        ### If the last group has sum <5 , add the the previous group
        #print("Add last batch of trials to the previous group as sum < 5")
        groups[[i]][[length(groups[[i]])]] <- list.append(groups[[i]][[length(groups[[i]])]],c((prevIndex+1):j))
        newSpikes[[i]][[length(newSpikes[[i]])]] = newSpikes[[i]][[length(newSpikes[[i]])]] + sum(nSpikes[(prevIndex+1):j,i])
        newTimesinBox[[i]][[length(newTimesinBox[[i]])]] = newTimesinBox[[i]][[length(newTimesinBox[[i]])]] + sum(timesinBoxes[(prevIndex+1):j,i])
      }
      else if(j==end_trial && sum <5 && length(groups[[i]]) == 0){
        ### Not even 1 group, so cannot do chi-square
        ### Split into 2 groups to do Sepideh's test
        #print("Split into 2 groups to do Sepideh's test as there are 0 groups and sum < 5 ")
        groups[[i]] <- list.append(groups[[i]],c(start_trial:round2((end_trial+start_trial)/2,0)))
        groups[[i]] <- list.append(groups[[i]],c(round2((end_trial+start_trial)/2+1,0): end_trial))
        
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[start_trial:round2((end_trial+start_trial)/2,0),i]))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[round2((end_trial+start_trial)/2+1,0): end_trial,i]))
        
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[start_trial:round2((end_trial+start_trial)/2,0),i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[round2((end_trial+start_trial)/2+1,0): end_trial ,i]))
      }
      else if(j==end_trial && sum <5 && length(groups[[i]]) == 1){
        ### Just 1 group, so cannot do chi-square
        ### Group the rest together into 2nd group to do Sepideh's test
        #print("Split into 2 groups to do Sepideh's test as there are 0 groups and sum < 5 ")
        groups[[i]] <- list.append(groups[[i]],c((prevIndex+1):j))
        #print(sprintf("nspikes= %s",sum(nSpikes[(prevIndex+1):j,i])))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(prevIndex+1):j,i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[(prevIndex+1):j,i]))
      }
      else if(j==end_trial && sum >=5 && length(groups[[i]]) ==0){
        ### Just 1 group, so cannot do chi-square
        ### Split into 2 groups to do Sepideh's test
        #print("Split into 2 groups to do Sepideh's test as there is just 1 group and sum < 5 ")
        groups[[i]] <- list()
        newSpikes[[i]] <- numeric()
        newTimesinBox[[i]] <- numeric()
        groups[[i]] <- list.append(groups[[i]],c(start_trial:round2((end_trial+start_trial)/2,0)))
        groups[[i]] <- list.append(groups[[i]],c(round2((end_trial+start_trial)/2+1,0): end_trial))
        
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[start_trial:round2((end_trial+start_trial)/2,0),i]))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[round2((end_trial+start_trial)/2+1,0): end_trial,i]))
        
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[start_trial:round2((end_trial+start_trial)/2,0),i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[round2((end_trial+start_trial)/2+1,0): end_trial ,i]))
        
      }else if(j==end_trial && sum >=5 && length(groups[[i]]) == 1){
        ### Just 1 group, so cannot do chi-square
        ### Group the rest together into 2nd group to do Sepideh's test
        #print("Split into 2 groups to do Sepideh's test as there are 0 groups and sum < 5 ")
        groups[[i]] <- list.append(groups[[i]],c((prevIndex+1):j))
        #print(sprintf("nspikes= %s",sum(nSpikes[(prevIndex+1):j,i])))
        newSpikes[[i]] <- c(newSpikes[[i]],sum(nSpikes[(prevIndex+1):j,i]))
        newTimesinBox[[i]] <- c(newTimesinBox[[i]],sum(timesinBoxes[(prevIndex+1):j,i]))
      }
      else if(sum>=5){
        #print("Add new group as sum > 5")
        sum=0
        groups[[i]] <- list.append(groups[[i]],c((prevIndex+1):j))
        #print(sprintf("nspikes= %s",sum(nSpikes[(prevIndex+1):j,i])))
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





################################################################333
###### Use this function recursively to split until you get good groups
splitAllGroupsPaths=function(nSpikes,timesinBoxes,i,start_trial,end_trial,pval_alpha,alpha_mat){
  #print(sprintf("Inside split group, i=%i",i))
  
  final_group <- list()
  pval_alpha_orig = pval_alpha
  #### Split trials into 2 groups - newgroup1 & newgroup2 
  newgroup1 = start_trial:round2((end_trial+start_trial)/2,0)
  newgroup2 = round2((((end_trial+start_trial)/2) + 1),0):end_trial
  #### Split nSpikes into 2 groups accroding to above groups - newNspikes1 & newNspikes2 
  newNspikes1 = nSpikes[newgroup1,i]
  newNspikes2 = nSpikes[newgroup2,i]
  #### Split timesinBoxes into 2 groups accroding to above groups - newTimesinBoxes1 & newTimesinBoxes2
  newTimesinBoxes1 = timesinBoxes[newgroup1,i]
  newTimesinBoxes2 = timesinBoxes[newgroup2,i]
  
  ############################## New group 1 - newgroup1 ###############################3  
  matIndex=max(which(alpha_mat[,1] != "0"))+1
  if(length(newgroup1)==1) {
    #stop split and add to final group as length =1
    
    alpha_mat[matIndex,4] = "NA"
    alpha_mat[matIndex,5] = "No"
    alpha_mat[matIndex,1] = unlist(newgroup1)
    final_group <- list.append(final_group,newgroup1)
  }else if(length(newgroup1)==2){
    ## Only 2 trials in newgroup1, if not homogeneous, add them as separate cells - no need to split further
    
    ### Test for homogeneity in newgroup1
    pval1 = testHomogeneityPaths(newNspikes1,newTimesinBoxes1)
    
    alpha_mat[matIndex,1] = paste( unlist(newgroup1), collapse=',')
    alpha_mat[matIndex,2] = pval_alpha_orig ## Alpha level for H0
    alpha_mat[matIndex,3] = pval1 ## Pval of H0 test
    
    if(is.nan(pval1)){
      ### 0 spikes in all trials in newgroup1, combine them
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste( unlist(newgroup1), collapse=',')
      final_group <- list.append(final_group,unlist(newgroup1))
    }else if(pval1 < pval_alpha_orig){
      ## not homogeneous, add them as separate cells - no need to split further
      final_group <- list.append(final_group,newgroup1[1])
      final_group <- list.append(final_group,newgroup1[2])
      alpha_mat[matIndex,4] = "Yes" 
      alpha_mat[matIndex,5] = "No"
    }else{
      ## all trials in newgroup1 are  homogeneous - add to final_group
      final_group <- list.append(final_group,unlist(newgroup1))
      alpha_mat[matIndex,4] = "No" 
      alpha_mat[matIndex,5] = "No"
    }
    
  }else{
    ## More than  2 trials in newgroup1, if not homogeneous, call splitAllGroupsPaths recursively
    output1 <- groupPathsForChiSqTest(nSpikes[,i,drop=FALSE],timesinBoxes[,i,drop=FALSE],start_trial,round2((end_trial+start_trial)/2,0))
    
    ### Test for homogeneity in newgroup1
    pval1 <- testHomogeneityPaths(output1$newSpikes[[1]],output1$newTimesinBox[[1]])
    
    
    alpha_mat[matIndex,2] = pval_alpha_orig ## Alpha level for H0
    alpha_mat[matIndex,3] = pval1 ## Pval of H0 test
    
    
    ### 0 spikes in all trials in newgroup1, combine them
    if(is.nan(pval1)){
      
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste(min(unlist(output1$groups[[1]])),max(unlist(output1$groups[[1]])),sep=":")
      final_group <- list.append(final_group,unlist(output1$groups[[1]]))
    }else  if(pval1 > pval_alpha_orig){
      ## all trials in newgroup1 are  homogeneous - add to final_group
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste(min(unlist(output1$groups[[1]])),max(unlist(output1$groups[[1]])),sep=":")
      final_group <- list.append(final_group,unlist(output1$groups[[1]]))
    }else if(length(newgroup1)>2){
      ## all trials in newgroup1 are not  homogeneous - call splitAllGroupsPaths recursively until homogeneous groups are found.
      alpha_mat[matIndex,4] = "Yes" 
      alpha_mat[matIndex,5] = "Yes"
      alpha_mat[matIndex,1] = paste(min(unlist(output1$groups[[1]])),max(unlist(output1$groups[[1]])),sep=":")
      pval_alpha = pval_alpha_orig/2
      newList1 <- splitAllGroupsPaths(nSpikes,timesinBoxes,i,min(newgroup1),max(newgroup1),pval_alpha,alpha_mat)
      final_group <- list.append(final_group,newList1$final_group)
      alpha_mat <- newList1$alpha_mat
    }
  }
  
  ##############################New group 2 - newgroup2###############################3  
  
  matIndex=max(which(alpha_mat[,1] != "0"))+1
  
  if(length(newgroup2)==1) {
    #stop split and add to final group
    
    alpha_mat[matIndex,4] = "NA"
    alpha_mat[matIndex,5] = "No"
    alpha_mat[matIndex,1] = unlist(newgroup2)
    final_group <- list.append(final_group,newgroup2)
  }
  else if(length(newgroup2)==2){
    pval2 = testHomogeneityPaths(newNspikes2,newTimesinBoxes2)
    
    alpha_mat[matIndex,1] = paste( unlist(newgroup2), collapse=',')
    alpha_mat[matIndex,2] = pval_alpha_orig ## Alpha level for H0
    alpha_mat[matIndex,3] = pval2 ## Pval of H0 test
    
    ### No spikes in group, and not visited, combine
    if(is.nan(pval2)){
      
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste( unlist(newgroup2), collapse=',')
      final_group <- list.append(final_group,unlist(newgroup2))
    }else if(pval2 < pval_alpha_orig){
      
      final_group <- list.append(final_group,newgroup2[1])
      final_group <- list.append(final_group,newgroup2[2])
      alpha_mat[matIndex,4] = "Yes" 
      alpha_mat[matIndex,5] = "No"
    }else{
      
      final_group <- list.append(final_group,unlist(newgroup2))
      alpha_mat[matIndex,4] = "No" 
      alpha_mat[matIndex,5] = "No"
    }
    
  }else{
    output2 = groupPathsForChiSqTest(nSpikes[,i,drop=FALSE],timesinBoxes[,i,drop=FALSE],round2((((end_trial+start_trial)/2) + 1),0),end_trial)
    pval2 = testHomogeneityPaths(output2$newSpikes[[1]],output2$newTimesinBox[[1]])
    alpha_mat[matIndex,1] = paste( unlist(output2$groups[[1]]), collapse=',')## Newgroup
    alpha_mat[matIndex,2] = pval_alpha_orig ## Alpha level for H0
    alpha_mat[matIndex,3] = pval2 ## Pval of H0 test
    
    if(is.nan(pval2)){
      
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste(min(unlist(output2$groups[[1]])),max(unlist(output2$groups[[1]])),sep=":")
      final_group <- list.append(final_group,unlist(output2$groups[[1]]))
    }else if(pval2 > pval_alpha_orig){
      
      alpha_mat[matIndex,4] = "No"
      alpha_mat[matIndex,5] = "No"
      alpha_mat[matIndex,1] = paste(min(unlist(output2$groups[[1]])),max(unlist(output2$groups[[1]])),sep=":")
      final_group <- list.append(final_group,unlist(output2$groups[[1]]))
      
    }else if(pval2 < pval_alpha_orig && length(newgroup2)>2) {
      alpha_mat[matIndex,4] = "Yes" 
      alpha_mat[matIndex,5] = "Yes"
      alpha_mat[matIndex,1] = paste(min(unlist(output2$groups[[1]])),max(unlist(output2$groups[[1]])),sep=":")
      pval_alpha = pval_alpha_orig/2
      newList2 <- splitAllGroupsPaths(nSpikes,timesinBoxes,i,min(newgroup2),max(newgroup2),pval_alpha,alpha_mat)
      final_group <- list.append(final_group,newList2$final_group)
      alpha_mat <- newList2$alpha_mat
      
    }
    
  }

  #print(alpha_mat[which(alpha_mat[1] != "0"),])
  newList <- list("final_group" = final_group, "alpha_mat" = alpha_mat)
  return(newList)
}

###########################################################################
#### Plot seriated matrix 
matrix.seriate.paths=function(mat,neuron,ses,rat,labels,dirpath){
  
  
  rownames <- c("Path1-49","Path1-51","Path2-49","Path2-51","Path3-49","Path3-51","CorrPath-49","CorrPath-51","Path5-49","Path5-51","UnkownPath-49","UnkownPath-51")
  colnames <- c("Path1-49","Path1-51","Path2-49","Path2-51","Path3-49","Path3-51","CorrPath-49","CorrPath-51","Path5-49","Path5-51","UnkownPath-49","UnkownPath-51")
  mat<-mat[rownames,,drop=FALSE]
  labels <- labels[,colnames,drop=FALSE]
  longData<-melt((mat))
  #mat[which(is.nan(mat))]<-0
  #mat[which(is.infinite(mat))] <- 0
  # o<-seriate(mat, method = "BEA_TSP")
  #longData$Var1 <- factor(longData$Var1, (unlist(o[[1]][])))
  #longData$Var2 <- factor(longData$Var2, names(unlist(o[[2]][])))
  # longData<-longData[longData$value!=0,]
  # ggplot(longData, aes(x = Var2, y = Var1)) + 
  #   geom_raster(aes(fill=value)) + 
  #   scale_fill_continuous(low="grey90", high="red", 
  #                         guide="colorbar",na.value="white") +
  #   labs(x="Trials", y="Boxes", title=paste('Heatmap_',rat,'_neuron_',neuron,'_ses_',ses,sep="")) +
  #   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
  #                      axis.text.y=element_text(size=9),
  #                      plot.title=element_text(size=11)) +geom_text(aes(label = as.vector(labels)))
  ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_continuous(low="grey90", high="red", 
                          guide="colorbar",na.value="yellow") +
    labs(x="Trials", y="Boxes", title=paste('Heatmap_',rat,'_neuron_',neuron,'_ses_',ses,sep="")) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=-2),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11)) +geom_text(aes(label = as.vector(t(labels))),size=13,vjust = 0.4, hjust = 0.5)+geom_vline(xintercept = c(1:NCOL(mat)), linetype=4)
  
  
  ggsave(path=dirpath,filename=paste('Heatmap_seriated_',rat,'_Neuron_',neuron,'_ses_',ses,'.png',sep=""), device = "png",width = 16, height = 9, dpi = 100)
}