####
###


library(Rmpfr)




R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1

### Use optimal paramters on actual data and compute Mean Squared Error.
mazeACA2=function(enreg,rat){
  
  allpaths <- matrix("",0,2)
  colnames(allpaths) <- c("Path","Session")
  ### Loop through all enreg[[ses]] of current rat
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }else if(rat=="rat_106" && ses==3){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_112" && ses==1){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_113" && ses==13){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }
    
    last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    reward49_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    reward51_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths_ses <- toString(r$values)
    allpaths_ses<-strsplit(allpaths_ses,"(?<=[ei])(?=(, j, k)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths_ses <-cbind(allpaths_ses,ses)
    colnames(allpaths_ses) <- c("Path","Session")
    l<-list(allpaths,allpaths_ses)
    allpaths <- rbind(allpaths,allpaths_ses)
    
    
  }
  allpaths = updateACAPathNb1(allpaths)
  print(sprintf("rat:%s",rat))
  #print(getStatsBeforeRewards(allpaths))
  
  # allpaths = updateACAPathNb(allpaths)
  sourceCpp('C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/aca_mle.cpp')
  
  enreg_comb<-matrix(, nrow = 0, ncol = 7)
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }else if(rat=="rat_106" && ses==3){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_112" && ses==1){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_113" && ses==13){
      print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }
    enreg_comb<-rbind(enreg_comb,enreg[[ses]]$POS)
  }
  l<-cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  
  y<-updateAllpaths(as.numeric(allpaths[,2]),l)
  allpaths<-cbind(allpaths,y)
  
  probEmp=getStatsAllpaths2(allpaths)
  print(probEmp)
  
  ################Call ACA
  
  
  ### Init H
  #H = matrix(0,nrow=2,ncol=6)
  H = matrix(c(-15.826769703,-19.590451568,-18.769041306,-14.974060309,-17.180505943,-6.825588735,2.936037932,-2.532011115,-0.286840528,2.511919180,1.897545027,10.564730887),nrow=2,ncol=6)
  colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(H)<-c("E","I")
  #alpha=0.699
  #alpha=0.671411
  alpha=0.002361247

  
  # H[1,1]=0.1875
  # H[2,1]=0.3125
  # alpha=0.604535749547033
  
  # H[1,1]=0.375
  # H[2,1]=0.625
  # alpha=0.017
  
  # H[1,1]=0.75
  # H[2,1]=0.25
  # alpha=0.00828704774614916
  
  # alpha=0.299893759452973
  # H[1,1]=0.8125
  # H[2,1]=0.8125
  
  #max_steps=500
  #debug(aca_rl)
  probACA=aca_rl2(H,alpha,allpaths)
  print(probACA)
  
  
  #############Call SARSA
  
  
  ### Init Q
  Q = matrix(0,nrow=2,ncol=6)
  colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Q)<-c("E","I")
  # Q[1,1]=0.115581894626535
  # Q[2,1]=0.885489207907076
  
  E=matrix(0,nrow=2,ncol=6)
  colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(E)<-c("E","I")
  
  ## Session 1
  alpha=0.12216275648779
  gamma=0.889464671985183
  epsilon=0
  lambda=1
  #max_steps=1380
  #debug(epsilon_greedy)
  #debug(getNextState)
  #debug(sarsa)
  probSARSA=sarsa_smax2(Q,E,alpha,epsilon,gamma,lambda,allpaths)
  print(probSARSA)
  
  plotProbs(probEmp,probACA,probSARSA,rat)
  
}

updateACAPathNb1=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      allpaths[i,4] = R
    }else{
      allpaths[i,4] = 0
    }
    allpaths[i,3] = getPathNumber(allpaths[i,1])
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,5]=1
    }else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }
    ## Why ? ( incomplete paths ?)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
    }else if(i==1){
      if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=2
      }else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      
    }
    
  }
  return(allpaths)
}

getStatsAllpaths2=function(allpaths){
  probMatrix_allpaths=matrix(0,12,0)
  rownames(probMatrix_allpaths)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  last_session = as.numeric(tail(allpaths[,"Session"],1))
  sessions <- numeric()
  count1=0
  count2=0
  for(ses in 1:last_session){
    allpaths_idx<-which(allpaths[,"Session"]==ses)
    if(length(allpaths_idx)==0){
      next
    }
    allpaths_ses1<-allpaths[allpaths_idx,]
    len1<-length(which(allpaths_ses1[,"State"]==1))
    len2<-length(which(allpaths_ses1[,"State"]==2))
    probMatrix_allpaths <- cbind(probMatrix_allpaths,0)
    colIndex=length(probMatrix_allpaths[1,])
    max_trial_ses=length(allpaths_ses1[,1])
    for(idx in allpaths_idx){
      action=as.numeric(allpaths[idx,3])
      if(allpaths[idx,5]==1){
        count1=count1+1
        probMatrix_allpaths[1,colIndex]=probMatrix_allpaths[1,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==1) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
        probMatrix_allpaths[2,colIndex]=probMatrix_allpaths[2,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==2) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
        probMatrix_allpaths[3,colIndex]=probMatrix_allpaths[3,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==3) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
        probMatrix_allpaths[4,colIndex]=probMatrix_allpaths[4,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==4) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
        probMatrix_allpaths[5,colIndex]=probMatrix_allpaths[5,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==5) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
        probMatrix_allpaths[6,colIndex]=probMatrix_allpaths[6,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==6) & (as.numeric(allpaths[1:idx,5])==1)))/count1)
      }else if(allpaths[idx,5]==2){
        count2=count2+1
        probMatrix_allpaths[7,colIndex]=probMatrix_allpaths[7,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==1) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
        probMatrix_allpaths[8,colIndex]=probMatrix_allpaths[8,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==2) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
        probMatrix_allpaths[9,colIndex]=probMatrix_allpaths[9,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==3) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
        probMatrix_allpaths[10,colIndex]=probMatrix_allpaths[10,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==4) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
        probMatrix_allpaths[11,colIndex]=probMatrix_allpaths[11,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==5) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
        probMatrix_allpaths[12,colIndex]=probMatrix_allpaths[12,colIndex]+(length(which((as.numeric(allpaths[1:idx,3])==6) & (as.numeric(allpaths[1:idx,5])==2)))/count2)
      }
      #print(probMatrix_allpaths[1:6,colIndex])
      #print(probMatrix_allpaths[7:12,colIndex])
    }
    probMatrix_allpaths[1:6,colIndex]=probMatrix_allpaths[1:6,colIndex]/len1
    probMatrix_allpaths[7:12,colIndex]=probMatrix_allpaths[7:12,colIndex]/len2
    
  }
  return(probMatrix_allpaths)
}



getNextState_aca2=function(allpaths,i){
  next_state=0
  if(grepl("^.*e$",allpaths[i,1])){
    next_state = 1
  }else if(grepl("^.*i$",allpaths[i,1])){
    next_state = 2
  }
  ### If allpaths[i,] does not end in E/I , use the begining of next path to find the new state 
  else{
    if(i<length(allpaths[,1])){
      # print("Here")
      s=gsub("^, ","",allpaths[i+1,1])
      if(grepl("^f",s)||grepl("^d",s)){
        next_state = 1
      }else if(grepl("^j",s)||grepl("^h",s)){
        next_state = 2
      }
    }else{
      next_state=-1
    }
    
  }
  
  return(next_state)
}

### Action = Path
## State = E or I

aca_rl2=function(H,alpha,allpaths){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  time_in_trial <-list()
  
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  time_in_trial[[episode]] <-vector()
  
  ## Counter for Actions
  Visits = matrix(0,nrow=2,ncol=6)
  colnames(Visits)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Visits)<-c("E","I")
  
  ses_max = as.numeric(allpaths[length(allpaths[,1]),"Session"])
  probMatrix_aca=matrix(0,nrow=length(allpaths[,1]),ncol=13)
  colnames(probMatrix_aca)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6","Session")
  
  
  
  initState=as.numeric(allpaths[1,5])
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  curr_session=0
  startIndex_session=0
  avg_score=0
  score_episode=0
  episodeFin=0
  allpaths <-cbind(allpaths,probability=0)
  tau=0
  for(i in c(1:(length(allpaths[,1])-1))){
    
    #print(sprintf("Step=%i,Episode=%i",i,episode))
    
    #print(sprintf("i=%i,episode=%i",i,episode))
    
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    pos_trial_t<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    if(length(pos_trial_t)==0){
      next
    }
    R=sum(as.numeric(enreg[[ses]]$POS[pos_trial_t,"Reward"]))
    
    if(R>0){
      score_episode=score_episode+1
    }else{
      score_episode=score_episode+0
    }
    
    A=as.numeric(allpaths[i,3])
    S_prime=getNextState_aca2(allpaths,i)
    
    actions[[episode]] <- append(actions[[episode]],unname(A))
    
    #print("Here")
    #actions <- c(actions,sprintf("S%i-P%i",S,A))
    states[[episode]] <- append(states[[episode]],unname(S))
    time_in_trial[[episode]] <- append(time_in_trial[[episode]],as.numeric(allpaths[i,6]))
    Visits[S,A]=Visits[S,A]+1
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S_prime!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S_prime==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }
    # if(i<exploration_end_trial){
    #   tau=tau1
    # }else{
    #   tau=tau2
    # }
    
    if(episode>1){
      # x<-mpfr(softmax2(A,S,H),128)
      # if(is.infinite(as.numeric(x))){
      #   stop("softmax return Inf")
      # }else if(is.nan(x)){
      #   stop("softmax return Nan")
      # }
      # allpaths[i,"probability"]=as.numeric(x)
      probMatrix_aca[i,13]=ses
      if(S==1){
        probMatrix_aca[i,7:12]=0
        for(act in 1:6){
          x<-mpfr(softmax_aca2(act,1,H),128)
          probMatrix_aca[i,(act)]=as.numeric(x)
        }
      }else if(S==2){
        probMatrix_aca[i,1:6]=0
        for(act in 1:6){
          x<-mpfr(softmax_aca2(act,2,H),128)
          probMatrix_aca[i,(6+act)]=as.numeric(x)
        }
      }
    }
    

    ## Check if episode ended
    if(returnToInitState){
      changeState = F
      returnToInitState = F
      episodeFin=episodeFin+1
      
      
      
      if(episodeFin==14 || i==length(allpaths[,1])){
        
        a<-actions[[episode]]
        s<-states[[episode]]
        t<-time_in_trial[[episode]]
        
        state1_idx=which(s==1)
        state2_idx=which(s==2)
        
        uniq_actions_s1 = unique(a[state1_idx])
        for(action_s1 in uniq_actions_s1){
          activity=length(which(a[state1_idx]==action_s1))*1000/sum(t[state1_idx])
          #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity/Visits[1,uniq_actions_s1[ids]])
          H[1,action_s1]=H[1,action_s1]+alpha*(score_episode-avg_score)*(1-as.numeric(softmax_aca2(action_s1,1,H)))*activity
          #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity)
        }
        
        setdiff_state1 = setdiff(c(1:6),uniq_actions_s1)
        for(action_s1 in setdiff_state1){
          H[1,action_s1]=H[1,action_s1]-alpha*(score_episode-avg_score)*(as.numeric(softmax_aca2(action_s1,1,H)))/(length(state1_idx))
        }
        
        
        uniq_actions_s2 = unique(a[state2_idx])
        for(action_s2 in uniq_actions_s2){
          activity=length(which(a[state2_idx]==action_s2))*1000/sum(t[state2_idx])
          #H[2,uniq_actions_s2[ids]]=H[2,uniq_actions_s2[ids]]+alpha*(score_episode*activity/Visits[2,uniq_actions_s2[ids]])
          H[2,action_s2]=H[2,action_s2]+alpha*(score_episode-avg_score)*(1-as.numeric(softmax_aca2(action_s2,2,H)))*activity
          #H[2,uniq_actions_s2[ids]]=H[2,uniq_actions_s2[ids]]+alpha*(score_episode*activity)
          if(is.nan(H[2,action_s2])){
            print(sprintf("Action=%i, activity=%f",action_s2,activity))
            stop("softmax return Nan")
          }
         
        }
        
        setdiff_state2 = setdiff(c(1:6),uniq_actions_s2)
        for(action_s2 in setdiff_state2){
          H[2,action_s2]=H[2,action_s2]-(alpha*(score_episode-avg_score)*(as.numeric(softmax_aca2(action_s2,2,H)))/(length(state2_idx)))
          if(is.nan(H[2,action_s2])){
            print(sprintf("Action=%i, activity=%f",action_s2,activity))
            stop("softmax return Nan")
          }
        }
        
        ## reset rewards
        score_episode=0
        episodeFin=0
        episode = episode+1

        #print(sprintf("Updating episode to %i",episode))
        if(i <= (length(allpaths[,1])-1)){
          actions[[episode]] <- vector()
          states[[episode]] <- vector()
          time_in_trial[[episode]] <-vector()
          #activations[[episode]] <- vector()
        }
      }
    }
    ### End of episode checkd
    S=S_prime
  }
  
  #capture.output(print(actions), file = "/home/ajames/intership2/actions_ACA.txt")
  #capture.output(print(actions), file = "/home/ajames/intership2/states_ACA.txt")
  
  #print()
  #capture.output(print(actions), file = sprintf("actions-aca.txt"))
  print(sprintf("MSE ACA:"))
  print(getMSE(probMatrix_aca,allpaths))
  probMat_res=getStatsOfLastSession2(probMatrix_aca)
  return(probMat_res)
}

softmax_aca2=function(A,S,H){
  m=max(H[S,])
  x1 <- mpfr(exp((H[S,A]-m)), precBits = 128)
  x2 <- mpfr(exp((H[S,1]-m)), precBits = 128)
  x3 <- mpfr(exp((H[S,2]-m)), precBits = 128)
  x4 <- mpfr(exp((H[S,3]-m)), precBits = 128)
  x5 <- mpfr(exp((H[S,4]-m)), precBits = 128)
  x6 <- mpfr(exp((H[S,5]-m)), precBits = 128)
  x7 <- mpfr(exp((H[S,6]-m)), precBits = 128)
  if(is.infinite(x1)){
    return(1)
  }else{
    pr_A=(x1)/((x2)+(x3)+(x4)+(x5)+(x6)+(x7))
    return(pr_A)
  }
}


getStatsOfLastSession2=function(probMatrix_aca){
  
  ses_max = as.numeric(probMatrix_aca[(length(probMatrix_aca[,13])-1),13])
  probMat_res=matrix(0,nrow=12,ncol=ses_max)
  rownames(probMat_res)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  

  for(ses in 1:ses_max){
    pos_ses<-which(as.numeric(probMatrix_aca[,13])==ses)
    if(isempty(pos_ses)){
      probMat_res[,ses]=NA
      next
    }else{
      
      probMat_res[1,ses]=sum(probMatrix_aca[pos_ses,1])/length(which(probMatrix_aca[pos_ses,1]!=0))
      probMat_res[2,ses]=sum(probMatrix_aca[pos_ses,2])/length(which(probMatrix_aca[pos_ses,2]!=0))
      probMat_res[3,ses]=sum(probMatrix_aca[pos_ses,3])/length(which(probMatrix_aca[pos_ses,3]!=0))
      probMat_res[4,ses]=sum(probMatrix_aca[pos_ses,4])/length(which(probMatrix_aca[pos_ses,4]!=0))
      probMat_res[5,ses]=sum(probMatrix_aca[pos_ses,5])/length(which(probMatrix_aca[pos_ses,5]!=0))
      probMat_res[6,ses]=sum(probMatrix_aca[pos_ses,6])/length(which(probMatrix_aca[pos_ses,6]!=0))
      probMat_res[7,ses]=sum(probMatrix_aca[pos_ses,7])/length(which(probMatrix_aca[pos_ses,7]!=0))
      probMat_res[8,ses]=sum(probMatrix_aca[pos_ses,8])/length(which(probMatrix_aca[pos_ses,8]!=0))
      probMat_res[9,ses]=sum(probMatrix_aca[pos_ses,9])/length(which(probMatrix_aca[pos_ses,9]!=0))
      probMat_res[10,ses]=sum(probMatrix_aca[pos_ses,10])/length(which(probMatrix_aca[pos_ses,10]!=0))
      probMat_res[11,ses]=sum(probMatrix_aca[pos_ses,11])/length(which(probMatrix_aca[pos_ses,11]!=0))
      probMat_res[12,ses]=sum(probMatrix_aca[pos_ses,12])/length(which(probMatrix_aca[pos_ses,12]!=0))
    }
  }
  

  
  probMat_res <- probMat_res[ , !apply(is.na(probMat_res), 2, all)]
  return(probMat_res)
}

plotProbs=function(probEmp,probACA,probSARSA,rat){
  
  for(i in 1:12){
    filename = paste(rat,"Path_probabilites-",rownames(probEmp)[i],".jpg",sep="")
    jpeg(filename,width=800,height=800,quality = 100)
    plot(probEmp[i,],col='black',type='l',ylim=c(0,1),xlab="Sessions",ylab="Path Prob",main=paste(rat,"_Path probability of ",rownames(probEmp)[i],sep=""))
    lines(probACA[i,],col='red',type='l',lty=2)
    lines(probSARSA[i,],col='blue',type='l',lty=3)
    legend("topright", legend=c("Empirical Prob from data", "Probability by ACA","Probability by SARSA"),lty=c(1,2,3),col=c("black", "red","blue"),cex=0.75,bty = "n")
    dev.off()
  }
  
}


getMSE=function(probMatrix_aca,allpaths){
  
  start_index = length(allpaths[,1])
  start_index = round(start_index/2)+1
  max_index=length(allpaths[,1])-start_index
  mseMatrix=matrix(0,nrow=12,ncol=max_index)
  rownames(mseMatrix)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  
  len1<-length(which(as.numeric(allpaths[start_index:length(allpaths[,1]),5])==1))
  len2<-length(which(as.numeric(allpaths[start_index:length(allpaths[,1]),5])==2))
  
  for(i in start_index:length(allpaths[,1])){
    action=as.numeric(allpaths[i,3])
    if(allpaths[i,5]==1){
      mseMatrix[1,i]=mseMatrix[1,i]+(probMatrix_aca[i,1]-(1==action))^2
      mseMatrix[2,i]=mseMatrix[2,i]+(probMatrix_aca[i,1]-(2==action))^2
      mseMatrix[3,i]=mseMatrix[3,i]+(probMatrix_aca[i,1]-(3==action))^2
      mseMatrix[4,i]=mseMatrix[4,i]+(probMatrix_aca[i,1]-(4==action))^2
      mseMatrix[5,i]=mseMatrix[5,i]+(probMatrix_aca[i,1]-(5==action))^2
      mseMatrix[6,i]=mseMatrix[6,i]+(probMatrix_aca[i,1]-(6==action))^2
    }else if(allpaths[i,5]==2){
      mseMatrix[7,i]=mseMatrix[7,i]+(probMatrix_aca[i,1]-(7==action))^2
      mseMatrix[8,i]=mseMatrix[8,i]+(probMatrix_aca[i,1]-(8==action))^2
      mseMatrix[9,i]=mseMatrix[9,i]+(probMatrix_aca[i,1]-(9==action))^2
      mseMatrix[10,i]=mseMatrix[10,i]+(probMatrix_aca[i,1]-(10==action))^2
      mseMatrix[11,i]=mseMatrix[11,i]+(probMatrix_aca[i,1]-(11==action))^2
      mseMatrix[12,i]=mseMatrix[12,i]+(probMatrix_aca[i,1]-(12==action))^2
    }
    
  }
  # mseMatrix[1:6,]=mseMatrix[1:6,]/len1
  # mseMatrix[7:12,]=mseMatrix[7:12,]/len2
  
  total_mse=sum(rowSums(mseMatrix))
  return(total_mse)
}
