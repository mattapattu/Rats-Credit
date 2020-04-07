####
###


library(Rmpfr)
library(DEoptim)



R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1

### Use optimal paramters on actual data and compute Mean Squared Error.
aca_mse=function(enreg,rat){
  
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
  allpaths = updateACAPathNb_acamse(allpaths)
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
  
  y<-updateAllpaths1(as.numeric(allpaths[,2]),l)
  allpaths<-cbind(allpaths,y)
  allpaths_num <- matrix(as.numeric(unlist(allpaths[,c(3,5,4,6)])),nrow=nrow(k))
  ################Call ACA
  sourceCpp('C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/aca_mle2.cpp')
  sourceCpp('C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/aca_mle3.cpp')
  
  ### Estimate paramteres using Model1 or Model2
  
  out <- DEoptim(aca_negLogLik1, lower = c(0,2), upper = c(1,30), allpaths = allpaths_num,  model = 1, DEoptim.control(NP = 140,F = 0.8, CR = 0.9, trace = TRUE))  
  alpha = out$optim$bestmem[1]
  episode_len = floor(out$optim$bestmem[2])
  H<-matrix(c(unname(out$optim$bestmem[3:14])),ncol=6,byrow=T)
  
  #probACA=aca_model1(H,alpha,allpaths)
  #print(probACA)
  mat_res <- matrix(0,nrow=100,ncol=5)
  colnames(mat_res) <- c("Model1 MSE","Model1 bestval","Model2 MSE","Model2 bestval","Selected Model")
  for(iter in c(1:100)){
    #H_mod <- matrix(c(rnorm(1,mean=H[1,1],sd=0.05),rnorm(1,mean=H[1,2],sd=0.05),rnorm(1,mean=H[1,3],sd=0.05),rnorm(1,mean=H[1,4],sd=0.05),rnorm(1,mean=H[1,5],sd=0.05),rnorm(1,mean=H[1,6],sd=0.05),rnorm(1,mean=H[2,1],sd=0.05),rnorm(1,mean=H[2,2],sd=0.05),rnorm(1,mean=H[2,3],sd=0.05),rnorm(1,mean=H[2,4],sd=0.05),rnorm(1,mean=H[2,5],sd=0.05),rnorm(1,mean=H[2,6],sd=0.05)),nrow=2,ncol=6)    #H_mod <- c(runif(1,(H[1,1]-H[1,1]/10),(H[1,1]+H[1,1]/10)),runif(1,(H[1,2]-H[1,2]/10),(H[1,2]+H[1,2]/10)),runif(1,(H[1,3]-H[1,3]/10),(H[1,3]+H[1,3]/10)),runif(1,(H[1,4]-H[1,4]/10),(H[1,4]+H[1,4]/10)),runif(1,(H[1,5]-H[1,5]/10),(H[1,5]+H[1,5]/10)),runif(1,(H[1,6]-H[1,6]/10),(H[1,6]+H[1,6]/10)),runif(1,(H[2,1]-H[2,1]/10),(H[2,1]+H[2,1]/10)),runif(1,(H[2,2]-H[2,2]/10),(H[2,2]+H[2,2]/10)),runif(1,(H[2,3]-H[2,3]/10),(H[2,3]+H[2,3]/10)),runif(1,(H[2,4]-H[2,4]/10),(H[2,4]+H[2,4]/10)),runif(1,(H[2,5]-H[2,5]/10),(H[2,5]+H[2,5]/10)),runif(1,(H[2,6]-H[2,6]/10),(H[2,6]+H[2,6]/10)),nrow=2,ncol=6)
    H_mod <- matrix(0,nrow=2,ncol=6)
    alpha_mod <-alpha
    episode_len_mod <- 2+(episode_len*28)
    total_trials = length(allpaths[,1])
    init_state=as.numeric(allpaths[1,5])
    ### Generate simulated data using the estimated params
    #aca_model1_sim <- aca_gen_model(H_mod,alpha,episode_len_mod,total_trials,init_state,model=1)
    aca_model2_sim <- aca_gen_model(H_mod,alpha,episode_len_mod,total_trials,init_state,model=1)
    
    ### Estimate Model1 parameters for simulated data
    half_index = floor(length(allpaths[,1])/2)
      out_model1 <- DEoptim(acamse_negLogLik,lower = c(0,0), upper = c(1,1),allpaths=aca_model2_sim[1:half_index,],model=1, DEoptim.control(NP=140,F=0.8, CR = 0.9,trace=TRUE))
    alpha_model1_est = out_model1$optim$bestmem[1]
    episode_model1_est = out_model1$optim$bestmem[2]
    H_model1_est<-matrix(c(unname(out_model1$optim$bestmem[3:14])),ncol=6,byrow=T)
    ### Compute MSE for Model1 using simulated data
    mse_model1 <-aca_mse_comp(H_model1_est,alpha_model1_est,episode_model1_est,aca_model2_sim,model=1)
    
    ## Estimate Model2 parameters for simulated data
    out_model2 <- DEoptim(acamse_negLogLik,lower = c(0,0), upper = c(1,1),allpaths=aca_model2_sim[1:half_index,],model=2, DEoptim.control(NP=140,F=0.8, CR = 0.9,trace=TRUE))
    alpha_model2_est = out_model2$optim$bestmem[1]
    episode_model2_est = out_model2$optim$bestmem[2]
    H_model2_est<-matrix(c(unname(out_model2$optim$bestmem[3:14])),ncol=6,byrow=T)
    ## Compute MSE for Model2 using simulated data
    mse_model2 <-aca_mse_comp(H_model2_est,alpha_model2_est,episode_model2_est,aca_model2_sim,model=2)
    
    mat_res[iter,1]=mse_model1
    mat_res[iter,2]=out_model1$optim$bestval
    mat_res[iter,3]=mse_model2
    mat_res[iter,4]=out_model2$optim$bestval
    # mat_res[iter,5]=out_model1$optim$bestmem
    # mat_res[iter,6]=
    
    if(mse_model1<=mse_model2){
      mat_res[iter,5]=1
    }else{
      mat_res[iter,5]=2
    }
    
  }
  
  print(mat_res)
  return(mat_res)
  
}

updateACAPathNb_acamse=function(allpaths){
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
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
      ## If cannot be estimated, then do by default : assume the trial = Path5
      else if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      else if(grepl("^.*i$",allpaths[i,1])){
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



getNextState_ACA_RL=function(curr_state,action){
  if(action == 5){
    new_state=curr_state
  }else if(curr_state==1){
    new_state=2
  }else if(curr_state==2){
    new_state=1
  }
  
  return(new_state)
}



aca_gen_model=function(H,alpha,episode_len,total_trials,init_state,model){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  time_in_trial <-list()
  allpaths_aca_model2 <- matrix(0,total_trials,3)
  
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  time_in_trial[[episode]] <-vector()
  
  ##### Reward Matrix
  R=matrix(0,nrow=2,ncol=6)
  colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(R)<-c("E","I")
  R[1,4]=1
  R[2,4]=1
  
  #### Store Action Prob each trial
  probMatrix_aca=matrix(0,nrow=total_trials,ncol=12)
  colnames(probMatrix_aca)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  initState=init_state
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  curr_session=0
  startIndex_session=0
  avg_score=0
  score_episode=0
  episodeFin=0
  tau=0
  for(i in c(1:(total_trials-1))){

    if(length(actions[[episode]])==0){
      initState=S
    }
    
    ### Select new action 
    A=softmax_action_sel(H,S)
    
    allpaths_aca_model2[i,1]=A
    allpaths_aca_model2[i,2]=S
    allpaths_aca_model2[i,3]=R[S,A]

    ## Check if action earns a reward
    if(R[S,A]>0){
      score_episode=score_episode+1
    }else{
      score_episode=score_episode+0
    }
    
    actions[[episode]] <- append(actions[[episode]],unname(A))

    S_prime=getNextState_ACA_RL(S,A)  

    #print("Here")
    #actions <- c(actions,sprintf("S%i-P%i",S,A))
    states[[episode]] <- append(states[[episode]],unname(S))
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S_prime!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S_prime==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }

    if(episode>1){
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

      
      if(episodeFin==episode_len || i==init_state){
        avg_score = avg_score + (score_episode-avg_score)/episode;
        a<-actions[[episode]]
        s<-states[[episode]]
        t<-time_in_trial[[episode]]
        
        state1_idx=which(s==1)
        state2_idx=which(s==2)
        
        if(model==1){
          uniq_actions_s1 = unique(a[state1_idx])
          for(action_s1 in uniq_actions_s1){
            activity=length(which(a[state1_idx]==action_s1))/(length(state1_idx))
            H[1,action_s1]=H[1,action_s1]+alpha*(score_episode*activity)
          }
          
          
          uniq_actions_s2 = unique(a[state2_idx])
          for(action_s2 in uniq_actions_s2){
            activity=length(which(a[state2_idx]==action_s2))/(length(state2_idx))
            H[2,action_s2]=H[2,action_s2]+alpha*(score_episode*activity)
          }
          
        }else if(model==2){
          
          uniq_actions_s1 = unique(a[state1_idx])
          for(action_s1 in uniq_actions_s1){
            #activity=length(which(a[state1_idx]==action_s1))*1000/sum(t[state1_idx])
            #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity/Visits[1,uniq_actions_s1[ids]])
            activity=length(which(a[state1_idx]==action_s1))/(length(state1_idx))
            H[1,action_s1]=H[1,action_s1]+alpha*(score_episode-avg_score)*(1-as.numeric(softmax_aca2(action_s1,1,H)))*activity
            #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity)
          }
          
          setdiff_state1 = setdiff(c(1:6),uniq_actions_s1)
          for(action_s1 in setdiff_state1){
            H[1,action_s1]=H[1,action_s1]-alpha*(score_episode-avg_score)*(as.numeric(softmax_aca2(action_s1,1,H)))/(length(state1_idx))
          }
          
          
          uniq_actions_s2 = unique(a[state2_idx])
          for(action_s2 in uniq_actions_s2){
            #activity=length(which(a[state2_idx]==action_s2))*1000/sum(t[state2_idx])
            #H[2,uniq_actions_s2[ids]]=H[2,uniq_actions_s2[ids]]+alpha*(score_episode*activity/Visits[2,uniq_actions_s2[ids]])
            activity=length(which(a[state2_idx]==action_s2))/(length(state2_idx))
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
          }
        }
        
        ## reset rewards
        score_episode=0
        episodeFin=0
        episode = episode+1
        
        #print(sprintf("Updating episode to %i",episode))
        if(i <= (total_trials-1)){
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

  return(allpaths_aca_model2)
  
}

aca_mse_comp=function(H,alpha,episode_len,aca_sim,model){
  
  ## Start form state 1 = Box "E"
  print(sprintf("model=%i",model))
  
  actions <-list()
  states <-list()
  time_in_trial <-list()
  allpaths_aca_model2 <- matrix(0,length(aca_sim[,1]),3)
  
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  time_in_trial[[episode]] <-vector()
  
  ##### Reward Matrix
  R=matrix(0,nrow=2,ncol=6)
  colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(R)<-c("E","I")
  R[1,4]=1
  R[2,4]=1
  
  #### Store Action Prob each trial
  probMatrix_aca=matrix(0,nrow=length(aca_sim[,1]),ncol=12)
  colnames(probMatrix_aca)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  initState=as.numeric(aca_sim[1,2])
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  curr_session=0
  startIndex_session=0
  avg_score=0
  score_episode=0
  episodeFin=0
  tau=0
  for(i in c(1:(length(aca_sim[,1])-1))){
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    ### Select new action 
    A=aca_sim[i,1]
    
    allpaths_aca_model2[i,1]=A
    allpaths_aca_model2[i,2]=aca_sim[i,2]
    allpaths_aca_model2[i,3]=R[S,A]
    
    
    ## Check if action earns a reward
    if(R[S,A]>0){
      score_episode=score_episode+1
    }else{
      score_episode=score_episode+0
    }
    
    actions[[episode]] <- append(actions[[episode]],unname(A))
    
    S_prime=aca_sim[(i+1),2]
    
    #print("Here")
    #actions <- c(actions,sprintf("S%i-P%i",S,A))
    states[[episode]] <- append(states[[episode]],unname(S))
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S_prime!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S_prime==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }
    
    if(episode>1){
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
      #print(sprintf("episodeFin=%i, episode_len=%f",episodeFin,episode_len))
      
      if(episodeFin==floor(episode_len) || i==length(aca_sim[,1])){
        #print(sprintf("Inside credit update"))
        avg_score = avg_score + (score_episode-avg_score)/episode;
        a<-actions[[episode]]
        s<-states[[episode]]
        t<-time_in_trial[[episode]]
        
        state1_idx=which(s==1)
        state2_idx=which(s==2)
        
        if(model==1){
          #print(sprintf("Model selected=1"))
          uniq_actions_s1 = unique(a[state1_idx])
          for(action_s1 in uniq_actions_s1){
            activity=length(which(a[state1_idx]==action_s1))/(length(state1_idx))
            H[1,action_s1]=H[1,action_s1]+alpha*(score_episode*activity)
          }
          
          
          uniq_actions_s2 = unique(a[state2_idx])
          for(action_s2 in uniq_actions_s2){
            activity=length(which(a[state2_idx]==action_s2))/(length(state2_idx))
            H[2,action_s2]=H[2,action_s2]+alpha*(score_episode*activity)
          }
          
        }else if(model==2){
          #print(sprintf("Model selected=2"))
          uniq_actions_s1 = unique(a[state1_idx])
          for(action_s1 in uniq_actions_s1){
            #activity=length(which(a[state1_idx]==action_s1))*1000/sum(t[state1_idx])
            #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity/Visits[1,uniq_actions_s1[ids]])
            activity=length(which(a[state1_idx]==action_s1))/(length(state1_idx))
            H[1,action_s1]=H[1,action_s1]+alpha*(score_episode-avg_score)*(1-as.numeric(softmax_aca2(action_s1,1,H)))*activity
            #H[1,uniq_actions_s1[ids]]=H[1,uniq_actions_s1[ids]]+alpha*(score_episode*activity)
          }
          
          setdiff_state1 = setdiff(c(1:6),uniq_actions_s1)
          for(action_s1 in setdiff_state1){
            H[1,action_s1]=H[1,action_s1]-alpha*(score_episode-avg_score)*(as.numeric(softmax_aca2(action_s1,1,H)))/(length(state1_idx))
          }
          
          
          uniq_actions_s2 = unique(a[state2_idx])
          for(action_s2 in uniq_actions_s2){
            #activity=length(which(a[state2_idx]==action_s2))*1000/sum(t[state2_idx])
            #H[2,uniq_actions_s2[ids]]=H[2,uniq_actions_s2[ids]]+alpha*(score_episode*activity/Visits[2,uniq_actions_s2[ids]])
            activity=length(which(a[state2_idx]==action_s2))/(length(state2_idx))
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
        }
        
        
        ## reset rewards
        score_episode=0
        episodeFin=0
        episode = episode+1
        
        #print(sprintf("Updating episode to %i",episode))
        if(i <= (length(aca_sim[,1])-1)){
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
  
  mse=getaca_MSE(probMatrix_aca,aca_sim)
  print(sprintf("MSE is:%f",mse))
  return(mse)
  
}


softmax_action_sel=function(H,state){
  
  x1=exp(H[state,1])
  x2=exp(H[state,2])
  x3=exp(H[state,3])
  x4=exp(H[state,4])
  x5=exp(H[state,5])
  x6=exp(H[state,6])
  
  p1=x1/(x1+x2+x3+x4+x5+x6)
  p2=x2/(x1+x2+x3+x4+x5+x6)
  p3=x3/(x1+x2+x3+x4+x5+x6)
  p4=x4/(x1+x2+x3+x4+x5+x6)
  p5=x5/(x1+x2+x3+x4+x5+x6)
  p6=x6/(x1+x2+x3+x4+x5+x6)
  
  action = sample(c(1:6),size=1,prob=c(p1,p2,p3,p4,p5,p6))
  return(action)
  
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
aca_negLogLik1 <- function(par,allpaths,model,sim) {
  
  alpha <- par[1]
  epsLim <- 2+(par[2]*28)
  #print(sprintf("epsLim=%f",epsLim))
  H <- matrix(0,2,6)
  # H[1,1:6]<-par[3:8]
  # H[2,1:6] <- par[9:14]
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- aca_mle_lik(allpaths,alpha,epsLim,H,model,sim)
  #negLogLik <- -sum(log(lik))
  if(is.infinite(lik)){
    return(1000000)
  }else{
    return(as.numeric(lik))
  }
  
}

sarsa_negLogLik <- function(par,allpaths,sim) {
  
  alpha <- par[1]
  gamma <- par[2]
  lambda <- par[3]
  #print(sprintf("epsLim=%f",epsLim))
  Q_vals <- matrix(0,2,6)
  # H[1,1:6]<-par[3:8]
  # H[2,1:6] <- par[9:14]
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- sarsa_mle(allpaths,alpha,gamma,lambda,Q_vals,sim)
  #negLogLik <- -sum(log(lik))
  if(is.infinite(lik)){
    return(1000000)
  }else{
    return(as.numeric(lik))
  }
  
}

# acamse_negLogLik <- function(par,allpaths,model) {
#   
#   alpha <- par[1]
#   epsLim <- par[2]
#   H <- matrix(0,2,6)
#   H[1,1:6]<-par[3:8]
#   H[2,1:6] <- par[9:14]
#   #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
#   if(model==1){
#     lik <- aca_mle_model1(allpaths,alpha,epsLim,H)
#   }else if(model==2){
#     lik <- aca_mle_model2(allpaths,alpha,epsLim,H)
#   }
#   
#   #negLogLik <- -sum(log(lik))
#   if(is.infinite(lik)){
#     return(1000000)
#   }else{
#     return(as.numeric(lik))
#   }
#   
# }




getaca_MSE=function(probMatrix_aca,allpaths_aca){
  
  start_index = length(allpaths_aca[,1])
  start_index = round(start_index/2)
  max_index=length(allpaths_aca[,1])-start_index
  mseMatrix=matrix(0,nrow=12,ncol=max_index)
  start_index= start_index+1
  rownames(mseMatrix)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  
  #len1<-length(which(as.numeric(allpaths[start_index:length(allpaths[,1]),5])==1))
  #len2<-length(which(as.numeric(allpaths[start_index:length(allpaths[,1]),5])==2))
  trial=1
  for(i in start_index:length(allpaths_aca[,1])){
    action=as.numeric(allpaths_aca[i,1])
    if(allpaths_aca[i,2]==1){
      mseMatrix[1,trial]=mseMatrix[1,trial]+(probMatrix_aca[i,1]-(1==action))^2
      mseMatrix[2,trial]=mseMatrix[2,trial]+(probMatrix_aca[i,2]-(2==action))^2
      mseMatrix[3,trial]=mseMatrix[3,trial]+(probMatrix_aca[i,3]-(3==action))^2
      mseMatrix[4,trial]=mseMatrix[4,trial]+(probMatrix_aca[i,4]-(4==action))^2
      mseMatrix[5,trial]=mseMatrix[5,trial]+(probMatrix_aca[i,5]-(5==action))^2
      mseMatrix[6,trial]=mseMatrix[6,trial]+(probMatrix_aca[i,6]-(6==action))^2
    }else if(allpaths_aca[i,2]==2){
      mseMatrix[7,trial]=mseMatrix[7,trial]+(probMatrix_aca[i,7]-(7==action))^2
      mseMatrix[8,trial]=mseMatrix[8,trial]+(probMatrix_aca[i,8]-(8==action))^2
      mseMatrix[9,trial]=mseMatrix[9,trial]+(probMatrix_aca[i,9]-(9==action))^2
      mseMatrix[10,trial]=mseMatrix[10,trial]+(probMatrix_aca[i,10]-(10==action))^2
      mseMatrix[11,trial]=mseMatrix[11,trial]+(probMatrix_aca[i,11]-(11==action))^2
      mseMatrix[12,trial]=mseMatrix[12,trial]+(probMatrix_aca[i,12]-(12==action))^2
    }
    trial=trial+1
  }
  # mseMatrix[1:6,]=mseMatrix[1:6,]/len1
  # mseMatrix[7:12,]=mseMatrix[7:12,]/len2
  
  total_mse=sum(rowSums(mseMatrix))/max_index
  return(total_mse)
}
