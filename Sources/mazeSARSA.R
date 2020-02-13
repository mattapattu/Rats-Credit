

R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1



### Action = Path
## State = E or I

sarsa_smax2=function(Q,E,alpha,epsilon,gamma,lambda,allpaths){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  
  ses_max = as.numeric(allpaths[length(allpaths[,1]),"Session"])
  probMatrix_sarsa=matrix(0,ses_max,13)
  colnames(probMatrix_sarsa)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6","Session")

  
  ## One episode = all actions to make a loop around maze, 
  ## Start at E -> visit I -> return to E
  
  changeState = F
  returnToInitState = F
  
  if(grepl("^.*e$",allpaths[1,1])){
    S = 1
  }else if(grepl("^.*i$",allpaths[1,1])){
    S = 2
  }else{
    print("Unknown intial state. Check")
  }
  E=E*0
  A = as.numeric(allpaths[1,3])
  episode=1
  actions[[episode]] <- vector()
  initState = S
  startIndex_session=0
  curr_session=0
  
  for(step in 2:(length(allpaths[,1])-1)){
    
    #print(sprintf("episode=%i",episode))
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    pos_trial_t<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[pos_trial_t,"Reward"]))
    reward=0
    if(R>0){
      reward=1
    }else{
      reward=0
    }
    
    ## Next state is the box where current action ends
    S_prime=getNextState(allpaths,i)
    A_prime=as.numeric(allpaths[i+1,3])
    
    E[S,A]=E[S,A]+1
    delta=reward+(gamma* Q[S_prime,A_prime]) - Q[S,A]
    Q=Q+ (alpha*delta*E)
    E=E*gamma*lambda
    
    #print(sprintf("Current state - %i, Action Selected - %i",S,A))
    if(A == 4 & S == 1){
      actions[[episode]] <- append(actions[[episode]],51)
    }else if(A == 4 & S == 2){
      actions[[episode]] <- append(actions[[episode]],49)
    }else{
      actions[[episode]] <- append(actions[[episode]],unname(A))
    }
    #actions <- c(actions,sprintf("S%i-P%i",S,A))
    states <- c(states,S)
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S_prime!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S_prime==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }
    
    if(step>1){
     
      probMatrix_sarsa[i,13]=ses
      if(S==1){
        probMatrix_sarsa[i,7:12]=0
        for(act in 1:6){
          x<-mpfr(softmax_sarsa(act,1,Q),128)
          probMatrix_sarsa[i,(act)]=as.numeric(x)
        }
      }else if(S==2){
        probMatrix_sarsa[i,1:6]=0
        for(act in 1:6){
          x<-mpfr(softmax_sarsa(act,2,Q),128)
          probMatrix_sarsa[i,(6+act)]=as.numeric(x)
        }
      }
    }
    
    S=S_prime
    A=A_prime
    
    if(returnToInitState){
      
      changeState = F
      returnToInitState = F
      initState=S
      E=E*0
      episode  = episode+1
      #print(sprintf("Updating episode to %i",episode))
      if(step <= (length(allpaths[,1])-1)){
        actions[[episode]] <- vector()
      }
    }
    
  }
  probMat_sarsa_res=getStatsOfLastSession2_sarsa(probMatrix_sarsa)
  return(probMat_sarsa_res)
}

getNextState_sarsa=function(curr_state,action){
  if(action == 5){
    new_state=curr_state
  }else if(curr_state==1){
    new_state=2
  }else if(curr_state==2){
    new_state=1
  }
  
  return(new_state)
}


# getStatsOfLastSession_sarsa2=function(probMatrix_sarsa,curr_session,allpaths){
#   mat<-matrix(0,0,2)
#   probMatrix_sarsa <- cbind(probMatrix_sarsa,0)
#   colIndex=length(probMatrix_sarsa[1,])
#   
#   print(sprintf("session=%f",colIndex))
#   
#   pos_ses<-which(as.numeric(allpaths[,2])==curr_session)
#   all_actions<-as.numeric(allpaths[pos_ses,3])
#   all_states<-as.numeric(allpaths[pos_ses,5])
#   
#   len1<-length(which(all_states==1))
#   len2<-length(which(all_states==2))
#   mat<-matrix(0,length(all_actions),2)
#   mat[,1]<-all_actions
#   mat[,2]<-all_states
#   
#   probMatrix_sarsa[1,colIndex]=length(which(mat[,1]==1 & mat[,2]==1))/len1
#   probMatrix_sarsa[2,colIndex]=length(which(mat[,1]==2 & mat[,2]==1))/len1
#   probMatrix_sarsa[3,colIndex]=length(which(mat[,1]==3 & mat[,2]==1))/len1
#   probMatrix_sarsa[4,colIndex]=length(which(mat[,1]==51 & mat[,2]==1))/len1
#   probMatrix_sarsa[5,colIndex]=length(which(mat[,1]==5 & mat[,2]==1))/len1
#   probMatrix_sarsa[6,colIndex]=length(which(mat[,1]==6 & mat[,2]==1))/len1
#   probMatrix_sarsa[7,colIndex]=length(which(mat[,1]==1 & mat[,2]==2))/len2
#   probMatrix_sarsa[8,colIndex]=length(which(mat[,1]==2 & mat[,2]==2))/len2
#   probMatrix_sarsa[9,colIndex]=length(which(mat[,1]==3 & mat[,2]==2))/len2
#   probMatrix_sarsa[10,colIndex]=length(which(mat[,1]==49 & mat[,2]==2))/len2
#   probMatrix_sarsa[11,colIndex]=length(which(mat[,1]==5 & mat[,2]==2))/len2
#   probMatrix_sarsa[12,colIndex]=length(which(mat[,1]==6 & mat[,2]==2))/len2
#   
#   return(probMatrix_sarsa)
# }

getStatsOfLastSession2_sarsa=function(probMatrix_sarsa){
  
  ses_max = as.numeric(probMatrix_sarsa[(length(probMatrix_sarsa[,13])-1),13])
  probMat_res_sarsa=matrix(0,nrow=12,ncol=ses_max)
  rownames(probMat_res_sarsa)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  
  for(ses in 1:ses_max){
    pos_ses<-which(as.numeric(probMatrix_sarsa[,13])==ses)
    if(isempty(pos_ses)){
      probMat_sarsa[,ses]=NA
      next
    }else{
      
      probMat_res_sarsa[1,ses]=sum(probMatrix_sarsa[pos_ses,1])/length(which(probMatrix_sarsa[pos_ses,1]!=0))
      probMat_res_sarsa[2,ses]=sum(probMatrix_sarsa[pos_ses,2])/length(which(probMatrix_sarsa[pos_ses,2]!=0))
      probMat_res_sarsa[3,ses]=sum(probMatrix_sarsa[pos_ses,3])/length(which(probMatrix_sarsa[pos_ses,3]!=0))
      probMat_res_sarsa[4,ses]=sum(probMatrix_sarsa[pos_ses,4])/length(which(probMatrix_sarsa[pos_ses,4]!=0))
      probMat_res_sarsa[5,ses]=sum(probMatrix_sarsa[pos_ses,5])/length(which(probMatrix_sarsa[pos_ses,5]!=0))
      probMat_res_sarsa[6,ses]=sum(probMatrix_sarsa[pos_ses,6])/length(which(probMatrix_sarsa[pos_ses,6]!=0))
      probMat_res_sarsa[7,ses]=sum(probMatrix_sarsa[pos_ses,7])/length(which(probMatrix_sarsa[pos_ses,7]!=0))
      probMat_res_sarsa[8,ses]=sum(probMatrix_sarsa[pos_ses,8])/length(which(probMatrix_sarsa[pos_ses,8]!=0))
      probMat_res_sarsa[9,ses]=sum(probMatrix_sarsa[pos_ses,9])/length(which(probMatrix_sarsa[pos_ses,9]!=0))
      probMat_res_sarsa[10,ses]=sum(probMatrix_sarsa[pos_ses,10])/length(which(probMatrix_sarsa[pos_ses,10]!=0))
      probMat_res_sarsa[11,ses]=sum(probMatrix_sarsa[pos_ses,11])/length(which(probMatrix_sarsa[pos_ses,11]!=0))
      probMat_res_sarsa[12,ses]=sum(probMatrix_sarsa[pos_ses,12])/length(which(probMatrix_sarsa[pos_ses,12]!=0))
    }
  }
  probMat_res_sarsa <- probMat_res_sarsa[ , !apply(is.na(probMat_res_sarsa), 2, all)]
  return(probMat_res_sarsa)
}



softmax_sarsa=function(A,S,Q){
  x1 <- mpfr(exp(Q[S,A]), precBits = 128)
  x2 <- mpfr(exp(Q[S,1]), precBits = 128)
  x3 <- mpfr(exp(Q[S,2]), precBits = 128)
  x4 <- mpfr(exp(Q[S,3]), precBits = 128)
  x5 <- mpfr(exp(Q[S,4]), precBits = 128)
  x6 <- mpfr(exp(Q[S,5]), precBits = 128)
  x7 <- mpfr(exp(Q[S,6]), precBits = 128)
  pr_A=(x1)/((x2)+(x3)+(x4)+(x5)+(x6)+(x7))
  # if(pr_A==1){
  #   stop(sprintf("pr_A is 1 = %.20f, Action=%i,State=%i, H=%s",pr_A,A,S,paste(as.numeric(x1),as.numeric(x2),as.numeric(x3),as.numeric(x4),as.numeric(x5),as.numeric(x6), sep=" ")))
  # }else if(pr_A==0){
  #   stop(sprintf("pr_A is 1 = %.20f, Action=%i,State=%i, H=%s",pr_A,A,S,paste(as.numeric(x1),as.numeric(x2),as.numeric(x3),as.numeric(x4),as.numeric(x5),as.numeric(x6), sep=" ")))
  # }
  return(pr_A)
}

softmax_policy_sarsa=function(Q,state){
  
  x1=exp(Q[state,1])
  x2=exp(Q[state,2])
  x3=exp(Q[state,3])
  x4=exp(Q[state,4])
  x5=exp(Q[state,5])
  x6=exp(Q[state,6])
  
  p1=x1/(x1+x2+x3+x4+x5+x6)
  p2=x2/(x1+x2+x3+x4+x5+x6)
  p3=x3/(x1+x2+x3+x4+x5+x6)
  p4=x4/(x1+x2+x3+x4+x5+x6)
  p5=x5/(x1+x2+x3+x4+x5+x6)
  p6=x6/(x1+x2+x3+x4+x5+x6)
  
  action = sample(c(1:6),size=1,prob=c(p1,p2,p3,p4,p5,p6))
  return(action)
  
}


# 
# ### Init Q
# Q = matrix(0,nrow=2,ncol=6)
# colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
# rownames(Q)<-c("E","I")
# Q[1,1]=0.875
# Q[2,1]=0.875
# 
# E=matrix(0,nrow=2,ncol=6)
# colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
# rownames(E)<-c("E","I")
# 
# ## Session 1
# alpha=0.2335897
# gamma=0.6011665
# epsilon=0.4385508
# lambda=0.12575
# max_steps=1380
# #debug(epsilon_greedy)
# #debug(getNextState)
# debug(sarsa)
# Q1=sarsa(Q,E,alpha,max_steps,epsilon,gamma,lambda,sessions)