

R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1



### Action = Path
## State = E or I

sarsa_smax=function(Q,E,alpha,max_steps,epsilon,gamma,lambda,sessions){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  
  probMatrix_sarsa=matrix(0,12,0)
  rownames(probMatrix_sarsa)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  
  ## One episode = all actions to make a loop around maze, 
  ## Start at E -> visit I -> return to E
  
  changeState = F
  returnToInitState = F
  
  S=1
  E=E*0
  A=softmax_policy_sarsa(Q,S)
  episode=1
  actions[[episode]] <- vector()
  initState = S
  startIndex_session=0
  session=0
  
  for(step in 1:max_steps){

    #print(sprintf("episode=%i",episode))
    r=R[S,A]
    S_prime=getNextState_sarsa(S,A)
    A_prime=softmax_policy_sarsa(Q,S_prime)
    E[S,A]=E[S,A]+1
    delta=r+(gamma* Q[S_prime,A_prime]) - Q[S,A]
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
    
    if(step %in% sessions){
      #print(sprintf("Start new session"))
      #print(sprintf("New session, step=%i",step))
      if(step>1){
        #debug(getStatsOfLastSession_sarsa)
        probMatrix_sarsa=getStatsOfLastSession_sarsa(probMatrix_sarsa,startIndex_session,session,actions,states)
      }
      
      session=session+1
      startIndex_session=i
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
      if(step <= max_steps-1){
        actions[[episode]] <- vector()
      }
      
    }
    
  }

  #print(sprintf("last step=%i",step))
  #print(unlist(states))
  # a=as.data.frame(actions)
  # colnames(a)=NULL
  # rownames(a)=NULL
  #print(actions)
  #capture.output(print(actions), file = "/home/ajames/intership2/actions_SARSA.txt")
  #capture.output(print(actions), file = "/home/ajames/intership2/states_SARSA.txt")
  #print(probMatrix_sarsa)
  return(probMatrix_sarsa)
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


getStatsOfLastSession_sarsa=function(probMatrix_sarsa,session_start,session,actions,states){
  mat<-matrix(0,0,2)
  probMatrix_sarsa <- cbind(probMatrix_sarsa,0)
  colIndex=length(probMatrix_sarsa[1,])
  print(sprintf("session=%f",colIndex))
  all_actions<-unlist(actions)
  all_actions<-all_actions[session_start:(length(all_actions)-1)]
  
  all_states<-unlist(states)
  all_states<-all_states[session_start:(length(all_states)-1)]
  len1<-length(which(all_states==1))
  len2<-length(which(all_states==2))
  mat<-matrix(0,length(all_actions),2)
  mat[,1]<-all_actions
  mat[,2]<-all_states
  
  probMatrix_sarsa[1,colIndex]=length(which(mat[,1]==1 & mat[,2]==1))/len1
  probMatrix_sarsa[2,colIndex]=length(which(mat[,1]==2 & mat[,2]==1))/len1
  probMatrix_sarsa[3,colIndex]=length(which(mat[,1]==3 & mat[,2]==1))/len1
  probMatrix_sarsa[4,colIndex]=length(which(mat[,1]==51 & mat[,2]==1))/len1
  probMatrix_sarsa[5,colIndex]=length(which(mat[,1]==5 & mat[,2]==1))/len1
  probMatrix_sarsa[6,colIndex]=length(which(mat[,1]==6 & mat[,2]==1))/len1
  probMatrix_sarsa[7,colIndex]=length(which(mat[,1]==1 & mat[,2]==2))/len2
  probMatrix_sarsa[8,colIndex]=length(which(mat[,1]==2 & mat[,2]==2))/len2
  probMatrix_sarsa[9,colIndex]=length(which(mat[,1]==3 & mat[,2]==2))/len2
  probMatrix_sarsa[10,colIndex]=length(which(mat[,1]==49 & mat[,2]==2))/len2
  probMatrix_sarsa[11,colIndex]=length(which(mat[,1]==5 & mat[,2]==2))/len2
  probMatrix_sarsa[12,colIndex]=length(which(mat[,1]==6 & mat[,2]==2))/len2
  
  return(probMatrix_sarsa)
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