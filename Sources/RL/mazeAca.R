

R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1



### Action = Path
## State = E or I

aca_rl=function(H,Visits,Scores,alpha,n,max_steps){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  
  QProb<-mpfrArray(1, prec = 128, dim = 1)
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  
  initState=1
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  for(i in c(1:max_steps)){
    
    
    if(step<= exploration_end){
      A=sample(c(1:6),size=1)
    }else{
      A=softmax_policy(H,S)
    }
    ## Update S based on action A
    S=getNextState(S,A)  
    
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    if(R[S,A]>0){
      reward=reward+1
    }else{
      reward=reward+0
    }
    
    if(A == 4 & S == 1){
      actions[[episode]] <- append(actions[[episode]],51)
    }else if(A == 4 & S == 2){
      actions[[episode]] <- append(actions[[episode]],49)
    }else{
      actions[[episode]] <- append(actions[[episode]],unname(A))
    }
    #actions <- c(actions,sprintf("S%i-P%i",S,A))
    states[[episode]] <- append(states[[episode]],unname(S))
    #print(sprintf("Current state = %i, Action = %i", S,A))
    if(S!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }
    
    
    ## Check if episode ended
    if(returnToInitState){
      changeState = F
      returnToInitState = F
      
      a<-actions[[episode]]
      s<-states[[episode]]
      
      for(state in 1:2){
        for(action in c(1,2,3,49,51,5,6)){
          
          ## If S,A is visited in the episode
          if(any(s[which(a %in% action)]==state)){
            
            if(action==49|action==51){
              action=4
            }
            
            
            if(i<=n){
              ## During exploration
              ### Credit = Score * activity
              ## Activity of (A,S) = #Nb of times Action A is taken in State S/ # Nb of times State S is visited
              #time_spent_in_trial = as.numeric(enreg[[ses]]$POS[pos_trial_t[length(pos_trial_t)],1]) - as.numeric(enreg[[ses]]$POS[pos_trial_t[1],1])
              H[state,action]=reward
              if(is.nan(H[state,action])){
                stop("H[state,action] is NaN")
              }
            }else{
              ## After exploration
              ## expected_score = E(Score of Action A in State S) = #Total Score of Action A in State S before current trial/#Nb of times Action A is taken in State S
              
              if(Visits[state,action]==0){
                expected_score=0
              }else{
                expected_score = Score[state,action]/Visits[state,action]
              }
              H[state,action]=H[state,action]+alpha*(reward-expected_score)*(1-as.numeric(softmax(action,state,H)))
              if(is.nan(H[state,action])){
                stop("H[state,action] is NaN")
              }
            }
            
            ## Update Score of current Action A in current state S
            Score[state,action]=Score[state,action]+reward
            Visits[state,action]=Visits[state,action]+1
            
          }
          
          
          ## If S,A is not visited in the episode
          else{
            if(action==49|action==51){
              action=4
            }
            
            if(i<=n){
              ## During exploration
              ### No Credit update as reward=0 during exploration
            }else{
              ## After exploration
              
              if(Visits[state,action]==0){
                expected_score=0
              }else{
                expected_score = Score[state,action]/Visits[state,action]
              }
              
              H[state,action]=H[state,action]-alpha*(reward-expected_score)*(as.numeric(softmax(action,state,H)))
              
            }
          }
        }
        
      }
      ## reset rewards
      reward=0
      episode = episode+1
      
      #print(sprintf("Updating episode to %i",episode))
      if(i < length(allpaths[,1])-1){
        actions[[episode]] <- vector()
        states[[episode]] <- vector()
      }
    }
    ### Start new episode
    
    
    if(i<=n){
      QProb<-c(QProb,mpfr(1/6,128))
    }else{
      QProb<-c(QProb,mpfr(softmax(A,S,H),128))
    }
    #print(sprintf("QProb=%f",QProb[i]))
    
    ## i+1 is used as I am adding "1" to QProb when initializing
    # if(is.nan(QProb[i+1])){
    #   stop(sprintf("i=%i,Qprob is 1 = %.20f, Action=%i,State=%i, Matrix = %s",i,QProb[i+1],A,S,paste(as.vector(H), collapse=" ")))
    #   print(sprintf('%.20f',QProb[i+1]))
    #   print(sprintf("Action=%i,State=%i",A,S))
    #   print(H)
    # }else if((QProb[i+1]==1)){
    #   stop(sprintf("i=%i,alpha=%f,n=%f,Qprob is 1 = %.20f, Action=%i,State=%i, Matrix = %s",i,alpha,n,QProb[i+1],A,S,paste(as.vector(H), collapse=" ")))
    #   print(sprintf("Qprob is 1 = %.20f, Action=%i,State=%i, Matrix = %s",QProb[i+1],A,S,paste(as.vector(H), collapse=" ")))
    #   print(sprintf("Action=%i,State=%i",A,S))
    #   print(H)
    # }else if((QProb[i+1]==0)){
    #   stop(sprintf("i=%i,Qprob is 1 = %.20f, Action=%i,State=%i, Matrix = %s",i,QProb[i+1],A,S,paste(as.vector(H), collapse=" ")))
    #   print(sprintf('%.20f',QProb[i+1]))
    #   print(sprintf("Action=%i,State=%i",A,S))
    #   print(H)
    # }
    
  }


  
  print(unlist(states))
  # a=as.data.frame(actions)
  # colnames(a)=NULL
  # rownames(a)=NULL
  print(actions)
  print(Q)
  return(Q)
}

getNextState=function(curr_state,action){
  if(action == 5){
    new_state=curr_state
  }else if(curr_state==1){
    new_state=2
  }else if(curr_state==2){
    new_state=1
  }
  
  return(new_state)
}


softmax_policy=function(H,state){
  
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
### Init Q
Q = matrix(0,nrow=2,ncol=6)
colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(Q)<-c("E","I")
Q[1,1]=0.5
Q[2,1]=0.5

E=matrix(0,nrow=2,ncol=6)
colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(E)<-c("E","I")

## Session 1
alpha=0.1
max_steps=30
epsilon=0.1
#gamma=0.3
gamma=0.9
lambda=1
#debug(epsilon_greedy)
#debug(getNextState)
#debug(sarsa)
Q1=sarsa(Q,E,alpha,max_steps,epsilon,gamma,lambda)

## Session 2
alpha=0.1
max_steps=80
epsilon=0.2
#gamma=0.3
gamma=0.7
lambda=1
#debug(epsilon_greedy)
#debug(getNextState)
#debug(sarsa)
Q2=sarsa(Q1,E,alpha,max_steps,epsilon,gamma,lambda)

### Session 3
alpha=0.2
max_steps=100
epsilon=0.2
gamma=0.8
lambda=1
#debug(epsilon_greedy)
#debug(getNextState)
#debug(Qlearn)
Q3=sarsa(Q2,E,alpha,max_steps,epsilon,gamma,lambda)

# ### Session 4
alpha=0.4
max_steps=100
epsilon=0.1
gamma=0.8
lambda=1
#debug(epsilon_greedy)
#debug(getNextState)
#debug(Qlearn)
Q4=sarsa(Q3,E,alpha,max_steps,epsilon,gamma,lambda)

# ## Session 2
# alpha=0.1
# max_steps=100
# epsilon=0.4
# gamma=0.8
# lambda=0.2
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q2=sarsa(Q1,E,alpha,max_steps,epsilon,gamma,lambda)
# # 
# # ### Session 3
# alpha=0.1
# max_steps=100
# epsilon=0.4
# gamma=0.8
# lambda=0.9
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q3=sarsa(Q2,E,alpha,max_steps,epsilon,gamma,lambda)
# # 
# # ### Session 4
# alpha=0.8
# max_steps=100
# epsilon=0.1
# gamma=0.8
# lambda=0.2
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q4=sarsa(Q3,E,alpha,max_steps,epsilon,gamma,lambda)
