

R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1



### Action = Path
## State = E or I

sarsa=function(Q,E,alpha,max_steps,epsilon,gamma,lambda){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  
  
  ## One episode = all actions to make a loop around maze, 
  ## Start at E -> visit I -> return to E
  
  changeState = F
  returnToInitState = F
  
  S=1
  E=E*0
  A=epsilon_greedy(Q,epsilon,S)
  episode=1
  actions[[episode]] <- vector()
  initState = S

  
  for(step in 1:max_steps-1){

    #print(sprintf("episode=%i",episode))
    r=R[S,A]
    S_prime=getNextState(S,A)
    A_prime=epsilon_greedy(Q,epsilon,S_prime)
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
    
    S=S_prime
    A=A_prime
    
    if(returnToInitState){
      
      changeState = F
      returnToInitState = F
      initState=S
      #E=E*0
      episode  = episode+1
      #print(sprintf("Updating episode to %i",episode))
      if(step < max_steps-1){
        actions[[episode]] <- vector()
      }
      
    }
    
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

epsilon_greedy=function(Q,epsilon,state){
  U=runif(1,0,1)
  action =0
  if(U <= epsilon){
    action = sample(c(1:6), 1)
    #print(sprintf("Selecting random action - %i in state - %i",action,state))
  }else{
    action = which.max(Q[state,])
    #print(sprintf("Selecting greedy action - %i in state - %i",action,state))
  }
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
lambda=0.8
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
lambda=0.8
#debug(epsilon_greedy)
#debug(getNextState)
#debug(sarsa)
Q2=sarsa(Q1,E,alpha,max_steps,epsilon,gamma,lambda)

### Session 3
alpha=0.2
max_steps=100
epsilon=0.2
gamma=0.8
lambda=0.9
#debug(epsilon_greedy)
#debug(getNextState)
#debug(Qlearn)
Q3=sarsa(Q2,E,alpha,max_steps,epsilon,gamma,lambda)

# ### Session 4
alpha=0.4
max_steps=100
epsilon=0.1
gamma=0.8
lambda=0.9
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
