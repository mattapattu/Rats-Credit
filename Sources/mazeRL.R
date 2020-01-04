

R=matrix(0,nrow=4,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E-E","E-I","I-I","I-E")
R[1,4]=1
R[2,4]=1
R[3,4]=1
R[4,4]=1


### Action = Path
## State = E or I

Qlearn=function(Q,E,alpha,max_steps,epsilon,gamma,lambda){
  
  ## Start form state 1 = Box "E"
  S=1
  A=0
  actions <-list()
  states <-list()
  
  for(i in 1:max_steps){
    
    print(i)
    
    A_prev=A
    A=epsilon_greedy(epsilon,S)
    actions <- list.append(actions,A)
    states <- list.append(states,S)
    S_prev=S
    S=getNextState(S,A)
    ## Reset eligibility trace to zero after one episode == after one reward
    if(R[S,A]==1){
      E=matrix(0,nrow=4,ncol=6)
    }
    E = E*gamma*lambda
    E[S,A]=E[S,A]+1
    #print(sprintf("New state = %s,Previous State = %s, action selected = %s",S,S_prev,A))
    
    if(i>1){
      
      delta= R[S,A]+ (gamma* max(Q[S,]) - Q[S_prev,A_prev])

      #Q[S_prev,A_prev]=Q[S_prev,A_prev]+alpha*delta
      Q=Q+alpha*delta*E
      #Q[S_prev,A_prev]=Q[S_prev,A_prev]+alpha*( R[S,A]+ (gamma* Q[S,A] - Q[S_prev,A_prev]) )
    }
    
  }
  
  print(unlist(states))
  print(unlist(actions))
  print(Q)
  return(Q)
}

getNextState=function(curr_state,action){
  if(((curr_state==1)|(curr_state==3)) && action == 5){
    new_state=curr_state
  }else if(curr_state==1){
    new_state=2
  }else if(curr_state==2 && action == 5){
    new_state=3
  }else if(curr_state==2){
    new_state=4
  }else if(curr_state==3 && action == 5){
    new_state=3
  }else if(curr_state==3){
    new_state=4
  }else if(curr_state==4 && action == 5){
    new_state=1
  }else if(curr_state==4){
    new_state=2
  }
  
  return(new_state)
}

epsilon_greedy=function(epsilon,state){
  U=runif(1,0,1)
  action =0
  if(U <= epsilon){
    action = sample(1:6, 1)
  }else{
    action = which.max(Q[state,])
  }
  return(action)
}
### Init Q
Q = matrix(0,nrow=4,ncol=6)
colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(Q)<-c("E-E","E-I","I-I","I-E")

E=matrix(0,nrow=4,ncol=6)
colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(E)<-c("E-E","E-I","I-I","I-E")

## Session 1
alpha=0.4
max_steps=60
epsilon=0.9
gamma=0.8
lambda=0.
#debug(epsilon_greedy)
#debug(getNextState)
#debug(Qlearn)
Q1=Qlearn(Q,E,alpha,max_steps,epsilon,gamma)

# ## Session 2
# alpha=0.8
# max_steps=100
# epsilon=0.4
# gamma=0.8
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q2=Qlearn(Q1,alpha,max_steps,epsilon,gamma)
# 
# ### Session 3
# alpha=0.8
# max_steps=100
# epsilon=0.5
# gamma=0.8
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q3=Qlearn(Q2,alpha,max_steps,epsilon,gamma)
# 
# ### Session 4
# alpha=0.8
# max_steps=100
# epsilon=0.5
# gamma=0.9
# #debug(epsilon_greedy)
# #debug(getNextState)
# #debug(Qlearn)
# Q4=Qlearn(Q3,alpha,max_steps,epsilon,gamma)
