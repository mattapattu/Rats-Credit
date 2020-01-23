
library(Rmpfr)

R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1



### Action = Path
## State = E or I

aca_rl=function(H,Visits,Scores,alpha,n,max_steps,sessions){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  probMatrix_aca=matrix(0,12,max_steps)
  rownames(probMatrix_aca)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  
  initState=1
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  
  for(i in c(1:max_steps)){
    
    if(i<= n){
      A=sample(c(1:6),size=1)
    }else{
      A=softmax_policy(H,S)
    }
    ## Update S based on action A
    S_prime=getNextState(S,A)  
    
    
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
    if(S_prime!=initState){
      changeState = T
      #print(sprintf("Setting changeState to T"))
    }else if(S_prime==initState && changeState){
      returnToInitState = T
      #print(sprintf("Setting returnToInitState to T"))
    }
    
 
    
    
    ## Check if episode ended
    if(returnToInitState){
      changeState = F
      returnToInitState = F
      
      probMatrix_aca[1:6,max_steps]=H[1,]
      probMatrix_aca[7:12,max_steps]=H[2,]
      
      a<-actions[[episode]]
      s<-states[[episode]]
      
      avg_score = avg_score + (score_episode-avg_score)/episode
      for(state in 1:2){
        for(action in c(1,2,3,49,51,5,6)){
          
          if(state==1 && action ==49){
            next
          }else if(state==2 && action ==51){
            next
          }
          
          ## If S,A is visited in the episode
          if(any(s[which(a %in% action)]==state)){
            
            if(action==49|action==51){
              action=4
              
            }
            
            if(Visits[state,action]==0){
              expected_score=0
            }else{
              
            }
            H[state,action]=H[state,action]+alpha*(score_episode-avg_score)*(1-as.numeric(softmax(action,state,H)))
            
            if(is.nan(H[state,action])){
              stop("H[state,action] is NaN")
            }
            
          }
          ## If S,A is not visited in the episode
          else{
            if(action==49|action==51){
              action=4
            }
            H[state,action]=H[state,action]-alpha*(score_episode-avg_score)*(as.numeric(softmax(action,state,H)))
            
          }
        }
      }
      ## reset rewards
      score_episode=0
      episode = episode+1
      
      #print(sprintf("Updating episode to %i",episode))
      if(i < length(allpaths[,1])-1){
        actions[[episode]] <- vector()
        states[[episode]] <- vector()
        activations[[episode]] <- vector()
      }
   
    }
    ### End of episode checkd
    S=S_prime
    

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

softmax=function(A,S,H){
  x1 <- mpfr(exp(H[S,A]), precBits = 128)
  x2 <- mpfr(exp(H[S,1]), precBits = 128)
  x3 <- mpfr(exp(H[S,2]), precBits = 128)
  x4 <- mpfr(exp(H[S,3]), precBits = 128)
  x5 <- mpfr(exp(H[S,4]), precBits = 128)
  x6 <- mpfr(exp(H[S,5]), precBits = 128)
  x7 <- mpfr(exp(H[S,6]), precBits = 128)
  pr_A=(x1)/((x2)+(x3)+(x4)+(x5)+(x6)+(x7))
  # if(pr_A==1){
  #   stop(sprintf("pr_A is 1 = %.20f, Action=%i,State=%i, H=%s",pr_A,A,S,paste(as.numeric(x1),as.numeric(x2),as.numeric(x3),as.numeric(x4),as.numeric(x5),as.numeric(x6), sep=" ")))
  # }else if(pr_A==0){
  #   stop(sprintf("pr_A is 1 = %.20f, Action=%i,State=%i, H=%s",pr_A,A,S,paste(as.numeric(x1),as.numeric(x2),as.numeric(x3),as.numeric(x4),as.numeric(x5),as.numeric(x6), sep=" ")))
  # }
  return(pr_A)
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

### Init H
H = matrix(0,nrow=2,ncol=6)
colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(H)<-c("E","I")
H[1,1]=0.5
H[2,1]=0.5


## Session 1
sessions <-c(100,200,300,400)
alpha=0.04
max_steps=100
n=96
#debug(epsilon_greedy)
#debug(getNextState)
#debug(sarsa)
Q1=aca_rl(H,Visits,Scores,alpha,n,max_steps)