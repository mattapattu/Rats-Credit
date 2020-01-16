
library(GenSA)

#### Function to call for plotting heatmap
mle_aca=function(enreg,rat){
  
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
    }
    
    last_trial <- as.numeric(enreg[[ses]]$POS[length(enreg[[ses]]$POS[,1]),"trial"])
    reward49_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "49"),"trial"])
    reward51_trials <- as.numeric(enreg[[ses]]$POS[which(enreg[[ses]]$POS[,"Reward"]== "51"),"trial"])
    
    r <- rle(enreg[[ses]]$POS[,"boxname"])
    allpaths_ses <- toString(r$values)
    allpaths_ses<-strsplit(allpaths_ses,"(?<=[ei])(?=(, j, k)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths_ses <-cbind(allpaths_ses,ses)
    colnames(allpaths_ses) <- c("Path","Session")
    l<-list(allpaths,allpaths_ses[-1])
    allpaths <- rbind(allpaths,allpaths_ses[-1,])
    
    
  }
  allpaths = updatePathNb(allpaths)
  
  
  ### Init H
  H = matrix(0,nrow=2,ncol=6)
  colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(H)<-c("E","I")
  
  ### Init S - for scores
  Scores = matrix(0,nrow=2,ncol=6)
  colnames(Scores)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Scores)<-c("E","I")
  
  ## Counter for Actions
  V = matrix(0,nrow=2,ncol=6)
  colnames(V)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(V)<-c("E","I")
  
  
  #out <- GenSA(lower = c(0.0,0.0,0.0,0.0), upper = c(1,1,1,1), fn = rl_eg_negLogLik,allpaths=allpaths, Q=Q, E=E,control = list(max.time=600, verbose=TRUE))
  
  #est <- optim(c(0.1,0.8,0.1,0.8,0.5,0.5),rl_eg_negLogLik,lower=c(0,0,0,0,0,0),upper=c(1,1,1,1,1,1),allpaths=allpaths, Q=Q, E=E, method="L-BFGS-B")
  #print(sprintf("Estimated parameters for rat %s = %s",rat,est$par ))
  minima=Inf
  
  for(alpha in seq(0,1,0.1)){
    for(gamma in seq(0,1,0.1)){
      for(epsilon in seq(0,1,0.1)){
        for(lambda in seq(0,1,0.1)){
          for(path1_prb1 in seq(0,1,0.1)){
            for(path1_prb2 in seq(0,1,0.1)){
              
              log_likelihood=rl_eg_negLogLik(c(alpha,gamma,epsilon,lambda),allpaths,Q,E)
              if(log_likelihood < minima){
                minima=log_likelihood
              }
            }
            
          }
          
        }
      }
    }
  }
  
  
}


updatePathNb=function(allpaths){
  allpaths <- cbind(allpaths,0)
  for(i in 1:(length(allpaths[,1]))){
    allpaths[i,3] = getPathNumber(allpaths[i,1])
  }
  return(allpaths)
}




getPathNumber=function(path){
  path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i",path)){
    pathnb = 1
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i",path)){
    pathnb = 2
  }else if(grepl("^f.*g.*a.*k.*j.*i",path)){
    pathnb = 3
  }else if(grepl("^j.*i",path)){
    pathnb = 5
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i",path)){
    pathnb = 4
  }else if(grepl("^h.*c.*d.*e",path)){
    pathnb = 1
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e",path)){
    pathnb = 2
  }else if(grepl("^j.*k.*a.*g.*f.*e",path)){
    pathnb = 3
  }else if(grepl("^f.*e",path)){
    pathnb = 5
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e",path)){
    pathnb = 4
  }else if(grepl("^.*e$",path)){
    pathnb = 6
  }else if(grepl("^.*i$",path)){
    pathnb = 6
  }else{
    ## A =7
    pathnb=6
  }
  
  return(pathnb)
}

### Action = Path
## State = E or I
aca_mle=function(alpha,n,allpaths,Q,E){
  
  actions <-list()
  states <-list()
  
  QProb <-numeric()
  ## Ignore first path
  ## Start from the end of path1
  
  if(grepl("^.*e$",allpaths[1,1])){
    S = 1
  }else if(grepl("^.*i$",allpaths[1,1])){
    S = 2
  }else{
    print("Unknown intial state. Check")
  }
  
  A  = as.numeric(allpaths[1,3])
 
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  
  initState=S
  changeState = F
  returnToInitState = F
  reward=0
  
  for(i in 2:(length(allpaths[,1])-1)){
    
    if(i<=n){
      QProb <- c(QProb,(epsilon/6))
    }else{
      QProb <- c(QProb,softmax(A,S,H))
    }
    
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==i)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      reward=reward+1
    }else{
      reward=reward+0
    }
    
    ## Next state is the box where current action ends
    S_prime=getNextState(allpaths,i)
    A_prime=as.numeric(allpaths[i+1,3])

   
    
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
      initState=S
      
      a<-actions[[episode]]
      s<-states[[episode]]
      
      for(state in 1:2){
        for(action in 1:6){
          ## If S,A is visited in the episode
          if(any(s==state) && any(a==action)){
            if(i<=n){
              ### Credit = Score * activity
              ## Activity of (A,S) = #Nb of times Action A is taken in State S/ # Nb of times State S is visited
              H[state,action]=reward*V[state,action]/rowsum(V[state,])
            }else{
              ## expected_score = E(Score of Action A in State S) = #Total Score of Action A in State S before current trial/#Nb of times Action A is taken in State S
              expected_score = Score[state,action]/V[state,action]
              H[state,action]=H[state,action]+alpha*(reward-expected_score)*(1-softmax(action,state,H))
            }
          }## If S,A is not visited in the episode
          else{
            if(i<=n){
              ### No Credit update as reward =0
            }else{
              expected_score = Score[state,action]/V[state,action]
              H[state,action]=H[state,action]-alpha*(reward-expected_score)*(softmax(action,state,H))
            }
          }
          
          ## Update Score of current Action A in current state S
          Score[state,action]=Score[state,action]+reward
          V[state,action]=V[state,action]+1
        }
      }
      ## reset rewards
      reward=0
      episode = episode+1
      
      #print(sprintf("Updating episode to %i",episode))
      if(i < length(allpaths)-1){
        actions[[episode]] <- vector()
      }
    }
    
    S=S_prime
    A=A_prime
  }
  return(QProb)
}




getNextState=function(allpaths,i){
  if(grepl("^.*e",allpaths[i,1])){
    next_state = 1
  }else if(grepl("^.*i",allpaths[i,1])){
    next_state = 2
  }
  ### If allpaths[i,] does not end in E/I , use the begining of next path to find the new state 
  else{
    if(i<length(allpaths[i,])){
      if(grepl("^, f",allpaths[i+1,1])||grepl("^, d",allpaths[i+1,1])){
        next_state = 1
      }else if(grepl("^, j",allpaths[i+1,1])||grepl("^, h",allpaths[i+1,1])){
        next_state = 2
      }
    }else{
      next_state=-1
    }
    
  }
  
  return(next_state)
}


rl_eg_negLogLik <- function(par,allpaths,Q,E) {
  alpha <- par[1]
  gamma <- par[2]
  epsilon <- par[3]
  lambda <- par[4]
  path1_prb1 <- par[5]
  path1_prb2 <- par[6]
  lik <- sarsa_mle(alpha,epsilon,gamma,lambda,path1_prb1,path1_prb2,allpaths,Q,E)
  negLogLik <- -sum(log(lik))
  return(negLogLik)
}

softmax=function(A,S,H){
  pr_A=exp(H[S,A])/rowsum(H[S,])
  return(pr_A)
}



