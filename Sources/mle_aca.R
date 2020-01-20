
library(GenSA)
library(Rmpfr)

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
    l<-list(allpaths,allpaths_ses)
    allpaths <- rbind(allpaths,allpaths_ses)
    
    
  }
  allpaths = updatePathNb(allpaths)
  

  
  #out <- GenSA(lower = c(0.0,0.0,0.0,0.0), upper = c(1,1,1,1), fn = rl_eg_negLogLik,allpaths=allpaths, Q=Q, E=E,control = list(max.time=600, verbose=TRUE))
  
  #est <- optim(c(0.1,0.8,0.1,0.8,0.5,0.5),rl_eg_negLogLik,lower=c(0,0,0,0,0,0),upper=c(1,1,1,1,1,1),allpaths=allpaths, Q=Q, E=E, method="L-BFGS-B")
  #print(sprintf("Estimated parameters for rat %s = %s",rat,est$par ))
  minima=Inf
  
  l<-which(as.numeric(allpaths[,"Reward"])!=0)
  n_init=l[1]+10
  alpha_min=0
  n_min=0
  n_max=n_init+100
  
  # for(alpha in seq(0.1,1,0.1)){
  #   for(n in seq(n_init,200)){
  #     
  #   
  #     log_likelihood=rl_aca_negLogLik(c(alpha,n),allpaths,H,Scores,Visits)
  #     
  #     if(log_likelihood < minima){
  #       minima=log_likelihood
  #       alpha_min=alpha
  #       n_min=n
  #     }
  #   }
  #   
  # }
  # print(sprintf("alpha_min=%f,n_min=%i",alpha_min,n_min))
  
  cl <- makeCluster(detectCores()-1)
  setDefaultCluster(cl=cl)
  clusterExport(cl, varlist=c("sarsa_mle","rl_eg_negLogLik","getNextState","getPathNumber","updatePathNb","enreg"))
  startIter <- 3
  # set the number of values for which to run optim in full
  fullIter <- 5
  # define a set of starting values
  starting_values<-generate_starting_values(4,c(0.001,n_init),c(0.999,n_max))
  # call optim with startIter iterations for each starting value
  opt <- apply(starting_values,2,function(x) optimParallel(x,rl_eg_negLogLik,lower=c(0.001,n_init),upper=c(0.999,n_max),allpaths=allpaths,method="L-BFGS-B",control=list(maxit=startIter)))
  # define new starting values as the fullIter best values found thus far
  starting_values_2 <- lapply(opt[order(unlist(lapply(opt,function(x) x$value)))[1:fullIter]],function(x) x$par)
  # run optim in full for these new starting values
  opt <- lapply(starting_values_2,optimParallel,fn=rl_eg_negLogLik,lower=c(0.001,n_init),upper=c(0.999,n_max),allpaths=allpaths,method="L-BFGS-B",parallel=list(loginfo=TRUE))
  
  optimal_vals<-opt[[which.min(unlist(lapply(opt,function(x) x$value)))]]$par
  
  print(sprintf("%s,optimal_vals: %s",rat,paste(optimal_vals,collapse = " ")))
          
}

generate_starting_values <- function(n,min,max) {
  if(length(min) != length(max)) stop("min and max should have the same length")
  dim <- length(min)
  # generate Sobol values
  start <- sobol(n,dim=dim)
  # transform these to lie between min and max on each dimension
  for(i in 1:ncol(start)) {
    start[,i] <- min[i] + (max[i]-min[i])*start[,i]
  }
  return(start)
}

updatePathNb=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0)
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
  }
  return(allpaths)
}




getPathNumber=function(path){
  path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i$",path)){
    pathnb = 1
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 2
  }else if(grepl("^f.*g.*a.*k.*j.*i$",path)){
    pathnb = 3
  }else if(grepl("^j.*i$",path)||grepl("^h.*i$",path)){
    pathnb = 5
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i$",path)){
    pathnb = 4
  }else if(grepl("^h.*c.*d.*e$",path)){
    pathnb = 1
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 2
  }else if(grepl("^j.*k.*a.*g.*f.*e$",path)){
    pathnb = 3
  }else if(grepl("^f.*e$",path)||grepl("^d.*e$",path)){
    pathnb = 5
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e$",path)){
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
aca_mle=function(alpha,n,allpaths){
  
  ### Init H
  H = matrix(0,nrow=2,ncol=6)
  colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(H)<-c("E","I")
  
  ### Init S - for scores
  Score = matrix(0,nrow=2,ncol=6)
  colnames(Score)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Score)<-c("E","I")
  
  ## Counter for Actions
  Visits = matrix(0,nrow=2,ncol=6)
  colnames(Visits)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Visits)<-c("E","I")
  
  ## 
  
  actions <-list()
  states <-list()
  
  QProb<-mpfrArray(1, prec = 128, dim = 1)
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  
  initState=0
  changeState = F
  returnToInitState = F
  reward=0
  
  for(i in 1:(length(allpaths[,1]))){
    
    
    S=getNextState(allpaths,i)
    A=as.numeric(allpaths[i,3])
    
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    pos_trial_t<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[pos_trial_t,"Reward"]))
    
    if(R>0){
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
              time_spent_in_trial = as.numeric(enreg[[ses]]$POS[pos_trial_t[length(pos_trial_t)],1]) - as.numeric(enreg[[ses]]$POS[pos_trial_t[1],1])
              H[state,action]=reward*1000/time_spent_in_trial
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
  return(QProb)
}




getNextState=function(allpaths,i){
  if(grepl("^.*e$",allpaths[i,1])){
    next_state = 1
  }else if(grepl("^.*i$",allpaths[i,1])){
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


rl_aca_negLogLik <- function(par,allpaths) {
  alpha <- par[1]
  n <- par[2]
  lik <- aca_mle(alpha,n,allpaths)
  negLogLik <- -sum(log(lik))
  return(as.numeric(negLogLik))
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

