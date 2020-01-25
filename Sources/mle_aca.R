
library(GenSA)
library(Rmpfr)
library(parallel)
library(optimParallel)

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
  allpaths = updateACAPathNb(allpaths)
  

  
  #out <- GenSA(lower = c(0.0,0.0,0.0,0.0), upper = c(1,1,1,1), fn = rl_eg_negLogLik,allpaths=allpaths, Q=Q, E=E,control = list(max.time=600, verbose=TRUE))
  
  #est <- optim(c(0.1,0.8,0.1,0.8,0.5,0.5),rl_eg_negLogLik,lower=c(0,0,0,0,0,0),upper=c(1,1,1,1,1,1),allpaths=allpaths, Q=Q, E=E, method="L-BFGS-B")
  #print(sprintf("Estimated parameters for rat %s = %s",rat,est$par ))


  cl <- makeCluster(detectCores()-1)
  setDefaultCluster(cl=cl)
  clusterExport(cl, varlist=c("aca_mle","rl_aca_negLogLik","getNextState","getPathNumber","updateACAPathNb","enreg","softmax"))
  clusterEvalQ(cl, library("Rmpfr"))
  # startIter <- 3
  # # set the number of values for which to run optim in full
  # fullIter <- 5
  # define a set of starting values
  starting_values<-generate_starting_values(10,c(0,0,0),c(0.999,1,1))
  # call optim with startIter iterations for each starting value
  # opt <- apply(starting_values,2,function(x) optimParallel(x,rl_aca_negLogLik,lower=c(0.001,0,0),upper=c(0.999,1,1),allpaths=allpaths,method="L-BFGS-B",control=list(maxit=startIter)))
  # # define new starting values as the fullIter best values found thus far
  # starting_values_2 <- lapply(opt[order(unlist(lapply(opt,function(x) x$value)))[1:fullIter]],function(x) x$par)

  optimal_vals <-numeric()
  min_val=Inf
  control <- list(factr=.01/.Machine$double.eps)
  for(i in 1:length(starting_values[,1])){
    print(sprintf("starting_values=%s",paste(starting_values[i,],collapse = " ")))
      est <- optimParallel(starting_values[i,],rl_aca_negLogLik,lower=c(0,0,0),upper=c(0.999,1,1),allpaths=allpaths, control=control)
      if(est$value<min_val && est$convergence==0){
        min_val = est$value
        optimal_vals <- est$par
      }
    
  }
  
  print(sprintf("%s,optimal_vals: %s, min_val :%f",rat,paste(optimal_vals,collapse = " "),min_val))
          
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

updateACAPathNb=function(allpaths){
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
aca_mle=function(alpha,path1_prob1,path1_prob2,allpaths){
  
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
  avg_score=0
  actions <-list()
  states <-list()
  activations <- list()
  
  QProb<-mpfrArray(1, prec = 128, dim = 1)
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  activations[[episode]] <- vector()
  
  if(grepl("^.*e$",allpaths[1,1])){
    S = 1
  }else if(grepl("^.*i$",allpaths[1,1])){
    S = 2
  }else{
    print("Unknown intial state. Check")
  }
  
  #A  = as.numeric(allpaths[1,3])
  episode=1
  
  
  initState=0
  changeState = F
  returnToInitState = F
  score_episode=0
  
  for(i in 2:(length(allpaths[,1]))-1){
    
    print(sprintf("i=%i,episode=%i",i,episode))
    
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    pos_trial_t<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[pos_trial_t,"Reward"]))
    time_spent_in_trial = as.numeric(enreg[[ses]]$POS[pos_trial_t[length(pos_trial_t)],1]) - as.numeric(enreg[[ses]]$POS[pos_trial_t[1],1])
    if(time_spent_in_trial==0){
      time_spent_in_trial=20
    }
    
    
    if(R>0){
      score_episode=score_episode+1
    }else{
      score_episode=score_episode+0
    }
    
    A=as.numeric(allpaths[i,3])
    S_prime=getNextState(allpaths,i)
    
    if(A == 4 & S == 1){
      actions[[episode]] <- append(actions[[episode]],51)
    }else if(A == 4 & S == 2){
      actions[[episode]] <- append(actions[[episode]],49)
    }else{
      actions[[episode]] <- append(actions[[episode]],unname(A))
    }
    activations[[episode]] <- append(activations[[episode]],1000/time_spent_in_trial)
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
      
      a<-actions[[episode]]
      s<-states[[episode]]
      
      total_actions= length((actions[[episode]]))
      avg_score = avg_score + ((score_episode/total_actions)-avg_score)/episode
      #activations[[episode]]<-activations[[episode]]/sum(activations[[episode]])
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
            
            #activity=sum(activations[[episode]][which(actions[[episode]]==action)])/sum(activations[[episode]])
            #print(sprintf("Activty=%f",score_episode*activity))
            #H[state,action]=H[state,action]+alpha*((score_episode*activity)-avg_score)*(1-as.numeric(softmax(action,state,H)))
            activity=as.numeric(softmax(action,state,H))
            H[state,action]=H[state,action]+alpha*(score_episode/total_actions)
            
            
            if(is.nan(H[state,action])){
              stop("H[state,action] is NaN")
            }
            
          }
          ## If S,A is not visited in the episode
        #   else{
        #     if(action==49|action==51){
        #       action=4
        #     }
        #     print(sprintf("Activty=%f",score_episode/total_actions))
        #     H[state,action]=H[state,action]-alpha*((score_episode/total_actions)-avg_score)*(as.numeric(softmax(action,state,H)))
        #     
        #   }
        }
      }
      
      #print(H)
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
    ### End episode check
    x<-mpfr(softmax(A,S,H),128)
    if(is.infinite(as.numeric(x))){
      stop("softmax return Inf")
    }
    QProb<-c(QProb,x)
    
    S=S_prime

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
  path1_prob1 <- par[2]
  path1_prob2 <- par[3]
  lik <- aca_mle(alpha,path1_prob1,path1_prob2,allpaths)
  negLogLik <- -sum(log(lik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(as.numeric(negLogLik))
  }
  
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
    return(pr_A)
}

