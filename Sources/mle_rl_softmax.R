
library(GenSA)
library(parallel)
library(optimParallel)
library(randtoolbox)
library(Rmpfr)

#### Function to call for plotting heatmap
mle_rl_softmax=function(enreg,rat){
  
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
  allpaths = updatePathNb(allpaths)
  
  cl <- makeCluster(detectCores()-1)
  setDefaultCluster(cl=cl)
  clusterExport(cl, varlist=c("sarsa_mle_softmax","rl_eg_negLogLik_softmax","getNextState","getPathNumber","softmax_sarsa","enreg"))
  clusterEvalQ(cl, library("Rmpfr"))
  startIter <- 3
  # set the number of values for which to run optim in full
  fullIter <- 5
  # define a set of starting values
  starting_values<-generate_starting_values(10,c(0.001,0.001,0,0),c(0.999,0.999,1,1))
  optimal_vals <-numeric()
  min_val=Inf
  control <- list(factr=.01/.Machine$double.eps)
  for(i in 1:length(starting_values[,1])){
    est <- optimParallel(starting_values[i,],rl_eg_negLogLik_softmax,lower=c(0.001,0.001,0,0),upper=c(0.999,0.999,1,1),allpaths=allpaths, control=control)
    if(est$value<min_val && est$convergence==0){
      min_val = est$value
      optimal_vals <- est$par
    }
  }
  
  print(sprintf("%s,optimal_vals: %s, min_val :%f",rat,paste(optimal_vals,collapse = " "),min_val))
  
  
}


updatePathNb=function(allpaths){
  allpaths <- cbind(allpaths,0)
  for(i in 1:(length(allpaths[,1]))){
    allpaths[i,3] = getPathNumber(allpaths[i,1])
  }
  return(allpaths)
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
sarsa_mle_softmax=function(alpha,epsilon,gamma,lambda,path1_prb1,path1_prb2,allpaths){
  
  ### Init Q
  Q = matrix(0,nrow=2,ncol=6)
  colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Q)<-c("E","I")
  
  ### Init E
  E=matrix(0,nrow=2,ncol=6)
  colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(E)<-c("E","I")
  
  actions <-list()
  states <-list()
  
  QProb<-mpfrArray(1, prec = 128, dim = 1)
  Q[1,1]=path1_prb1
  Q[2,1]=path1_prb2
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
  
  initState=S
  changeState = F
  returnToInitState = F
  
  
  for(i in 2:(length(allpaths[,1])-1)){
    
    
    # if(A==which.max(Q[S,])){
    #   QProb <- c(QProb,(1-epsilon +epsilon/6))
    # }else{
    #   QProb <- c(QProb,(epsilon/6))
    # }
    QProb<-c(QProb,mpfr(softmax_sarsa(A,S,Q),128))
    
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
      E=E*0
      episode  = episode+1
      #print(sprintf("Updating episode to %i",episode))
      if(i < length(allpaths)-1){
        actions[[episode]] <- vector()
      }
    }
  }
  return(QProb)
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
  return(pr_A)
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


rl_eg_negLogLik_softmax <- function(par,allpaths) {
  alpha <- par[1]
  gamma <- par[2]
  epsilon <- 0
  lambda <- 1
  path1_prb1 <- par[3]
  path1_prb2 <- par[4]
  lik <- sarsa_mle_softmax(alpha,epsilon,gamma,lambda,path1_prb1,path1_prb2,allpaths)
  negLogLik <- -sum(log(lik))
  
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(as.numeric(negLogLik))
  }
}


