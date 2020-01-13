

#### Function to call for plotting heatmap
mle_rl=function(enreg,rat,dirpath1,plot){

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
    allpaths <- toString(r$values)
    allpaths<-strsplit(allpaths,"(?<=[ei])(?=(, j, k)|(, f, g)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    
    ### CHANGE THIS:
    allpaths <- allpaths[,1]
    ###
    est <- optim(c(0.1,0.8,0.1,0.8),rl_eg_negLogLik,lower=c(0,0,0,0),upper=c(1,1,1,1),allpaths=allpaths, Q=Q, E=E,method="L-BFGS-B")
    est$pa
    
    
  }
}



R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1

### Init Q
Q = matrix(0,nrow=2,ncol=6)
colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(Q)<-c("E","I")
Q[1,1]=0.5
Q[2,1]=0.5

E=matrix(0,nrow=2,ncol=6)
colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(E)<-c("E","I")

QProb=matrix(0,nrow=2,ncol=6)
colnames(QProb)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(QProb)<-c("E","I")


getPathNb=function(path){
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
  }
  
  return(pathnb)
}

### Action = Path
## State = E or I
sarsa_mle=function(alpha,epsilon,gamma,lambda,allpaths,Q,E){
  
  actions <-list()
  states <-list()
  
  QProb <-numeric()
  ## Ignore first path
  ## Start from the end of path1
  
  if(grepl("^.*e$",allpaths[1])){
    S = 1
  }else if(grepl("^.*i$",allpaths[1])){
    S = 2
  }else{
    print("Unknown intial state. Check")
  }
  
  A  = getPathNb(allpaths[2])
  episode=1
  actions[[episode]] <- vector()
  
  initState=S
  changeState = F
  returnToInitState = F
  

  for(i in 2:(length(allpaths)-1)){

    if(A==which.max(Q[S,])){
      QProb <- c(QProb,(1-epsilon +epsilon/6))
    }else{
      QProb <- c(QProb,(epsilon/6))
    }
    
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==i)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    reward=0
    if(R>0){
      reward=1
    }else{
      reward=0
    }
    
    S_prime=getNextState(S,A)
    A_prime=getPathNb(allpaths[i+1])
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


rl_eg_negLogLik <- function(par,allpaths,Q,E) {
  alpha <- par[1]
  gamma <- par[2]
  epsilon <- par[3]
  lambda <- par[4]
  lik <- sarsa_mle(alpha,epsilon,gamma,lambda,allpaths,Q,E)
  negLogLik <- -sum(log(lik))
  return(negLogLik)
}



