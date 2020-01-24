
library(Rmpfr)




R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1


mazeACA=function(enreg,rat){
  
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
  allpaths = updateACAPathNb1(allpaths)
  
  probEmp=getStatsAllpaths(allpaths)
  
  ################Call ACA
  last_session = as.numeric(tail(allpaths[,"Session"],1))
  sessions <- numeric()
  for(ses in 1:last_session){
    allpaths_ses1<-which(allpaths[,"Session"]==ses)
    sessions<-c(sessions,allpaths_ses1[1])
  }
  sessions<-sessions[!is.na(sessions)]
  max_steps=length(allpaths[,1])
  sessions<-c(sessions,max_steps)
  
  ### Init H
  H = matrix(0,nrow=2,ncol=6)
  colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(H)<-c("E","I")
  # H[1,1]=0.25
  # H[2,1]=0.75
  # alpha=0.04177

    # H[1,1]=0.125
  # H[2,1]=0.375
  # alpha=0.02892765
  
  H[1,1]=0.5
  H[2,1]=0.5
  alpha=0.001
  
  #max_steps=500
  #debug(aca_rl)
  probACA=aca_rl(H,alpha,max_steps,sessions)
  
  
  
  #############Call SARSA
  
  
  ### Init Q
  Q = matrix(0,nrow=2,ncol=6)
  colnames(Q)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(Q)<-c("E","I")
  Q[1,1]=0.875
  Q[2,1]=0.875
  
  E=matrix(0,nrow=2,ncol=6)
  colnames(E)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
  rownames(E)<-c("E","I")
  
  ## Session 1
  alpha=0.1221627
  gamma=0.8894646
  epsilon=0
  lambda=1
  max_steps=1380
  #debug(epsilon_greedy)
  #debug(getNextState)
  #debug(sarsa)
  probSARSA=sarsa_smax(Q,E,alpha,max_steps,epsilon,gamma,lambda,sessions)

  plotProbs(probEmp,probACA,probSARSA,rat)
  
  print(getMSE(probEmp,probACA))
  print(getMSE(probEmp,probSARSA))
  print(getCorrMatrix(probEmp,probACA))
  print(getCorrMatrix(probEmp,probSARSA))
}

updateACAPathNb1=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
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
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,5]=1
    }else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
    }else{
      allpaths[i,5]=0
    }
    
  }
  return(allpaths)
}

getStatsAllpaths=function(allpaths){
  probMatrix_allpaths=matrix(0,12,0)
  rownames(probMatrix_allpaths)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  last_session = as.numeric(tail(allpaths[,"Session"],1))
  sessions <- numeric()
  for(ses in 1:last_session){
    allpaths_ses1<-which(allpaths[,"Session"]==ses)
    if(length(allpaths_ses1)==0){
      next
    }
    allpaths_ses1<-allpaths[allpaths_ses1,]
    len<-length(allpaths_ses1[,1])
    probMatrix_allpaths <- cbind(probMatrix_allpaths,0)
    colIndex=length(probMatrix_allpaths[1,])
    
    probMatrix_allpaths[1,colIndex]=length(which(allpaths_ses1[,3]==1 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[2,colIndex]=length(which(allpaths_ses1[,3]==2 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[3,colIndex]=length(which(allpaths_ses1[,3]==3 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[4,colIndex]=length(which(allpaths_ses1[,3]==4 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[5,colIndex]=length(which(allpaths_ses1[,3]==5 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[6,colIndex]=length(which(allpaths_ses1[,3]==6 & allpaths_ses1[,5]==1))/len
    probMatrix_allpaths[7,colIndex]=length(which(allpaths_ses1[,3]==1 & allpaths_ses1[,5]==2))/len
    probMatrix_allpaths[8,colIndex]=length(which(allpaths_ses1[,3]==2 & allpaths_ses1[,5]==2))/len
    probMatrix_allpaths[9,colIndex]=length(which(allpaths_ses1[,3]==3 & allpaths_ses1[,5]==2))/len
    probMatrix_allpaths[10,colIndex]=length(which(allpaths_ses1[,3]==4 & allpaths_ses1[,5]==2))/len
    probMatrix_allpaths[11,colIndex]=length(which(allpaths_ses1[,3]==5 & allpaths_ses1[,5]==2))/len
    probMatrix_allpaths[12,colIndex]=length(which(allpaths_ses1[,3]==6 & allpaths_ses1[,5]==2))/len
  }
  return(probMatrix_allpaths)
}

### Action = Path
## State = E or I

aca_rl=function(H,alpha,max_steps,sessions){
  
  ## Start form state 1 = Box "E"
  
  actions <-list()
  states <-list()
  probMatrix_aca=matrix(0,12,0)
  rownames(probMatrix_aca)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  
  episode=1
  actions[[episode]] <- vector()
  states[[episode]] <- vector()
  
  initState=1
  changeState = F
  returnToInitState = F
  reward=0
  S=initState
  session=0
  startIndex_session=0
  avg_score=0
  score_episode=0
  
  for(i in c(1:max_steps)){
    
    #print(sprintf("Step=%i,Episode=%i",i,episode))
    
    A=softmax_policy(H,S)
    
    ## Update S based on action A
    S_prime=getNextState_ACA_RL(S,A)  
    
    
    if(length(actions[[episode]])==0){
      initState=S
    }
    
    if(R[S,A]>0){
      score_episode=score_episode+1
    }else{
      score_episode=score_episode+0
    }
    
    if(A == 4 & S == 1){
      actions[[episode]] <- append(actions[[episode]],51)
    }else if(A == 4 & S == 2){
      actions[[episode]] <- append(actions[[episode]],49)
    }else{
      actions[[episode]] <- append(actions[[episode]],unname(A))
    }
    #print("Here")
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
    
    if(i %in% sessions){
      print(sprintf("Start new session"))
      if(i>1){
        #debug(getStatsOfLastSession)
        probMatrix_aca=getStatsOfLastSession(probMatrix_aca,startIndex_session,session,actions,states)
      }
      
      session=session+1
      startIndex_session=i
    }
    
    
    ## Check if episode ended
    if(returnToInitState){
      changeState = F
      returnToInitState = F
      
      a<-actions[[episode]]
      s<-states[[episode]]
      
      total_actions= length((actions[[episode]]))
      avg_score = avg_score + (score_episode/total_actions-avg_score)/episode
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
            
            activity=length(which(actions[[episode]]==action))/total_actions
            H[state,action]=H[state,action]+alpha*((score_episode*activity)-avg_score)*(1-as.numeric(softmax(action,state,H)))
            
            if(is.nan(H[state,action])){
              stop("H[state,action] is NaN")
            }
            
          }
          ## If S,A is not visited in the episode
          else{
            if(action==49|action==51){
              action=4
            }
            H[state,action]=H[state,action]-alpha*((score_episode/total_actions)-avg_score)*(as.numeric(softmax(action,state,H)))
            
          }
        }
      }
      ## reset rewards
      score_episode=0
      episode = episode+1
      
      #print(sprintf("Updating episode to %i",episode))
      if(i <= (max_steps-1)){
        actions[[episode]] <- vector()
        states[[episode]] <- vector()
        #activations[[episode]] <- vector()
      }
   
    }
    ### End of episode checkd
    S=S_prime
    

  }

  print(unlist(states))
  # a=as.data.frame(actions)
  # colnames(a)=NULL
  # rownames(a)=NULL
  capture.output(print(actions), file = "/home/ajames/intership2/actions_ACA.txt")
  capture.output(print(actions), file = "/home/ajames/intership2/states_ACA.txt")
  
  #print()
  return(probMatrix_aca)
}

getNextState_ACA_RL=function(curr_state,action){
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


getStatsOfLastSession=function(probMatrix_aca,session_start,session,actions,states){
  mat<-matrix(0,0,2)
  probMatrix_aca <- cbind(probMatrix_aca,0)
  colIndex=length(probMatrix_aca[1,])
  all_actions<-unlist(actions)
  all_actions<-all_actions[session_start:(length(all_actions)-1)]
  
  all_states<-unlist(states)
  all_states<-all_states[session_start:(length(all_states)-1)]
  len1<-length(which(all_states==1))
  len2<-length(which(all_states==2))
  mat<-matrix(0,length(all_actions),2)
  mat[,1]<-all_actions
  mat[,2]<-all_states
  
  probMatrix_aca[1,colIndex]=length(which(mat[,1]==1 & mat[,2]==1))/len1
  probMatrix_aca[2,colIndex]=length(which(mat[,1]==2 & mat[,2]==1))/len1
  probMatrix_aca[3,colIndex]=length(which(mat[,1]==3 & mat[,2]==1))/len1
  probMatrix_aca[4,colIndex]=length(which(mat[,1]==51 & mat[,2]==1))/len1
  probMatrix_aca[5,colIndex]=length(which(mat[,1]==5 & mat[,2]==1))/len1
  probMatrix_aca[6,colIndex]=length(which(mat[,1]==6 & mat[,2]==1))/len1
  probMatrix_aca[7,colIndex]=length(which(mat[,1]==1 & mat[,2]==2))/len2
  probMatrix_aca[8,colIndex]=length(which(mat[,1]==2 & mat[,2]==2))/len2
  probMatrix_aca[9,colIndex]=length(which(mat[,1]==3 & mat[,2]==2))/len2
  probMatrix_aca[10,colIndex]=length(which(mat[,1]==49 & mat[,2]==2))/len2
  probMatrix_aca[11,colIndex]=length(which(mat[,1]==5 & mat[,2]==2))/len2
  probMatrix_aca[12,colIndex]=length(which(mat[,1]==6 & mat[,2]==2))/len2
  
  return(probMatrix_aca)
}

plotProbs=function(probEmp,probACA,probSARSA,rat){
  
  for(i in 1:12){
    filename = paste(rat,"Path_probabilites-",rownames(probEmp)[i],".jpg",sep="")
    jpeg(filename,width=800,height=800,quality = 100)
    plot(probEmp[i,],col='black',type='l',ylim=c(0,1),xlab="Sessions",ylab="Path Prob",main=paste(rat,"_Path probability of ",rownames(probEmp)[i],sep=""))
    lines(probACA[i,],col='red',type='l',lty=2)
    lines(probSARSA[i,],col='blue',type='l',lty=3)
    legend("topright", legend=c("Empirical Prob from data", "Probability by ACA","Probability by SARSA"),lty=c(1,2,3),col=c("black", "red","blue"),cex=0.75,bty = "n")
    dev.off()
  }
  
}

getCorrMatrix=function(prob1,prob2){
  corrMatrix=matrix(0,2,12)
  rownames(corrMatrix)<-c("correlation","p-value")
  colnames(corrMatrix)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  for(i in 1:12){
   x<- cor.test(prob1[i,],prob2[i,])
   corrMatrix[1,i]=x$estimate
   corrMatrix[2,i]=x$p.value
  }
  return(corrMatrix)
}

getMSE=function(prob1,prob2){
  mseMatrix=matrix(0,1,12)
  rownames(mseMatrix)<-c("MSE")
  colnames(mseMatrix)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  for(i in 1:12){
    x<- mean((prob1[i,]-prob2[i,])^2)
    mseMatrix[1,i]=x
  }
  return(mseMatrix)
}

getCovarianceMatrix=function(prob1,prob2){
  covMatrix=matrix(0,2,12)
  rownames(covMatrix)<-c("correlation","p-value")
  colnames(covMatrix)<-c("State1-Path1","State1-Path2","State1-Path3","State1-Path4","State1-Path5","State1-Path6","State2-Path1","State2-Path2","State2-Path3","State2-Path4","State2-Path5","State2-Path6")
  for(i in 1:12){
    x<- cov(prob1[i,],prob2[i,])
    covMatrix[1,i]=x$estimate
    covMatrix[2,i]=x$p.value
  }
  return(corrMatrix)
}

# ### Init H
# H = matrix(0,nrow=2,ncol=6)
# colnames(H)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
# rownames(H)<-c("E","I")
# H[1,1]=0.5
# H[2,1]=0.5
# 
# 
# sessions <-c(1,100,200,300,400)
# alpha=0.04
# max_steps=500
# #debug(aca_rl)
# probM=aca_rl(H,alpha,max_steps,sessions)