library(dplyr)
library(RColorBrewer)
library(TTR)


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
  #path  = gsub("^, ","",path)
  
  if(grepl("^d.*c.*h.*i$",path)){
    pathnb = 1
  }else if(grepl("^d.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 2
  }else if(grepl("^f.*g.*a.*k.*j.*i$",path)){
    pathnb = 3
  }else if(grepl("^f.*g.*a.*b.*c.*c.*d.*e$",path)){
    pathnb = 5
  }else if(grepl("^f.*g.*a.*b.*c.*h.*i$",path)){
    pathnb = 4
  }else if(grepl("^d.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 6
  }
  else if(grepl("^h.*c.*d.*e$",path)){
    pathnb = 1
  }else if(grepl("^h.*c.*b.*a.*g.*f.*e$",path)){
    pathnb = 2
  }else if(grepl("^j.*k.*a.*g.*f.*e$",path)){
    pathnb = 3
  }else if(grepl("^j.*k.*a.*b.*c.*h.*i$",path)){
    pathnb = 5
  }else if(grepl("^j.*k.*a.*b.*c.*d.*e$",path)){
    pathnb = 4
  }else if(grepl("^h.*c.*b.*a.*k.*j.*i$",path)){
    pathnb = 6
  }
  else if(grepl("^.*e$",path)){
    pathnb = 7
  }else if(grepl("^.*i$",path)){
    pathnb = 7
  }else{
    ## A =7
    pathnb=7
  }
  
  return(pathnb)
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
    }
    ## Why ? ( incomplete paths ?)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
    }else if(i==1){
      if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=2
      }else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      
    }
    
  }
  return(allpaths)
}


# Handle incomplete paths in the begining or when recording is lost
updateACAPathNbmse=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    #ses=as.numeric(allpaths[i,"Session"])
    #trial=i-which(allpaths[,"Session"]==ses)[1]+1
    #l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    #R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    
    
    allpaths[i,5] = getPathNumber(allpaths[i,1])
    
    if(allpaths[i,5] == 4)
    {
      allpaths[i,6] = 1
    }
    else
    {
      allpaths[i,6] = 0
    }
    
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,7]=1
    }
    else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,7]=2
    }
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,7]=1
      }
      else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,7]=2
      }
      ## If cannot be estimated, then do by default : assume the trial = Path5
      else if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,7]=1
      }
      else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,7]=2
      }
      
    }else if(i==1){
      if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,7]=2
      }else if(grepl("^.*i$",allpaths[i,1])){
        allpaths[i,7]=1
      }
      
    }
    
  }
  return(allpaths)
}


getTurnDuration=function(path, state, enregRows)
{
  
  turntimes=list();
  if (state == 0)
  {
    if (path == 0)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      h = enregRows[3,2] - enregRows[3,1]
      c2h = c2 + h
      
      
      turntimes = list("dc1"=dc1, "c2h" = c2h)
      
    }
    else if (path == 1)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      kj = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2kj = a2 + kj
      
      
      turntimes = list("fga1"=fga1, "a2kj" = a2kj)
      
    }
    else if (path == 2)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2+kj
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
      
      
    }
    else if (path == 3)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2h"=c2h)
      
    }
    else if (path == 4)
    {
      
      fg = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      fga1 = fg + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      turntimes = list("fga1"=fga1, "a2bc1" = a2bc1, "c2d"=c2d)
    }
    else if (path == 5)
    {
      
      d = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      dc1 = d + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      turntimes = list("dc1"=dc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
    }
  }
  else if (state == 1)
  {
    if (path == 0)
    {
      
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      d = enregRows[3,2] - enregRows[3,1]
      c2d = c2 + d
      
      
      turntimes = list("hc1"=hc1, "c2d" = c2d)
    }
    else if (path == 1)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      gf = sum(enregRows[4:5,2] - enregRows[4:5,1])
      a2gf = a2 + gf
      
      
      turntimes = list("jka1"=jka1, "a2gf" = a2gf)
    }
    else if (path == 2)
    {
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      gf = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2gf = a2 + gf
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2gf"=a2gf)
      
    }
    else if (path == 3)
    {
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      d = enregRows[6,2] - enregRows[6,1]
      c2d = c2 + d
      
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2d"=c2d)
      
    }
    else if (path == 4)
    {
      
      jk = sum(enregRows[1:2,2] - enregRows[1:2,1])
      a = enregRows[3,2] - enregRows[3,1]
      a1 = a/2
      jka1 = jk + a1
      
      a2 = a/2
      b = enregRows[4,2] - enregRows[4,1]
      c = enregRows[5,2] - enregRows[5,1]
      c1 = c/2
      a2bc1 = a2 + b + c1
      
      c2 = c/2
      h = enregRows[6,2] - enregRows[6,1]
      c2h = c2 + h
      
      turntimes = list("jka1"=jka1, "a2bc1" = a2bc1, "c2h"=c2h)
    }
    else if (path == 5)
    {
      
      h = enregRows[1,2] - enregRows[1,1]
      c = enregRows[2,2] - enregRows[2,1]
      c1 = c/2
      hc1 = h + c1
      
      c2 = c/2
      b = enregRows[3,2] - enregRows[3,1]
      a = enregRows[4,2] - enregRows[4,1]
      a1 = a/2
      c2ba1 = c2 + b + a1
      
      a2 = a/2
      kj = sum(enregRows[5:6,2] - enregRows[5:6,1])
      a2kj = a2 + kj
      
      turntimes = list("hc1"=hc1, "c2ba1" = c2ba1, "a2kj"=a2kj)
    }
  }
  
  return (turntimes);
}


getTurnsMatrix=function(allpaths,enreg,turnsModel)
{
  totalTurns = 3*length(allpaths[,1])
  turnTimes = matrix(0,totalTurns,6)
  colnames(turnTimes) <- c("Action", "Path", "State","Turn","Session", "Duration" )
  turnIdx = 1;
  boxIndices = as.numeric(allpaths[,3])
  uniqSess = uniq(as.numeric(allpaths[,4]))$b
  
  for(ses in uniqSess)
  {
    idx_ses = which(as.numeric(allpaths[,4])==ses)
    pathCount_ses = length(allpaths[idx_ses,1])
    
    for(i in 1:pathCount_ses)
    {
      path = as.numeric(allpaths[idx_ses[i],5])-1
      state = as.numeric(allpaths[idx_ses[i],7])-1
      
      pathSlot = ""
      nodeList = ""
      stateSlot = ""
      
      if(path == 0)
      {
        pathSlot = "Path0"
      }
      else if(path == 1)
      {
        pathSlot = "Path1"
      }
      else if(path == 2)
      {
        pathSlot = "Path2"
      }
      else if(path == 3)
      {
        pathSlot = "Path3"
      }
      else if(path == 4)
      {
        pathSlot = "Path4"
      }
      else if(path == 5)
      {
        pathSlot = "Path5"
      }
      else if(path == 6)
      {
        next
      }
      
      if(state==0)
      {
        nodeList = "nodes.S0"
        stateSlot = "S0"
      }
      else
      {
        nodeList = "nodes.S1"
        stateSlot = "S1"
      }
      
      
      turns = slot(slot(turnsModel, stateSlot),pathSlot)
      
      if(i==1)
      {
        idx = seq(1,boxIndices[idx_ses[i]])
      }
      else
      {
        idx = seq((boxIndices[idx_ses[i]-1]+1), boxIndices[idx_ses[i]])
      }
      
      
      enregRows = enreg[[ses]]$tab[idx,]
      
      turntimes = getTurnDuration(path, state, enregRows)
      
      
      if(length(turns) >0)
      {
        for(j in 1:length(turns))
        {
          turnTimes[turnIdx,1] = idx_ses[i]
          turnTimes[turnIdx,2] = path
          turnTimes[turnIdx,3] = state
          turnTimes[turnIdx,4] = which(slot(turnsModel, nodeList) == turns[j])
          turnTimes[turnIdx,5] = ses
          turnTimes[turnIdx,6] = turntimes[[j]]
          turnIdx = turnIdx+1
        }
      }
      
    }
    
  }
  
  turnTimes = turnTimes[-(turnIdx:totalTurns),]
  
  return(turnTimes)
}

genInitValues=function(allpaths,sim){
  H <- matrix(0,2,6)
  twoHundredActions <- allpaths[1:100,c(1,2)]
  if(sim==1){
    stateOne = 0
    stateTwo = 1
    actRange = c(0:5)
  }else{
    stateOne = 1
    stateTwo = 2
    actRange = c(1:6)
  }
  
  stateOneVisits = which(twoHundredActions[,2]==stateOne)
  stateTwoVisits = which(twoHundredActions[,2]==stateTwo)
  for(act in actRange){
    for(state in c(stateOne:stateTwo)){
      if(state == stateOne){
        actCounter = length(which(twoHundredActions[stateOneVisits,1] == act))
        if(sim == 1){
          H[(state+1),(act+1)]= actCounter/length(stateOneVisits)
        }else{
          H[state,act]= actCounter/length(stateOneVisits)
        }
        
        
      }else{
        actCounter = length(which(twoHundredActions[stateTwoVisits,1] == act))
        
        if(sim == 1){
          H[(state+1),(act+1)]= actCounter/length(stateTwoVisits)
        }else{
          H[state,act]= actCounter/length(stateTwoVisits)
        }
      }
      
    }
  }
  
  
  return(H)
}

boxplotMse = function(mat_res, model,rat){
  
  if(model == 1){
    jpeg(paste("boxplot_ACA_",rat,".jpeg",sep=""))
  }else if(model == 2){
    jpeg(paste("boxplot_GB_",rat,".jpeg",sep=""))
  }else if(model == 3){
    jpeg(paste("boxplot_GB_ACA_",rat,".jpeg",sep=""))
  }else if(model == 4){
    jpeg(paste("boxplot_ACA2_",rat,".jpeg",sep=""))
  }else if(model == 5){
    jpeg(paste("boxplot_ACA3_",rat,".jpeg",sep=""))
  }
  
  boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),as.numeric(mat_res[,5]), as.numeric(mat_res[,7]), xaxt="n")
  axis(side=1, at=c(1,2,3,4), labels = c("ACA","GB", "ACA2", "ACA3"))
  dev.off()
}

checkValidation=function(mat_res, model,rat){
  ret = TRUE 
  if(model==1){
    if(length(which(mat_res[,7]=="ACA")) < 70){
      print(sprintf("ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==2){
    if(length(which(mat_res[,7]=="GB")) < 70){
      print(sprintf("GB is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==3){
    if(length(which(mat_res[,7]=="ACA_GB")) < 70){
      print(sprintf("GB_ACA is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("GB_ACA is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==4){
    if(length(which(mat_res[,7]=="ACA2")) < 70){
      print(sprintf("ACA2 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA2 is selected more than 70 times for %s.",rat))
    }
  }
  else if(model==5){
    if(length(which(mat_res[,7]=="ACA3")) < 70){
      print(sprintf("ACA3 is selected less than 70 times for %s. Exiting validation",rat))
      ret = FALSE
    }else{
      print(sprintf("ACA3 is selected more than 70 times for %s.",rat))
    }
  }
  return(ret)
}


generatePlots=function(rat,window, ACAprobMatrix, GBprobMatrix,  SARSAprobMatrix, ACA3probMatrix, allpaths_num){
  
  rle_sess = rle(allpaths_num[,5])
  last_paths<-cumsum(rle_sess$lengths)
  allpaths_num<-allpaths_num[-last_paths,]
  
  empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num, window = window)
  
  state1=which(allpaths_num[,2]==1)
  state2=which(allpaths_num[,2]==2)
  rle_state1 = rle(allpaths_num[state1,5])
  rle_state2 = rle(allpaths_num[state2,5])
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("Prob_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      
      if(state==1){
        rle_sess = rle_state1
        stateidx= state1
      }else{
        rle_sess = rle_state2
        stateidx=state2
      }
      
      plot(GBprobMatrix[stateidx,(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability", xaxt='n', xlab='')
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      
      axis(1, at=seq(1,length(stateidx),by=100))
      mtext(paste("State", state, "Trials"), side = 1, line = 2, cex=0.9)
      axis(3, line=0,at=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), labels = rle_sess$values)
      mtext("Session Nb", side = 3, line = 2, cex=0.9)
      abline(v=c(1,cumsum(rle_sess$lengths[-length(rle_sess$lengths)])), lty=3)
      title(paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ), line = 3.3, cex=0.4)
      
      lines(empiricalProbMatrix[stateidx,(act+6*(state-1))],col='blue',type='l',lty=1, lwd=1)
      lines(ACAprobMatrix[stateidx,(act+6*(state-1))],col='green',type='l', lwd=1)
      lines(SARSAprobMatrix[stateidx,(act+6*(state-1))],col='orange',type='l', lwd=1)
      lines(ACA3probMatrix[stateidx,(act+6*(state-1))],col='red',type='l', lwd=1)
      
      
      if(act==4||act==10){
        legend("bottomright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,1))
        
      }else{
        legend("topright", legend=c("Prob. of reward for GB", "Prob. of reward for ACA","Prob. of reward for SARSA","Prob. of reward for ACA3", "Empirical prob."),col=c("black","green","orange","red", "blue"),cex=0.8,lty = c(1,1,1,1,1))
        
      }
      
      
      dev.off()
    }
  }
  
}

generateModelProbPlots=function(rat, window, res1, res2,models, allpaths_num){
  
  rle_sess = rle(allpaths_num[,5])
  last_paths<-cumsum(rle_sess$lengths)
  #allpaths_num1<-allpaths_num[-last_paths,]
  
  empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num, window = window)
  
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      pdf(file=paste("Prob_",rat,"_Path", act, "_State",state,".pdf",sep=""))
      
      
      cols <- brewer.pal(8,'Dark2')
      
      if(act==4||act==1)
      {
        ylim=c(0,1)
      }
      else
      {
        ylim=c(0,0.6)
      }
      
      ### Ugly - update this "aca3TurnData"
      if(any(grepl("Turns",models)))
      {
        pathids = res2$aca3TurnData@ProbMatrix[,17]
        uniqPathIds = unique(pathids)
        #uniqPathIds = uniqPathIds[which(uniqPathIds %in% allpaths_num[,6])]
        empiricalMatIdx = which(allpaths_num[,6] %in% uniqPathIds & allpaths_num[,2] == state)
        pathIdsInState = allpaths_num[which(allpaths_num[,6] %in% uniqPathIds & allpaths_num[,2] == state),6]
      }
      else
      {
        empiricalMatIdx = which(allpaths_num[,2]==state)
        pathIdsInState = allpaths_num[which(allpaths_num[,2]==state),6]
      }
      
      if(state==1)
      {
        box = "box E"
      }
      else
      {
        box = "box I"
      }
      actNb = act+ ((state-1)*6)
      plot(1, type="n", xlab="Trials", ylab="Probability", xlim=c(0, length(which(allpaths_num[,2]==state))), ylim=ylim, main = paste0(rat,": Path ",actNb,", ",box),cex.lab=1.3)
      lines(empiricalProbMatrix[empiricalMatIdx,(act+6*(state-1))])
      
      i=0
      for(m in models)
      {
        i = i+1
        if(m == "aca")
        {
          probmatrix = res1$acamse@ProbMatrix
        }
        else if(m == "gb")
        {
          probmatrix = res1$gbmse@ProbMatrix
        }
        else if(m == "aca2")
        {
          probmatrix = res1$aca2mse@ProbMatrix
        }
        else if(m == "aca3")
        {
          probmatrix = res1$aca3mse@ProbMatrix
        }
        else if(m == "sarsa")
        {
          probmatrix = res1$sarsamse@ProbMatrix
        }
        else if(m == "acaTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$acaTurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "gbTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$gbTurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "aca2Turns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$aca2TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "aca3Turns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$aca3TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        else if(m == "sarsaTurns")
        {
          probmatrix = TurnsModels::getPathProbMatrix(res2$TurnData@ProbMatrix,allpaths_num,sim = 2)
        }
        
        
        lines(probmatrix[which(probmatrix[,13] %in% pathIdsInState),(act+6*(state-1))],col=cols[i],ylab="Probability",lwd=2)
        
      }
      modelnames = models
      turnIndices = grep("Turns",modelnames)
      modelnames[-turnIndices] = paste(modelnames[-turnIndices],"Paths",sep="")
      if(act ==4)
      {
        legend("bottomright", legend=c(modelnames,"Empirical"),col=c(cols[1:i],cols[8]),cex=1.5,lty = rep(1,(i+1)),lwd=2)
      }
      else
      {
        legend("topright", legend=c(modelnames,"Empirical"),col=c(cols[1:i],cols[8]),cex=1.5,lty = rep(1,(i+1)),lwd=2)
      }
      
      dev.off()
    }
  }
  
}

generateModelProbPlots2=function(rat, window, res1, res2,models, allpaths_num,path){
  
  rle_sess = rle(allpaths_num[,5])
  last_paths<-cumsum(rle_sess$lengths)
  allpaths_num1<-allpaths_num[-last_paths,]
  
  empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num1, window = window)
  #setwd(path)
  dirpath1 = file.path(path,rat)
  #print(sprintf("Creating dir %s",dirpath1))
  dir.create(dirpath1)
  
  
  for(m in models)
  {
    dirpath2=file.path(dirpath1,m)
    #print(sprintf("subdir %s",dirpath2))
    dir.create(dirpath2)
    #print(sprintf("Setting path %s",dirpath2))
    setwd(dirpath2)
    
    if(m == "aca")
    {
      probmatrix = res1$acamse@ProbMatrix
    }
    else if(m == "gb")
    {
      probmatrix = res1$gbmse@ProbMatrix
    }
    else if(m == "aca2")
    {
      probmatrix = res1$aca2mse@ProbMatrix
    }
    else if(m == "aca3")
    {
      probmatrix = res1$aca3mse@ProbMatrix
    }
    else if(m == "sarsa")
    {
      probmatrix = res1$sarsamse@ProbMatrix
    }
    else if(m == "acaTurns")
    {
      probmatrix = TurnsModels::getPathProbMatrix(res2$acaTurnData@ProbMatrix,allpaths_num,sim = 2)
      probmatrix2 = res2$acaTurnData@ProbMatrix
    }
    else if(m == "gbTurns")
    {
      probmatrix = TurnsModels::getPathProbMatrix(res2$gbTurnData@ProbMatrix,allpaths_num,sim = 2)
      probmatrix2 = res2$gbTurnData@ProbMatrix
    }
    else if(m == "aca2Turns")
    {
      probmatrix = TurnsModels::getPathProbMatrix(res2$aca2TurnData@ProbMatrix,allpaths_num,sim = 2)
      probmatrix2 = res2$aca2TurnData@ProbMatrix
    }
    else if(m == "aca3Turns")
    {
      probmatrix = TurnsModels::getPathProbMatrix(res2$aca3TurnData@ProbMatrix,allpaths_num,sim = 2)
      probmatrix2 = res2$aca3TurnData@ProbMatrix
    }
    else if(m == "sarsaTurns")
    {
      probmatrix = TurnsModels::getPathProbMatrix(res2$sarsaTurnData@ProbMatrix,allpaths_num,sim = 2)
      probmatrix2 = res2$sarsaTurnData@ProbMatrix
    }
    
    
    
    for(act in c(1:6)){
      for(state in c(1:2)){
        
        
        pdf(file=paste("Prob_",rat,"_Path", act, "_State",state,".pdf",sep=""))
        
        if(act==4||act==1)
        {
          ylim=c(0,1)
        }
        else
        {
          ylim=c(0,0.6)
        }
        if(m == "acaTurns" ||m == "gbTurns"|| m == "aca2Turns"||m == "aca3Turns"||m == "sarsaTurns")
        {
          if(state==1)
          {
            if(act==1)
            {
              pathIdx = probmatrix2[which(probmatrix2[,2]!=-1),17]
            }
            else if(act==2)
            {
              pathIdx = probmatrix2[which(probmatrix2[,3]!=-1),17]
            }  
            else if(act==3)
            {
              pathIdx = probmatrix2[which(probmatrix2[,5]!=-1),17]
            }  
            else if(act==4)
            {
              pathIdx = probmatrix2[which(probmatrix2[,8]!=-1),17]
            }  
            else if(act==5)
            {
              pathIdx = probmatrix2[which(probmatrix2[,7]!=-1),17]
            }  
          }
          else
          {
            
            if(act==1)
            {
              pathIdx = probmatrix2[which(probmatrix2[,10]!=-1),17]
            }
            else if(act==2)
            {
              pathIdx = probmatrix2[which(probmatrix2[,11]!=-1),17]
            }  
            else if(act==3)
            {
              pathIdx = probmatrix2[which(probmatrix2[,14]!=-1),17]
            }  
            else if(act==4)
            {
              pathIdx = probmatrix2[which(probmatrix2[,15]!=-1),17]
            }  
            else if(act==5)
            {
              pathIdx = probmatrix2[which(probmatrix2[,16]!=-1),17]
            }  
          }
          
          
          empIdx = which(allpaths_num1[,6] %in% pathIdx)
          
        }
        else
        {
          empIdx = which(allpaths_num1[,2]==state)
        }
        
        
        plot(empiricalProbMatrix[empIdx,(act+6*(state-1))],type='l', xlab="Trials", ylab="Probability", xlim=c(0, length(which(allpaths_num[,2]==state))), ylim=ylim, main = paste0(rat,": Path ",act," State ",state),cex.lab=1.3)
        lines(probmatrix[which(probmatrix[,(act+6*(state-1))]>0),(act+6*(state-1))],col="red",ylab="Probability",lwd=2)
        
        if(act ==4)
        {
          legend("bottomright", legend=c(m,"Empirical"),col=c("red","black"),cex=1.5,lty = rep(1,(i+1)),lwd=2)
        }
        else
        {
          legend("topright", legend=c(m,"Empirical"),col=c("red","black"),cex=1.5,lty = rep(1,(i+1)),lwd=2)
        }
        
        dev.off()
      }
    }
    
    
  }
  
  
  
}


getPathProb=function(probabilityMatrix){
  
  for(state in c(1:2))
  {
    if(state==1)
    {
      probmatrix = probabilityMatrix[,1:8]
      path1ProbVec = probmatrix[which(probmatrix[,2]!=-1),2] #dch
      path2ProbVec = probmatrix[which(probmatrix[,3]!=-1),3] #gak
      bak = which(probmatrix[,5]!=-1)
      path3ProbVec = probmatrix[(bak-1),1] * probmatrix[bak,5] 
      bch = which(probmatrix[,8]!=-1)
      path4ProbVec = probmatrix[(bch-1),4]*probmatrix[bch,8]
      bcd = which(probmatrix[,7]!=-1)
      path5ProbVec = probmatrix[(bcd-1),4]*probmatrix[bcd,7]
      
      matlen = max(length(path1ProbVec),length(path2ProbVec),length(path3ProbVec),length(path4ProbVec),length(path5ProbVec))
      probMat1 = matrix(0,matlen,6)
      probMat1[(1:length(path1ProbVec)),1]=path1ProbVec
      probMat1[(1:length(path2ProbVec)),2]=path2ProbVec
      probMat1[(1:length(path3ProbVec)),3]=path3ProbVec
      probMat1[(1:length(path4ProbVec)),4]=path4ProbVec
      probMat1[(1:length(path5ProbVec)),5]=path5ProbVec
      
    }
    else
    {
      probmatrix = probabilityMatrix[,9:16]
      path1ProbVec = probmatrix[which(probmatrix[,2]!=-1),2] #dch
      path2ProbVec = probmatrix[which(probmatrix[,3]!=-1),3] #gak
      bag = which(probmatrix[,6]!=-1)
      path3ProbVec = probmatrix[(bag-1),1] * probmatrix[bag,6] 
      bcd = which(probmatrix[,7]!=-1)
      path4ProbVec = probmatrix[(bcd-1),4]*probmatrix[bcd,7]
      bch = which(probmatrix[,8]!=-1)
      path5ProbVec = probmatrix[(bch-1),4]*probmatrix[bch,8]
      
      matlen = max(length(path1ProbVec),length(path2ProbVec),length(path3ProbVec),length(path4ProbVec),length(path5ProbVec))
      probMat2 = matrix(0,matlen,6)
      probMat2[(1:length(path1ProbVec)),1]=path1ProbVec
      probMat2[(1:length(path2ProbVec)),2]=path2ProbVec
      probMat2[(1:length(path3ProbVec)),3]=path3ProbVec
      probMat2[(1:length(path4ProbVec)),4]=path4ProbVec
      probMat2[(1:length(path5ProbVec)),5]=path5ProbVec
    }
    
  }
  max_len = max(length(probMat1[,1]),length(probMat2[,1]))
  probMat = matrix(0,max_len,12)
  
  probMat[(1:length(probMat1[,1])),1:6]=probMat1
  probMat[(1:length(probMat2[,1])),7:12]=probMat2
  
  return(probMat)
  
}

generateTurnPlots=function(rat,window, res2, turnTimes){
  
  # rle_sess = rle(allpaths_num[,5])
  # last_paths<-cumsum(rle_sess$lengths)
  # allpaths_num<-allpaths_num[-last_paths,]
  rle_sess = rle(turnTimes[,5])
  last_paths<-cumsum(rle_sess$lengths)
  turnTimes1 =  turnTimes[-c(last_paths, (last_paths-1)),]
  
  #empiricalProbMatrix = baseModels::empiricalProbMat(allpaths_num, window = window)
  
  state1=which(turnTimes1[,3]==1)
  state2=which(turnTimes1[,3]==2)
  # rle_state1 = rle(allpaths_num[state1,5])
  # rle_state2 = rle(allpaths_num[state2,5])
  
  
  
  for(state in c(1:2))
  {
    pdf(file=paste("Prob_",rat,"_at_E_State",state,".pdf",sep=""))
    layout(matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), heights=c(4,4,1))
    par(mai=rep(0.5, 4))
    
    for(turn in c(1:4))
    {
      turnIdx = turn + 8*(state-1)
      turnName = TurnsModels::getTurnString(turnIdx)
      
      plot(GBprobMatrix[which(GBprobMatrix[,turnIdx]!=-1),turnIdx],col='black',type='l',ylim=c(0,1),ylab="Probability", xlab=paste("State", state, "Trials"))
      title(paste("Probability of \"",turnName,"\",State ", state, ",", rat,sep="" ), line = 1, cex=0.4)
      
      #lines(empiricalProbMatrix[stateidx,(act+6*(state-1))],col='blue',type='l',lty=1, lwd=1)
      lines(ACAprobMatrix[which(ACAprobMatrix[,turnIdx]!=-1),turnIdx],col='green',type='l', lwd=1)
      lines(SARSAprobMatrix[which(SARSAprobMatrix[,turnIdx]!=-1),turnIdx],col='blue',type='l', lwd=1)
      lines(ACA3probMatrix[which(ACA3probMatrix[,turnIdx]!=-1),turnIdx],col='red',type='l', lwd=1)
      
    }
    par(mai=c(0,0,0,0))
    plot.new()
    legend("center", legend=c("Prob. of GB", "Prob. of ACA","Prob. of SARSA","Prob. of ACA3"),col=c("black","green","blue", "red"),cex=0.8,lty = c(1,1,1,1), ncol=4)
    dev.off()
    
    
    
    pdf(file=paste("Prob_",rat,"_at_A_State",state,".pdf",sep=""))
    layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4,1))
    par(mai=rep(0.5, 4))
    
    for(turn in c(5:6)){
      turnIdx = turn + 8*(state-1)
      turnName = TurnsModels::getTurnString(turnIdx)
      
      plot(GBprobMatrix[which(GBprobMatrix[,turnIdx]!=-1),turnIdx],col='black',type='l',ylim=c(0,1),ylab="Probability", xlab=paste("State", state, "Trials"))
      title(paste("Probability of \"",turnName,"\",State ", state, ",", rat,sep="" ), line = 1, cex=0.4)
      
      #lines(empiricalProbMatrix[stateidx,(act+6*(state-1))],col='blue',type='l',lty=1, lwd=1)
      lines(ACAprobMatrix[which(ACAprobMatrix[,turnIdx]!=-1),turnIdx],col='green',type='l', lwd=1)
      lines(SARSAprobMatrix[which(SARSAprobMatrix[,turnIdx]!=-1),turnIdx],col='blue',type='l', lwd=1)
      lines(ACA3probMatrix[which(ACA3probMatrix[,turnIdx]!=-1),turnIdx],col='red',type='l', lwd=1)
      
    }
    par(mai=c(0,0,0,0))
    plot.new()
    legend("center", legend=c("Prob. of GB", "Prob. of ACA","Prob. of SARSA","Prob. of ACA3"),col=c("black","green","red", "blue"),cex=0.8,lty = c(1,1,1,1), ncol=4)
    dev.off()
    
    
    
    pdf(file=paste("Prob_",rat,"_at_C_State",state,".pdf",sep=""))
    layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4,1))
    par(mai=rep(0.5, 4))
    
    for(turn in c(7:8)){
      turnIdx = turn + 8*(state-1)
      turnName = TurnsModels::getTurnString(turnIdx)
      
      plot(GBprobMatrix[which(GBprobMatrix[,turnIdx]!=-1),turnIdx],col='black',type='l',ylim=c(0,1),ylab="Probability", xlab=paste("State", state, "Trials"))
      title(paste("Probability of \"",turnName,"\",State ", state, ",", rat,sep="" ), line = 1, cex=0.4)
      
      #lines(empiricalProbMatrix[stateidx,(act+6*(state-1))],col='blue',type='l',lty=1, lwd=1)
      lines(ACAprobMatrix[which(ACAprobMatrix[,turnIdx]!=-1),turnIdx],col='green',type='l', lwd=1)
      lines(SARSAprobMatrix[which(SARSAprobMatrix[,turnIdx]!=-1),turnIdx],col='blue',type='l', lwd=1)
      lines(ACA3probMatrix[which(ACA3probMatrix[,turnIdx]!=-1),turnIdx],col='red',type='l', lwd=1)
      
    }
    par(mai=c(0,0,0,0))
    plot.new()
    legend("center", legend=c("Prob. of GB", "Prob. of ACA","Prob. of SARSA","Prob. of ACA3"),col=c("black","green","red", "blue"),cex=0.8,lty = c(1,1,1,1), ncol=4)
    dev.off() 
  }
  
  
}



generateEmpiricalPlots=function(rat,empiricalProbMatrix2,endLearningStage){
  x2<-empiricalProbMatrix2[,13]
  x2_rle <- rle(x2)
  x1<-cumsum(empiricalProbMatrix2[,15]-empiricalProbMatrix2[,14]+1)
  endLearningIdx = findInterval(endLearningStage, x1)
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("EmpProb_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      
      plot(empiricalProbMatrix2[,(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability", xaxt='n', xlab='Session Nb')
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      
      axis(1, line=0,at=cumsum(x2_rle$lengths), labels = x2_rle$values)
      abline(v=cumsum(x2_rle$lengths), lty=3)
      abline(v=endLearningIdx,col='red',lwd=3)
      title(paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ), line = 2, cex=0.4)
      
      dev.off()
    }
  }
  
}

getEmpProbMat=function(allpaths,window,sim){
  colLen = length(allpaths[,1])
  empProbMat = numeric(colLen)
  stateOne = 1
  stateTwo = 2
  if(sim == 1){
    stateOne = 0
    stateTwo = 1
  }
  state1Idx <- which(allpaths[,2]==stateOne)
  state2Idx <- which(allpaths[,2]==stateTwo)
  
  for(trial in c(1:colLen)){
    
    action = allpaths[trial,1]
    state = allpaths[trial,2]
    curr_state_idx <- which(allpaths[1:trial,2]==state)
    if(length(curr_state_idx) > window){
      curr_state_idx <- curr_state_idx[(length(curr_state_idx)-window):length(curr_state_idx)]
      
    }
    empProbMat[trial] <- length(which(allpaths[curr_state_idx,1]== action))/length(curr_state_idx)
    
  }
  return(empProbMat)
}

getStartIndex = function(generated_data){
  start_index=0
  l<-which(SMA(generated_data[,3],30)>=0.6)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      start_index=k[[set]][1]
      break
    }
  }
  return(start_index)
}

getEndIndex = function(generated_data, sim, limit){
  if(sim==1){
    generated_data[,1:2] = generated_data[,1:2] + 1
  }
  end_index1=0
  s1 <- which(generated_data[,2]==1)
  l<-which(SMA(generated_data[s1,3],30)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>20){
      end_index1=k[[set]][1]
      break
    }
  }
  
  
  end_index2=0
  s2 <- which(generated_data[,2]==2)
  l<-which(SMA(generated_data[s2,3],30)>=limit)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>20){
      end_index2=k[[set]][1]
      break
    }
  }
  
  if(end_index1==0 || end_index2 ==0){
    end_index = -1
  }else{
    end_index = max(s1[end_index1],s2[end_index2])
  }
  
  #print(sprintf("end_index=%i", end_index))
  
  return(end_index)
}

simulateTurnTime=function(turnTimes, allpaths,turnId, turnNb)
{
  grp1 = c(0,2,7,8,10,15)
  grp2 = c(1,5,6,9,13,14)
  grp3 = c(3,4,11,12)
  
  endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
  turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
  endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
  turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
  endStage3 = length(allpaths[,1])
  turnIdxStage3 = length(turnTimes[,1])
  #pathstages=c(1,endStage1,endStage2,endStage3)
  turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
  
  out<-cut(turnNb, breaks=turnstages,right = FALSE,include.lowest = TRUE)
  as.numeric(out)
  start = turnstages[as.numeric(out)]
  end = turnstages[as.numeric(out)+1]-1
  
  if(turnId %in% grp1)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp1),6]
  }
  else if(turnId %in% grp2)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp2),6] 
  }
  else if(turnId %in% grp3)
  {
    x=turnTimes[which(turnTimes[start:end,4] %in% grp3),6] 
  }
  x=x[!x %in% boxplot.stats(x)$out]
  sampledTime = sample(x,1)
  return(sampledTime)
}


simulatePathTime=function(turnTimes, allpaths,path, pathNb)
{
  grp1 = c(1)
  grp2 = c(2,3,4,5,6)
  
  endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
  turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
  endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
  turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
  endStage3 = length(allpaths[,1])
  turnIdxStage3 = length(turnTimes[,1])
  
  pathstages=c(1,endStage1,endStage2,endStage3)
  #turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
  
  out<-cut(pathNb, breaks=pathstages,right = FALSE,include.lowest = TRUE)
  as.numeric(out)
  start = pathstages[as.numeric(out)]
  end = pathstages[as.numeric(out)+1]-1
  
  if(path %in% grp1)
  {
    allpathIdx=which(allpaths[start:end,1] %in% grp1)
  }
  else if(path %in% grp2)
  {
    allpathIdx=which(allpaths[start:end,1] %in% grp2)
  }
  x=allpaths[allpathIdx,4]
  remove = which(x %in% boxplot.stats(x)$out)
  allpathIdx = allpathIdx[-remove]
  sampledId = sample(allpathIdx,1)
  pathNbSelected = allpaths[sampledId,6]
  turndurations = turnTimes[which(turnTimes[,1]==pathNbSelected),6]
  return(turndurations)
}

enregCombine=function(enreg,rat){
  allpaths <- matrix("",0,4)
  colnames(allpaths) <- c("Path","Time","boxId","sessNb")
  boxTimes <- vector()
  ### Loop through all enreg[[ses]] of current rat
  for(ses in 1:length(enreg)){
    
    allpaths_ses = strsplit(enreg[[ses]]$short,"(?<=[ei])(?=(jk)|(ja)|(jb)|(fg)|(fb)|(fa)|(dc)|(hc)|(jik))",perl=TRUE)[[1]]
    boxIndices = c(1,cumsum(nchar(allpaths_ses)))
    allpaths_ses = cbind(allpaths_ses,rep(0,length(allpaths_ses)),rep(0,length(allpaths_ses)))
    
    for(i in 1:(length(boxIndices)-1))
    {
      range = boxIndices[i+1]-boxIndices[i]
      if(range > 0)
      {
        range = range-1
      }
      idx = seq((boxIndices[i+1]-range), boxIndices[i+1])
      enregRows = enreg[[ses]]$tab[idx,]
      if(is.matrix(enregRows))
      {
        pathTime = sum((enregRows[,2]-enregRows[,1])[-length(enregRows[,2])]) ## Remove last box which is i/e
      }
      else
      {
        pathTime = enregRows[2] - enregRows[1]
      }
      
      allpaths_ses[i,2] = pathTime
      allpaths_ses[i,3] = boxIndices[i+1]
      
    }
    
    allpaths_ses = cbind(allpaths_ses,ses)
    allpaths <- rbind(allpaths,allpaths_ses)
  }
  
  return(list("allpaths" = allpaths, "boxTimes" = boxTimes))
}


populateRatModel=function(rat,allpaths,enreg,turnsModel)
{
  
  allpaths = updateACAPathNbmse(allpaths)
  
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(5,7,6,2,4)])),nrow=nrow(allpaths[,c(5,7,6,2,4)]))
  allpaths_num = cbind(allpaths_num,c(1:length(allpaths_num[,1])))
  allpaths_num = cbind(allpaths_num,as.numeric(allpaths[,3]))
  turnTimes = getTurnsMatrix(allpaths,enreg,turnsModel)
  
  ratdata = new("RatData", rat = rat,allpaths = allpaths_num,turnTimes = turnTimes)
  
  return(ratdata)
  
}

locate_xtrem <- function (x, last = FALSE)
{
  # use rle to deal with duplicates
  x_rle <- rle(x)
  
  # force the first value to be identified as an extrema
  first_value <- x_rle$values[1] - x_rle$values[2]
  
  # differentiate the series, keep only the sign, and use 'rle' function to
  # locate increase or decrease concerning multiple successive values.
  # The result values is a series of (only) -1 and 1.
  #
  # ! NOTE: with this method, last value will be considered as an extrema
  diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()
  
  # this vector will be used to get the initial positions
  diff_idx <- cumsum(diff_sign_rle$lengths)
  
  # find min and max
  diff_min <- diff_idx[diff_sign_rle$values < 0]
  diff_max <- diff_idx[diff_sign_rle$values > 0]
  
  # get the min and max indexes in the original series
  x_idx <- cumsum(x_rle$lengths)
  if (last) {
    min <- x_idx[diff_min]
    max <- x_idx[diff_max]
  } else {
    min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
    max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
  }
  # just get number of occurences
  min_nb <- x_rle$lengths[diff_min]
  max_nb <- x_rle$lengths[diff_max]
  
  # format the result as a tibble
  bind_rows(
    tibble(Idx = min, Values = x[min], NB = min_nb, Status = "min"),
    tibble(Idx = max, Values = x[max], NB = max_nb, Status = "max")) %>%
    arrange(.data$Idx) %>%
    mutate(Last = last) %>%
    mutate_at(vars(.data$Idx, .data$NB), as.integer)
}


computeActivity = function(probVector, window){
  
  activityVec = numeric(length(c(probVector)))
  for(i in c(2:(length(probVector)))){
    
    if(i <= window){
      if(sum(probVector[1:i]) != 0 && length(unique(round(probVector[1:i],4))) != 1){
        res = locate_xtrem(round(probVector[1:i],4))
        activityVec[i] = sum(abs(diff(res$Values)))
      }
      
    }else{
      
      if(sum(probVector[(i-window):i]) != 0 && length(unique(round(probVector[(i-window):i],4))) != 1){
        res = locate_xtrem(round(probVector[(i-window):i],4))
        activityVec[i] = sum(abs(diff(res$Values)))
      }
      
    }
  }
  
  # if(!all(probVector == 0)){
  #   res = locate_xtrem(round(probVector,4))
  #   activitySum = sum(abs(diff(res$Values)))
  # }else{
  #   activitySum = 0
  # }
  
  return(activityVec)
}

plotData = function(res,rat, ranges){
  
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$acamse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$acamse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA"))
      lines(res$acamse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$acamse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$gbmse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$gbmse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " GB"))
      lines(res$gbmse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$gbmse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca2mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$aca2mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA2"))
      lines(res$aca2mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca2mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca3mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    par(mfrow=c(3,2))
    for(act in c(1:6)){
      plot(res$aca3mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA3"))
      lines(res$aca3mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca3mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  
}


plotData2 = function(res,rat, ranges){
  
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$acamse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$acamse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA"))
      lines(res$acamse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$acamse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$gbmse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$gbmse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " GB"))
      lines(res$gbmse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$gbmse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca2mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$aca2mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA2"))
      lines(res$aca2mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca2mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  for(state in c(1:2)){
    jpeg(paste("Activity_",res$aca3mse@Name, "_", rat,"_State_",state,".jpeg",sep=""))
    for(act in c(4)){
      plot(res$aca3mse@Actions[[(6*(state-1)+act)]]@empActivity, type='l',ylab="Activity",main=paste0("Path ",act, " ACA3"))
      lines(res$aca3mse@Actions[[(6*(state-1)+act)]]@modelActivity,col='red',lty=3)
      if(state==1){
        abline(v=ranges$state1$start_index,col='green',lty=2)
        abline(v=ranges$state1$end_index,col='green',lty=2)
      }else{
        abline(v=ranges$state2$start_index,col='green',lty=2)
        abline(v=ranges$state2$end_index,col='green',lty=2)
      }
    }
    title(paste0(rat," ",res$aca3mse@Name, "_Activity,  State ", state), outer = TRUE, line = -1)
    dev.off()
  }
  
  
}
