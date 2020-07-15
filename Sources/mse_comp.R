library(Rmpfr)
library(DEoptim)
library(TTR)


R=matrix(0,nrow=2,ncol=6)
colnames(R)<-c("Path1","Path2","Path3","CorrPath","WM-Path","Unknown-Paths")
rownames(R)<-c("E","I")
R[1,4]=1
R[2,4]=1

### Use optimal paramters on actual data and compute Mean Squared Error.
mse_compare=function(enreg,rat){
  
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
    allpaths_ses<-strsplit(allpaths_ses,"(?<=[ei])(?=(, j, k)|(, j, a)|(, j, b)|(, f, g)|(, f, b)|(, f, a)|(, d, c)|(, h, c))",perl=TRUE)[[1]]
    allpaths_ses <-cbind(allpaths_ses,ses)
    colnames(allpaths_ses) <- c("Path","Session")
    l<-list(allpaths,allpaths_ses)
    allpaths <- rbind(allpaths,allpaths_ses)
    
    
  }
  allpaths = updateACAPathNbmse(allpaths)
  print(sprintf("rat:%s",rat))
  #print(getStatsBeforeRewards(allpaths))
  
  # allpaths = updateACAPathNb(allpaths)
  sourceCpp('C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/sarsa_mle.cpp')
  sourceCpp('C:/Users/matta/OneDrive/Documents/Rats-Credit/Sources/aca_mle3.cpp')
  
  enreg_comb<-matrix(, nrow = 0, ncol = 7)
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
    enreg_comb<-rbind(enreg_comb,enreg[[ses]]$POS)
  }
  l<-cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  
  y<-updateAllpaths1(as.numeric(allpaths[,2]),l)
  allpaths<-cbind(allpaths,y)
  allpaths_num <- matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))
  ################Call ACA

  # endLearningStage = 0 ## Trial nb where learning stage ends
  # l<-which(SMA(allpaths_num[,3],30)>=0.9)
  # k<-split(l, cumsum(c(1, diff(l) != 1)))
  # for(set in 1:length(k)){
  #   if(length(k[[set]])>30){
  #     endLearningStage=k[[set]][1]
  #     break
  #   }
  # }
  # 
  # Hinit <-genInitValues(allpaths_num,sim=2)
  # #Hinit <- matrix(0,2,6)
  # 
  # for(model in c(1:2)){
  #   out <- DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = model, sim=2,DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  #   alpha = out$optim$bestmem[1]
  #   mat_res <- matrix(0,nrow=100,ncol=7)
  #   colnames(mat_res) <- c("ACA MSE","ACA bestval","GB MSE","GB bestval","GB-ACA MSE","GB-ACA bestval","Selected Model")
  #   iter=1
  #   start_index=0
  #   end_index=0
  #   missedOptimalIter=0
  #   while(iter<=100){
  #     #print(sprintf("iter=%i",iter))
  #     total_trials = length(allpaths_num[,1])
  #     init_state=as.numeric(allpaths_num[1,2])-1
  #     ### Generate simulated data using the estimated params
  #     generated_data <- aca_gen_sim(allpaths_num,H=Hinit, alpha,total_trials,init_state,model=model)
  #     #generated_data <- sarsa_gen_sim(allpaths_num,alpha_mod,gamma_mod,lambda_mod,total_trials,init_state)
  # 
  # 
  #     if(length(which(SMA(generated_data[,3],10)>=0.9))<500){
  #       missedOptimalIter=missedOptimalIter+1
  #       next
  #     }
  # 
  #     ## all indices of allpaths_num with SMA >=0.6
  #     ## Split the indices into sets of consecutive indices
  #     ## Start index = When rewards/30 trials reaches 0.5
  #     start_index=0
  #     l<-which(SMA(generated_data[,3],30)>=0.6)
  #     k<-split(l, cumsum(c(1, diff(l) != 1)))
  #     for(set in 1:length(k)){
  #       if(length(k[[set]])>30){
  #         start_index=k[[set]][1]
  #         break
  #       }
  #     }
  #     end_index=0
  #     l<-which(SMA(generated_data[,3],30)>=0.95)
  #     k<-split(l, cumsum(c(1, diff(l) != 1)))
  #     for(set in 1:length(k)){
  #       if(length(k[[set]])>30){
  #         end_index=k[[set]][1]
  #         break
  #       }
  #     }
  # 
  #     if(end_index <= start_index){
  #       next
  #     }
  # 
  #     ### Estimate Model1 parameters for simulated data
  #     half_index = start_index
  # 
  #     Hinit1 <-genInitValues(generated_data,sim=1)
  #     #Hinit <- matrix(0,2,6)
  #     
  #     movAvg1 <- getMovingAverage(generated_data,sim=1)
  #     
  # 
  #     ACA <-DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit1, allpaths = generated_data[1:half_index,], model = 1, sim=1,DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  #     alpha_ACA = ACA$optim$bestmem[1]
  #     ### Compute MSE for Model1 using simulated data
  #     ACA_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA,H=Hinit1, model=1, sim=1)
  #     # mse_model1_mat_method1 <- computeMSE(generated_data,ACA_probMatrix,sim=1)
  #     # mse_model1_method1 <- sum(mse_model1_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
  #     mse_ACA_mat_method2 <- computeMSE3(generated_data,ACA_probMatrix,movAvg1,sim=1)
  #     mse_ACA_method2 <- sum(mse_ACA_mat_method2[(half_index+1):end_index])/(end_index-half_index)
  #     # ACA_lik_mat <-aca_mle_lik(generated_data,alpha_ACA, H=Hinit, model=1,sim=1)
  #     # ACA_lik <- (-1)*sum(ACA_lik_mat[(half_index+1):end_index])
  # 
  #     # ## Estimate Model2 parameters for simulated data
  #     GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H=Hinit1, allpaths=generated_data[1:half_index,],model = 2, sim=1, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  #     alpha_GB = GB$optim$bestmem[1]
  #     #episode_model2_est = floor(2+(out_model2$optim$bestmem[2]*28))
  #     ## Compute MSE for Model2 using simulated data
  #     GB_probMatrix <-acaGetProbMatrix(generated_data, alpha_GB, H=Hinit1, model=2,sim=1)
  #     mse_GB_mat_method2 <- computeMSE3(generated_data,GB_probMatrix,movAvg1,sim=1)
  #     mse_GB_method2 <- sum(mse_GB_mat_method2[(half_index+1):end_index])/(end_index-half_index)
  #     # GB_lik_mat <-aca_mle_lik(generated_data,alpha_GB,H=Hinit,model=2,sim=1)
  #     # GB_lik <- (-1)*sum(GB_lik_mat[(half_index+1):end_index])
  # 
  #     # ## Estimate Model2 parameters for simulated data
  #     # ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1,H=Hinit1, allpaths=generated_data[1:half_index,],model = 3, sim=1, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  #     # alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  #     # ## Compute MSE for Model2 using simulated data
  #     # GB_ACA_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA_GB,H=Hinit1,model=3,sim=1)
  #     # # mse_model2_mat_method1 <- computeMSE(generated_data,aca_model2_probMatrix,sim=1)legend("bottomright", legend=c("Prob. of reward in S2 for GB", "Prob. of reward in S2 for GB_ACA","Prob. of reward in S2 for ACA","Empirical prob. of reward in S2", "Mov. Avg of Reward/30 trials in S2"),col=c("black","red","green","orange","blue"),cex=0.6,lty = c(1,1,1,1,2))
  #     # # mse_model2_method1 <- sum(mse_model2_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
  #     # mse_GB_ACA_mat_method2 <- computeMSE3(generated_data,GB_ACA_probMatrix,movAvg1,sim=1)
  #     # mse_GB_ACA_method2 <- sum(mse_GB_ACA_mat_method2[(half_index+1):end_index])/(end_index-half_index)
  #     # ACA_GB_lik_mat <-aca_mle_lik(generated_data,alpha_ACA_GB,H=Hinit,model=3,sim=1)
  #     # ACA_GB_lik <- (-1)*sum(ACA_GB_lik_mat[(half_index+1):end_index])
  # 
  # 
  # 
  #    
  #     mat_res[iter,1]=mse_ACA_method2
  #     mat_res[iter,2]=toString(ACA$optim$bestmem)
  #     mat_res[iter,3]=mse_GB_method2
  #     mat_res[iter,4]=toString(GB$optim$bestmem)
  #     # mat_res[iter,5]=mse_GB_ACA_method2
  #     # mat_res[iter,6]=toString(ACA_GB$optim$bestmem)
  # 
  #     # if(mat_res[iter,1]==0 ||mat_res[iter,3]==0||mat_res[iter,5]==0){
  #     #   print(sprintf("Stopping as MSE =0"))
  #     #   break
  #     # }
  # 
  #     index_min=which.min(c(mat_res[iter,1],mat_res[iter,3]))
  #     if(index_min==1){
  #       mat_res[iter,7]="ACA"
  #       #break
  #     }
  #     else if(index_min==2){
  #       mat_res[iter,7]="GB"
  #       #break
  #     }
  #     # else if(index_min==3){
  #     #   mat_res[iter,7]="ACA_GB"
  #     # }
  # 
  #     iter=iter+1
  # 
  #   }
  #   print(sprintf("Nb of iterations where optimal behaviour was not learned=%i",missedOptimalIter))
  # 
  #   if(model==1){
  #     jpeg(paste("boxplot_ACA_",rat,".jpeg",sep=""))
  #   }else if(model==2){
  #     jpeg(paste("boxplot_GB_",rat,".jpeg",sep=""))
  #    }
  #   # else if(model==3){
  #   #    jpeg(paste("boxplot_GB_ACA_",rat,".jpeg",sep=""))
  #   #  }
  # 
  #   boxplot(as.numeric(mat_res[,1]),as.numeric(mat_res[,3]),xaxt="n")
  #   axis(side=1, at=c(1,2), labels = c("ACA","GB"))
  #   dev.off()
  # 
  #   if(model==1){
  #     if(length(which(mat_res[,7]=="ACA")) < 70){
  #       print(sprintf("ACA is selected less than 70 times for %s. Exiting validation",rat))
  #       break
  #     }else{
  #       print(sprintf("ACA is selected more than 70 times for %s.",rat))
  #     }
  #   }
  #   else if(model==2){
  #     if(length(which(mat_res[,7]=="GB")) < 70){
  #       print(sprintf("GB is selected less than 70 times for %s. Exiting validation",rat))
  #       break
  #     }else{
  #       print(sprintf("GB is selected more than 70 times for %s.",rat))
  #     }
  #   }
  #   # else if(model==3){
  #   #   if(length(which(mat_res[,7]=="ACA_GB")) < 70){
  #   #     print(sprintf("GB_ACA is selected less than 70 times for %s. Exiting validation",rat))
  #   #     break
  #   #   }else{
  #   #     print(sprintf("GB_ACA is selected more than 70 times for %s.",rat))
  #   #   }
  #   # }
  # 
  # }
  # #print(sprintf("All 3  models validated for %s.",rat))

 ##### Model Selection GB vd GB_ACA on Acutal Data #########################3
  
  generated_data <- allpaths_num
  start_index=0
  l<-which(SMA(generated_data[,3],30)>=0.6)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      start_index=k[[set]][1]
      break
    }
  }
  end_index=0
  l<-which(SMA(generated_data[,3],30)>=0.95)
  k<-split(l, cumsum(c(1, diff(l) != 1)))
  for(set in 1:length(k)){
    if(length(k[[set]])>30){
      end_index=k[[set]][1]
      break
    }
  }
  
  if(end_index <= start_index){
    next
  }
  
  ### Estimate Model1 parameters for simulated data
  half_index = start_index
  #half_index=0
  
  Hinit2 <-genInitValues(generated_data,sim=2)
  #Hinit <- matrix(0,2,6)
  movAvg2 <- getMovingAverage(generated_data,sim=2)
  
  ACA <-DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit2, allpaths = generated_data[1:half_index,], model = 1, sim=2,DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  alpha_ACA = ACA$optim$bestmem[1]
  ### Compute MSE for Model1 using simulated data
  #ACA_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA,H=Hinit2, model=1, sim=2)
  #mse_ACA_mat <- computeMSE3(generated_data,ACA_probMatrix,movAvg2,sim=2)
  #mse_ACA <- sum(mse_ACA_mat[(half_index+1):end_index])/(end_index-half_index)
  ACA_lik_mat <-aca_mle_lik(generated_data,alpha_ACA, H=Hinit2, model=1,sim=2)
  ACA_lik <- (-1)*sum(ACA_lik_mat[(half_index+1):end_index])
  
  # ## Estimate Model2 parameters for simulated data
  GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H=Hinit2, allpaths=generated_data[1:half_index,],model = 2, sim=2, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  alpha_GB = GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  #GB_probMatrix <-acaGetProbMatrix(generated_data, alpha_GB, H=Hinit2, model=2,sim=2)
  #mse_GB_mat <- computeMSE3(generated_data,GB_probMatrix,movAvg2,sim=2)
  #mse_GB <- sum(mse_GB_mat[(half_index+1):end_index])/(end_index-half_index)
  GB_lik_mat <-aca_mle_lik(generated_data,alpha_GB,H=Hinit2,model=2,sim=2)
  GB_lik <- (-1)*sum(GB_lik_mat[(half_index+1):end_index])
  
  # # ## Estimate Model2 parameters for simulated data
  # ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1,H=Hinit2, allpaths=generated_data[1:half_index,],model = 3, sim=2, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  # alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  # ## Compute MSE for Model2 using simulated data
  # GB_ACA_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA_GB,H=Hinit2,model=3,sim=2)
  # mse_GB_ACA_mat <- computeMSE3(generated_data,GB_ACA_probMatrix,movAvg2,sim=2)
  # mse_GB_ACA <- sum(mse_GB_ACA_mat[(half_index+1):end_index])/(end_index-half_index)
  # # ACA_GB_lik_mat <-aca_mle_lik(generated_data,alpha_ACA_GB,H=Hinit,model=3,sim=2)
  # # ACA_GB_lik <- (-1)*sum(ACA_GB_lik_mat[(half_index+1):end_index])
  
  #print(sprintf("For %s, ACA likelihood = %f, GB likelihood = %f, GB_ACA likelihood = %f", rat, ACA_lik, GB_lik, ACA_GB_lik))
  #print(sprintf("For %s, ACA MSE = %f, GB MSE = %f", rat, mse_ACA, mse_GB))
  print(sprintf("For %s, ACA likelihood = %f, GB likelihood = %f", rat, ACA_lik, GB_lik))
  
  #index_min=which.min(c(mse_ACA,mse_GB))
  index_min=which.min(c(ACA_lik,GB_lik))
  
  if(index_min==1){
    print(sprintf("ACA is best fit for %s", rat))
  }else if(index_min==2){
    print(sprintf("GB is best fit for %s", rat))
   }
  #else if(index_min==3){
  #   print(sprintf("GB_ACA is best fit for %s", rat))
  # }
  # 
  #generatePlots(rat,allpaths_num,ACA_probMatrix, GB_probMatrix)
  
  
  
}

getExponent=function(x){
  return(ifelse(x == 0, 0, floor(log10(abs(x)))+1 ))
}


aca_negLogLik1 <- function(par,Hinit, allpaths,model,sim) {
  
  alpha <- par[1]
  #epsLim <- 2+(par[2]*28)
  #epsLim <- 1
  #print(sprintf("epsLim=%f",epsLim))
  H <- Hinit
  # H[1,1]=3
  # H[2,1]=3
  # H[1,1:6]<-par[3:8]
  # H[2,1:6] <- par[9:14]
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- aca_mle_lik(allpaths,alpha,H,model,sim)
  negLogLik <- (-1) *sum(lik)
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}

sarsa_negLogLik <- function(par,allpaths,sim) {
  
  alpha <- par[1]
  gamma <- par[2]
  lambda <- par[3]
  #print(sprintf("epsLim=%f",epsLim))
  Q_vals <- matrix(0,2,6)
  # H[1,1:6]<-par[3:8]
  # H[2,1:6] <- par[9:14]
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- sarsa_mle(allpaths,alpha,gamma,lambda,Q_vals,sim)
  negLogLik <- (-1) *sum(lik)
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}

updateACAPathNbmse=function(allpaths){
  allpaths <- cbind(allpaths,Path=0,Reward=0,State=0)
  for(i in 1:(length(allpaths[,1]))){
    ses=as.numeric(allpaths[i,"Session"])
    trial=i-which(allpaths[,"Session"]==ses)[1]+1
    l<-which(as.numeric(enreg[[ses]]$POS[,"trial"])==trial)
    R=sum(as.numeric(enreg[[ses]]$POS[l,"Reward"]))
    if(R>0){
      allpaths[i,4] = 1
    }else{
      allpaths[i,4] = 0
    }
    allpaths[i,3] = getPathNumber(allpaths[i,1])
    if(grepl("^, f",allpaths[i,1])||grepl("^, d",allpaths[i,1])){
      allpaths[i,5]=1
    }else if(grepl("^, h",allpaths[i,1])||grepl("^, j",allpaths[i,1])){
      allpaths[i,5]=2
    }
    ## (to assign states for incomplete paths seen at the end/begining of records)
    else if(i>1){
      if(grepl("^.*e$",allpaths[i-1,1])){
        allpaths[i,5]=1
      }else if(grepl("^.*i$",allpaths[i-1,1])){
        allpaths[i,5]=2
      }
      ## If cannot be estimated, then do by default : assume the trial = Path5
      else if(grepl("^.*e$",allpaths[i,1])){
        allpaths[i,5]=1
      }
      else if(grepl("^.*i$",allpaths[i,1])){
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


generatePlots=function(rat,allpaths,GBprobMatrix, ACAprobMatrix){
  
  for(act in c(1:6)){
    for(state in c(1:2)){
      jpeg(paste("Prob_",rat,"_Path", act, "_State",state,".jpeg",sep=""))
      plot(GBprobMatrix[which(GBprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='black',type='l',ylim=c(0,1),ylab="Probability",main=paste("Probability of selecting Path",act," in State ", state, " for ", rat,sep="" ))
      #lines(GB_ACAprobMatrix[which(GB_ACAprobMatrix[,(act+6*(state-1))]!=0),(act+6*(state-1))],col='red',type='l')
      lines(ACAprobMatrix[which(ACAprobMatrix[,act+6*(state-1)]!=0),(act+6*(state-1))],col='green',type='l')
      
      if(act==4||act==10){
        lines(movavg(allpaths[which(ACAprobMatrix[,(act+6*(state-1))]!=0),3],100),col='blue',lty=2)
        legend("bottomright", legend=c("Prob. of reward for GB", "Prob. of reward for GB_ACA","Prob. of reward for ACA", "Mov. Avg of Reward/100 trials"),col=c("black","red","green","blue"),cex=0.6,lty = c(1,1,1,2))
        
      }else{
        lines(movavg(as.numeric(allpaths[,1]== act & allpaths[,2]==state),100),col='blue',lty=2)
        legend("topright", legend=c("Prob. of reward for GB", "Prob. of reward for GB_ACA","Prob. of reward for ACA","Empirical prob."),col=c("black","red","green","blue"),cex=0.6,lty = c(1,1,1,2))
        
      }
      dev.off()
    }
  }
  
}

getMovingAverage=function(allpaths,sim){
  colLen = length(allpaths[,1])
  movingAvg = numeric(colLen)
  stateOne = 1
  stateTwo = 2
  window = 20
  if(sim == 1){
    stateOne = 0
    stateTwo = 1
  }
  state1Idx <- which(allpaths[,2]==stateOne)
  state2Idx <- which(allpaths[,2]==stateTwo)
  
  for(trial in c(1:colLen)){
    
    action = allpaths[trial,1]
    state = allpaths[trial,2]
    last_30_state_idx <- which(allpaths[1:trial,2]==state)
    if(length(last_30_state_idx) > window){
      last_30_state_idx <- last_30_state_idx[(length(last_30_state_idx)-window):length(last_30_state_idx)]
      
    }
    movingAvg[trial] <- length(which(allpaths[last_30_state_idx,1]== action))/length(last_30_state_idx)
           
  }
  return(movingAvg)
}