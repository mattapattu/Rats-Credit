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
  allpaths_num <- matrix(as.numeric(unlist(allpaths[,c(3,5,4,6)])),nrow=nrow(allpaths[,c(3,5,4,6)]))
  ################Call ACA
  ### Estimate paramteres using Model1 or Model2
  #0DEoptim(sarsa_negLogLik, lower = c(0,0,0), upper = c(1,1,1), allpaths = l, sim=1, DEoptim.control(NP = 140,F = 0.8, CR = 0.9, trace = TRUE)) 
  # out <- DEoptim(sarsa_negLogLik, lower = c(0,0,0), upper = c(1,1,1), allpaths = allpaths_num, sim=2,DEoptim.control(NP = 30,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  # alpha = out$optim$bestmem[1]
  # gamma = out$optim$bestmem[2]
  # lambda = out$optim$bestmem[3]
  
  out <- DEoptim(aca_negLogLik1, lower = c(0,0), upper = c(1,1), allpaths = allpaths_num,  model = 1, sim=2,DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  alpha = out$optim$bestmem[1]
  episode_len = out$optim$bestmem[2]

  #probACA=aca_model1(H,alpha,allpaths)
  #print(probACA)
  mat_res <- matrix(0,nrow=100,ncol=7)
  colnames(mat_res) <- c("Model1 MSE","Model1 bestval","Model2 MSE","Model2 bestval","SARSA MSE","SARSA bestval","Selected Model")
  iter=1
  start_index=0
  end_index=0
  missedOptimalIter=0
  while(iter<=100){
    #H_mod <- matrix(c(rnorm(1,mean=H[1,1],sd=0.05),rnorm(1,mean=H[1,2],sd=0.05),rnorm(1,mean=H[1,3],sd=0.05),rnorm(1,mean=H[1,4],sd=0.05),rnorm(1,mean=H[1,5],sd=0.05),rnorm(1,mean=H[1,6],sd=0.05),rnorm(1,mean=H[2,1],sd=0.05),rnorm(1,mean=H[2,2],sd=0.05),rnorm(1,mean=H[2,3],sd=0.05),rnorm(1,mean=H[2,4],sd=0.05),rnorm(1,mean=H[2,5],sd=0.05),rnorm(1,mean=H[2,6],sd=0.05)),nrow=2,ncol=6)    #H_mod <- c(runif(1,(H[1,1]-H[1,1]/10),(H[1,1]+H[1,1]/10)),runif(1,(H[1,2]-H[1,2]/10),(H[1,2]+H[1,2]/10)),runif(1,(H[1,3]-H[1,3]/10),(H[1,3]+H[1,3]/10)),runif(1,(H[1,4]-H[1,4]/10),(H[1,4]+H[1,4]/10)),runif(1,(H[1,5]-H[1,5]/10),(H[1,5]+H[1,5]/10)),runif(1,(H[1,6]-H[1,6]/10),(H[1,6]+H[1,6]/10)),runif(1,(H[2,1]-H[2,1]/10),(H[2,1]+H[2,1]/10)),runif(1,(H[2,2]-H[2,2]/10),(H[2,2]+H[2,2]/10)),runif(1,(H[2,3]-H[2,3]/10),(H[2,3]+H[2,3]/10)),runif(1,(H[2,4]-H[2,4]/10),(H[2,4]+H[2,4]/10)),runif(1,(H[2,5]-H[2,5]/10),(H[2,5]+H[2,5]/10)),runif(1,(H[2,6]-H[2,6]/10),(H[2,6]+H[2,6]/10)),nrow=2,ncol=6)
    #H_mod <- matrix(0,nrow=2,ncol=6)
    alpha_mod <- runif(1,min=(alpha-(0.03*10**getExponent(alpha))),max=(alpha+(0.03*10**getExponent(alpha))))
    episode_len_mod <- 2+(episode_len*28)
    # gamma_mod <- runif(1,min=(gamma-(0.03*10**getExponent(gamma))),max=(gamma+(0.03*10**getExponent(gamma))))
    # lambda_mod <- runif(1,min=(lambda-(0.03*10**getExponent(lambda))),max=(lambda+(0.03*10**getExponent(lambda))))
    total_trials = length(allpaths_num[,1])
    init_state=as.numeric(allpaths_num[1,2])
    ### Generate simulated data using the estimated params
    #aca_model1_sim <- aca_gen_model(H_mod,alpha,episode_len_mod,total_trials,init_state,model=1)
    episode_length <- floor(2+28*episode_len)
    generated_data <- aca_gen_sim(allpaths_num,alpha,episode_length, total_trials,init_state,model=1)
    #generated_data <- sarsa_gen_sim(allpaths_num,alpha_mod,gamma_mod,lambda_mod,total_trials,init_state)
    

    if(length(which(SMA(generated_data[,3],10)>=0.9))<500){
      missedOptimalIter=missedOptimalIter+1
      next
    }
    
    ## all indices of allpaths_num with SMA >=0.6
    ## Split the indices into sets of consecutive indices
    ## Start index = When rewards/30 trials reaches 0.5
    start_index=0
    end_index=0
    l<-which(SMA(generated_data[,3],10)>=0.5)
    k<-split(l, cumsum(c(1, diff(l) != 1)))
    for(set in 1:length(k)){
      if(length(k[[set]])>10){
        start_index=k[[set]][1]
        break
      }
    }

    r<-rle(SMA(generated_data[,3],30))
    rIndx<-which(diff(cumsum(r$lengths))>50)[1]
    end_index=cumsum(r$lengths)[(rIndx)]

    if(end_index <= start_index){
      next
    }
    
    ### Estimate Model1 parameters for simulated data
    half_index = start_index
    
    out_model1 <-DEoptim(aca_negLogLik1, lower = c(0,0), upper = c(1,1), allpaths = generated_data[1:half_index,], model = 1, sim=1,DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200)) 
    alpha_model1_est = out_model1$optim$bestmem[1]
    episode_model1_est = floor(2+(out_model1$optim$bestmem[2]*28))
    ### Compute MSE for Model1 using simulated data
    aca_model1_probMatrix <-acaGetProbMatrix(generated_data, alpha_model1_est,episode_model1_est,matrix(0,nrow=2,ncol=6),model=1,sim=1)
    # mse_model1_mat_method1 <- computeMSE(generated_data,aca_model1_probMatrix,sim=1)
    # mse_model1_method1 <- sum(mse_model1_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
    mse_model1_mat_method2 <- computeMSE2(generated_data,aca_model1_probMatrix,sim=1)
    mse_model1_method2 <- sum(mse_model1_mat_method2[(half_index+1):end_index])/(end_index-half_index)
    
    #mse_model1_mat <-aca_mle_lik(generated_data,alpha_model1_est,episode_model1_est,matrix(0,2,6),model=1,sim=1)
    #mse_model1 <- (-1)*sum(mse_model1_mat[(half_index+1):end_index])

    # ## Estimate Model2 parameters for simulated data
    out_model2 <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1),allpaths=generated_data[1:half_index,],model = 2, sim=1, DEoptim.control(NP=20,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
    alpha_model2_est = out_model2$optim$bestmem[1]
    episode_model2_est = floor(2+(out_model2$optim$bestmem[2]*28))
    ## Compute MSE for Model2 using simulated data
    aca_model2_probMatrix <-acaGetProbMatrix(generated_data, alpha_model2_est,episode_model2_est,matrix(0,nrow=2,ncol=6),model=2,sim=1)
    # mse_model2_mat_method1 <- computeMSE(generated_data,aca_model2_probMatrix,sim=1)
    # mse_model2_method1 <- sum(mse_model2_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
    mse_model2_mat_method2 <- computeMSE2(generated_data,aca_model2_probMatrix,sim=1)
    mse_model2_method2 <- sum(mse_model2_mat_method2[(half_index+1):end_index])/(end_index-half_index)
        #mse_model2_mat <-aca_mle_lik(generated_data,alpha_model2_est,episode_model2_est,matrix(0,2,6),model=2,sim=1)
    #mse_model2 <- (-1)*sum(mse_model2_mat[(half_index+1):end_index])
    
    
    
    # ## Estimate SARSA parameters for simulated data
    out_sarsa <- DEoptim(sarsa_negLogLik, lower = c(0,0,0), upper = c(1,1,1), allpaths = generated_data[1:half_index,], sim=1, DEoptim.control(NP = 30,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
    alpha_sarsa_est = out_sarsa$optim$bestmem[1]
    gamma_sarsa_est = out_sarsa$optim$bestmem[2]
    lambda_sarsa_est = out_sarsa$optim$bestmem[3]
    ## Compute MSE for Model2 using simulated data
    sarsa_probMatrix <-sarsaGetProbMatrix(generated_data, alpha_sarsa_est,gamma_sarsa_est,lambda_sarsa_est, matrix(0,nrow=2,ncol=6),sim=1)
    # mse_sarsa_mat_method1 <-computeMSE(generated_data,sarsa_probMatrix,sim=1)
    # mse_sarsa_method1 <- sum(mse_sarsa_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
    mse_sarsa_mat_method2 <-computeMSE2(generated_data,sarsa_probMatrix,sim=1)
    mse_sarsa_method2 <- sum(mse_sarsa_mat_method2[(half_index+1):end_index])/(end_index-half_index)
    
    #mse_sarsa_mat <-sarsa_mle(generated_data,alpha_sarsa_est,gamma_sarsa_est,lambda_sarsa_est,matrix(0,2,6),sim=1)
    #mse_sarsa <- (-1)*sum(mse_sarsa_mat[(half_index+1):end_index])
    
    mat_res[iter,1]=mse_model1_method2
    mat_res[iter,2]=toString(out_model1$optim$bestmem)
    mat_res[iter,3]=mse_model2_method2
    mat_res[iter,4]=toString(out_model2$optim$bestmem)
    mat_res[iter,5]=mse_sarsa_method2
    mat_res[iter,6]=toString(out_sarsa$optim$bestmem)
    
    if(mat_res[iter,1]==0 ||mat_res[iter,3]==0||mat_res[iter,5]==0 ){
      print(sprintf("Stopping as MSE =0"))
      break
    }
    
    index_min=which.min(c(mse_model1_method1,100000,mse_sarsa_method1))
    if(index_min==1){
      mat_res[iter,7]="ACA model1"
      #break
    }else if(index_min==2){
      mat_res[iter,7]="ACA model2"
    }
    else if(index_min==3){
      mat_res[iter,7]="SARSA"
      print(sprintf("SARSA likelihood lower, stopping"))
      break
    }
    
    iter=iter+1
    
  }
  print(sprintf("Nb of iterations where optimal behaviour was not learned=%i",missedOptimalIter))
  
  print(mat_res)
  return(mat_res)
  
}

getExponent=function(x){
  return(ifelse(x == 0, 0, floor(log10(abs(x)))+1 ))
}


aca_negLogLik1 <- function(par,allpaths,model,sim) {
  
  alpha <- par[1]
  epsLim <- 2+(par[2]*28)
  #epsLim <- 1
  #print(sprintf("epsLim=%f",epsLim))
  H <- matrix(0,2,6)
  # H[1,1:6]<-par[3:8]
  # H[2,1:6] <- par[9:14]
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- aca_mle_lik(allpaths,alpha,epsLim,H,model,sim)
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



