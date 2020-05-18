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
  ### Estimate paramteres using Model1 or Model2
  #0DEoptim(sarsa_negLogLik, lower = c(0,0,0), upper = c(1,1,1), allpaths = l, sim=1, DEoptim.control(NP = 140,F = 0.8, CR = 0.9, trace = TRUE)) 
  # out <- DEoptim(sarsa_negLogLik, lower = c(0,0,0), upper = c(1,1,1), allpaths = allpaths_num, sim=2,DEoptim.control(NP = 30,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  # alpha = out$optim$bestmem[1]
  # gamma = out$optim$bestmem[2]
  # lambda = out$optim$bestmem[3]
  out <- DEoptim(aca_negLogLik1, lower = c(0,0), upper = c(1,1), H=matrix(0,2,6),allpaths = allpaths_num[1:664,], learningend=664,  model = 1, sim=2,DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  alpha = out$optim$bestmem[[1]]
  temperature = out$optim$bestmem[[2]]
  #episode_len = out$optim$bestmem[2]

  #probACA=aca_model1(H,alpha,allpaths)
  #print(probACA)
  mat_res <- matrix(0,nrow=100,ncol=7)
  colnames(mat_res) <- c("ACA MSE","ACA bestval","GB MSE","GB bestval","GB-ACA MSE","GB-ACA bestval","Selected Model")
  iter=1
  start_index=0
  end_index=0
  missedOptimalIter=0
  while(iter<=100){

    total_trials = length(allpaths_num[,1])
    init_state=as.numeric(allpaths_num[1,2])
    ### Generate simulated data using the estimated params
    H<-matrix(0,2,6)
    H[1,1]=3
    H[2,1]=3 
    generated_data <- aca_gen_sim(allpaths_num,H,alpha,total_trials,init_state,model=2,temperature*1000)
    #generated_data <- sarsa_gen_sim(allpaths_num,alpha_mod,gamma_mod,lambda_mod,total_trials,init_state)
    

    if(length(which(SMA(generated_data[,3],10)>=0.9))<500){
      missedOptimalIter=missedOptimalIter+1
      next
    }
    
    generated_data<-allpaths_num
    ## all indices of allpaths_num with SMA >=0.6
    ## Split the indices into sets of consecutive indices
    ## Start index = When rewards/30 trials reaches 0.5
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
   
    
    ACA <-DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H=matrix(0,2,6),allpaths = generated_data[1:half_index,],learningend=end_index, model = 1, sim=1,DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200)) 
    alpha_ACA = ACA$optim$bestmem[1]
    temperature_ACA = ACA$optim$bestmem[2]
    ### Compute MSE for Model1 using simulated data
    H<-matrix(0,2,6)
    H[1,1]=3*temperature_ACA*1000
    H[2,1]=3*temperature_ACA*1000
    ACA_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA,H,model=1,sim=1,temperature_ACA*1000,1,end_index)
    # mse_model1_mat_method1 <- computeMSE(generated_data,ACA_probMatrix,sim=1)
    # mse_model1_method1 <- sum(mse_model1_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
    mse_aca_mat_method2 <- computeMSE2(generated_data,ACA_probMatrix,sim=1)
    mse_model1_method2 <- sum(mse_aca_mat_method2[(half_index+1):end_index])/(end_index-half_index)
    # H<-matrix(0,2,6)
    # H[1,1]=3
    # H[2,1]=3
    # ACA_lik_mat <-aca_mle_lik(generated_data,alpha_ACA,H,model=1,sim=1)
    # ACA_lik <- (-1)*sum(ACA_lik_mat[(half_index+1):end_index])

    # ## Estimate Model2 parameters for simulated data
    GB <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H=matrix(0,2,6),allpaths=generated_data[1:half_index,], learningend=end_index, model=2, sim=1, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
    alpha_GB = GB$optim$bestmem[1]
    temperature_GB = GB$optim$bestmem[2]
    #episode_model2_est = floor(2+(out_model2$optim$bestmem[2]*28))
    ## Compute MSE for Model2 using simulated data
    H<-matrix(0,2,6)
    H[1,1]=3*temperature_GB*1000
    H[2,1]=3*temperature_GB*1000
    GB_probMatrix <-acaGetProbMatrix(generated_data, alpha_GB,H,model=2,sim=1,temperature_GB*1000,1,end_index)
    mse_model2_mat_method2 <- computeMSE2(generated_data,GB_probMatrix,sim=1)
    mse_model2_method2 <- sum(mse_model2_mat_method2[(half_index+1):end_index])/(end_index-half_index)
    
    # H<-matrix(0,2,6)
    # H[1,1]=3
    # H[2,1]=3
    # GB_lik_mat <-aca_mle_lik(generated_data,alpha_GB,H,model=2,sim=1)
    # GB_lik <- (-1)*sum(GB_lik_mat[(half_index+1):end_index])
    
    # ## Estimate Model2 parameters for simulated data
    ACA_GB <- DEoptim(aca_negLogLik1,lower = c(0,0,0,0), upper = c(1,100,half_index,100),H=matrix(0,2,6),allpaths=generated_data[1:half_index,], model = 3, sim=2, DEoptim.control(NP=40,F=0.8, CR = 0.9,trace = FALSE, reltol=.00000001,steptol=150, itermax = 5000))
    alpha_ACA_GB = ACA_GB$optim$bestmem[1]
    temperature1_ACA_GB = ACA_GB$optim$bestmem[2]
    endExplore = ACA_GB$optim$bestmem[3]
    temperature2_ACA_GB = ACA_GB$optim$bestmem[4]
    print(ACA_GB$optim$bestmem)
    #episode_model2_est = floor(2+(out_model2$optim$bestmem[2]*28))
    ## Compute MSE for Model2 using simulated data
    H<-matrix(0,2,6)
    H[1,1]=3*temperature1_ACA_GB
    H[2,1]=3*temperature1_ACA_GB
    aca_model3_probMatrix <-acaGetProbMatrix(generated_data, alpha_ACA_GB,H,model=3,sim=2,temperature1_ACA_GB,temperature2_ACA_GB,endExplore)
    # mse_model2_mat_method1 <- computeMSE(generated_data,aca_model2_probMatrix,sim=1)
    # mse_model2_method1 <- sum(mse_model2_mat_method1[,(half_index+1):end_index])/(end_index-half_index)
    mse_model3_mat_method2 <- computeMSE2(generated_data,aca_model3_probMatrix,sim=2)
    mse_model3_method2 <- sum(mse_model3_mat_method2[(half_index+1):end_index])/(end_index-half_index)
    
    # H<-matrix(0,2,6)
    # H[1,1]=3
    # H[2,1]=3
    # ACA_GB_lik_mat <-aca_mle_lik(generated_data,alpha_ACA_GB,H,model=3,sim=1)
    # ACA_GB_lik <- (-1)*sum(ACA_GB_lik_mat[(half_index+1):end_index])
    
  
    mat_res[iter,1]=mse_model1_method2
    mat_res[iter,2]=toString(ACA$optim$bestmem)
    mat_res[iter,3]=mse_model2_method2
    mat_res[iter,4]=toString(GB$optim$bestmem)
    mat_res[iter,5]=mse_model3_method2
    mat_res[iter,6]=toString(ACA_GB$optim$bestmem)
    
    if(mat_res[iter,1]==0 ||mat_res[iter,3]==0||mat_res[iter,5]==0 ){
      print(sprintf("Stopping as MSE =0"))
      break
    }
    
    index_min=which.min(c(mat_res[iter,1],mat_res[iter,3],mat_res[iter,5]))
    if(index_min==1){
      mat_res[iter,7]="ACA"
      #break
    }else if(index_min==2){
      mat_res[iter,7]="GB"
    }
    else if(index_min==3){
      mat_res[iter,7]="ACA_GB"
      # print(sprintf("SARSA likelihood lower, stopping"))
      # break
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


aca_negLogLik1 <- function(par,H,allpaths,model,sim) {
  
  alpha <- par[1]
  temperature1 <- par[2]
  endExplore <- par[3]
  temperature2 <- par[4]
  #temperature2 <- 1
  #epsLim <- 2+(par[2]*28)
  #epsLim <- 1
  #print(sprintf("epsLim=%f",epsLim))
  H<-matrix(0,2,6)
  H[1,1]=3*temperature1
  H[2,1]=3*temperature1
  #lik <- aca_mle_cpp2(allpaths,enreg,alpha,epsLim,H)
  lik <- aca_mle_lik(allpaths,alpha,H,model,sim,temperature1,endExplore,temperature2)
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



