library(Rmpfr)
library(DEoptim)
library(TTR)




acaData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA = DEoptim(aca_negLogLik1, lower = 0, upper = 1, Hinit = Hinit2, allpaths = generated_data,half_index=half_index, model=1, sim=sim, DEoptim.control(NP=10, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA = ACA$optim$bestmem[1]
  # ACA = ga("real-valued", fitness =  aca_negLogLik1,lower = c(0), upper = c(1),Hinit = Hinit2, allpaths = generated_data, half_index=half_index, model = 1, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE) 
  # alpha_ACA = ACA@solution[1]
  
  params_lik = list("alpha"=alpha_ACA)
  
  # ACA = DEoptim(aca_negLogLik2, lower = c(0,0,0), upper = c(1,1,1), H=Hinit2, allpaths = generated_data[1:half_index,], model=1, sim=sim, DEoptim.control(NP = 30,F = 0.8, CR = 0.9, trace = FALSE, itermax = 100))  
  # alpha_ACA = ACA$optim$bestmem[1]
  # epsilon_ACA = ACA$optim$bestmem[2]
  # endTrial_ACA = ACA$optim$bestmem[3]
  # params_lik = list("alpha"=alpha_ACA, "epsilon"=epsilon_ACA, "endTrial"=endTrial_ACA)
  ACA_probMatrix = baseModels::getProbMatrix(generated_data, alpha_ACA, H=Hinit2, sim, model=1, policyMethod=1)
  
  #acaActions = getActionData(generated_data, ACA_probMatrix, half_index, end_index, window, sim)
  paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,ACA_probMatrix)
  computationalActivity = vector()
  lik = baseModels::getPathLikelihood(generated_data, alpha_ACA, Hinit2, sim, model=1, policyMethod=1)
  if(end_index==0)
  {
    end_index = length(lik)
  }
 
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  ACA <- new("Model", Name = "ACA", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = ACA_probMatrix)
  
  return(ACA)
}

gbData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  
  GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, Hinit =  Hinit2, allpaths = generated_data, half_index=half_index, model = 2, sim=sim, DEoptim.control(NP=10, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_GB = GB$optim$bestmem[1]
  # GB = ga("real-valued", fitness =  aca_negLogLik1,lower = c(0), upper = c(1),Hinit =  Hinit2, allpaths = generated_data, half_index=half_index, model = 2, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE)
  # alpha_GB = GB@solution[1]
  params_lik = list("alpha"=alpha_GB)
  
  # GB <- DEoptim(aca_negLogLik2, lower = c(0,0,0), upper = c(1,1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 2, sim=sim, DEoptim.control(NP=30,F=0.8, CR = 0.9,trace = FALSE, itermax = 50))
  # alpha_GB = GB$optim$bestmem[1]
  # epsilon_GB = GB$optim$bestmem[2]
  # endTrial_GB = ACA$optim$bestmem[3]
  # params_activity = list("alpha"=alpha_GB)
  GB_probMatrix = baseModels::getProbMatrix(generated_data, alpha_GB, H=Hinit2, sim, model=2, policyMethod=1)
  #params_lik = list("alpha"=alpha_GB)
  #gbActions = getActionData(generated_data, GB_probMatrix, half_index, end_index, window, sim)
  paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,GB_probMatrix)
  computationalActivity = vector()
  
  lik = baseModels::getPathLikelihood(generated_data, alpha_GB, Hinit2, sim, model=2, policyMethod=1)
  if(end_index==0)
  {
    end_index = length(lik)
  }
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  GB <- new("Model", Name = "GB", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = GB_probMatrix)
  
  return(GB)
}

gbAcaData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, Hinit = Hinit2, allpaths = generated_data[1:half_index,], model = 3, sim = sim, DEoptim.control(NP=100, F=2, CR = 0.9,trace = FALSE, itermax = 200, strategy = 1))
  alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_ACA_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_ACA_GB,H=Hinit2, sim, model=3)
  gbacaActions = getActionData(generated_data, GB_ACA_probMatrix, half_index, end_index, window, sim)
  mse1 = getMse1(gbacaActions,half_index, end_index)
  mse2 = getMse2(gbacaActions,half_index, end_index)
  
  lik = baseModels::getPathLikelihood(generated_data, alpha_ACA_GB, Hinit2, sim, model=3)
  
  GBACA <- new("Model", Name = "GB-ACA", Actions = gbacaActions, Metrics = list("mse1" = mse1,"mse2" = mse2,"likelihood" = lik), ProbMatrix = GB_ACA_probMatrix)
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  return(GBACA)
}


aca2Data = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA2 <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data,half_index=half_index, model = 4, sim = sim, DEoptim.control(NP=20, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA2 = ACA2$optim$bestmem[1]
  gamma1_ACA2 = ACA2$optim$bestmem[2]
  # ACA2 = ga("real-valued", fitness =  aca_negLogLik1,lower = c(0,0), upper = c(1,1),Hinit = Hinit2,allpaths = generated_data, half_index=half_index, model = 4, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE)
  # alpha_ACA2 = ACA2@solution[1]
  # gamma1_ACA2 = ACA2@solution[2]
  params_lik = list("alpha"=alpha_ACA2,"gamma1"=gamma1_ACA2)
  
  # ACA2 <- DEoptim(aca_negLogLik2, lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 4, sim = sim, DEoptim.control(NP=20,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  # alpha_ACA2 = ACA2$optim$bestmem[1]
  # epsilon_ACA2 = ACA2$optim$bestmem[2]
  # params_lik = list("alpha"=alpha_ACA2, "epsilon"=epsilon_ACA2)
  ACA2_probMatrix = Aca2::getProbMatrix(generated_data, alpha_ACA2, gamma1_ACA2, Hinit2, sim, model=4, policyMethod=1)
  
  #aca2Actions = getActionData(generated_data, ACA2_probMatrix, half_index, end_index, window, sim)
  paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,ACA2_probMatrix)
  computationalActivity = vector()
  lik = Aca2::getPathLikelihood(generated_data, alpha_ACA2, gamma1_ACA2, Hinit2, sim, model=4, policyMethod=1)
  if(end_index==0)
  {
    end_index = length(lik)
  }
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  ACA2 <- new("Model", Name = "ACA2", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = ACA2_probMatrix)
  
  return(ACA2)
}

aca3Data = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA3 <- DEoptim(aca_negLogLik1,lower = c(0,0,0), upper = c(1,1,1), Hinit = Hinit2, allpaths = generated_data,half_index=half_index, model = 5, sim = sim, DEoptim.control(NP=30, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma1_ACA3 = ACA3$optim$bestmem[2]
  gamma2_ACA3 = ACA3$optim$bestmem[3]
  # ACA3 = ga("real-valued", fitness =  aca_negLogLik1,lower = c(0,0,0), upper = c(1,1,1),allpaths = generated_data, half_index=half_index,Hinit = Hinit2, model = 5, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE, optim = TRUE)
  # alpha_ACA3 = ACA3@solution[1]
  # gamma1_ACA3 = ACA3@solution[2]
  # gamma2_ACA3 = ACA3@solution[3]

  ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma1_ACA3,gamma2_ACA3, H=Hinit2, sim, model=5, policyMethod = 1)
  params_lik = list("alpha"=alpha_ACA3, "gamma1"=gamma1_ACA3, "gamma2"=gamma2_ACA3)
    # ACA3 <- DEoptim(aca_negLogLik2,lower = c(0,0,0), upper = c(1,1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 5, sim = sim, DEoptim.control(NP=30, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  # alpha_ACA3 = ACA3$optim$bestmem[1]
  # gamma_ACA3 = ACA3$optim$bestmem[2]
  # epsilon_ACA3 = ACA3$optim$bestmem[3]
  # params_activity = list("alpha"=alpha_ACA3, "gamma"=gamma_ACA3, "epsilon"=epsilon_ACA3)
  #ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma_ACA3, H=Hinit2, sim, model=5, policyMethod=1)

  #aca3Actions = getActionData(generated_data, ACA3_probMatrix, half_index, end_index, window, sim)
  paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,ACA3_probMatrix)
  computationalActivity = vector()
  lik = Aca3::getPathLikelihood(generated_data, alpha_ACA3, gamma1_ACA3, gamma2_ACA3, Hinit2, sim, model=5, policyMethod=1)
  if(end_index==0)
  {
    end_index = length(lik)
  }
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  ACA3 <- new("Model", Name = "ACA3", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = ACA3_probMatrix)
  
  return(ACA3)
}

sarsaData=function(Qinit, generated_data, sim, half_index, end_index, window){
  SARSA <- DEoptim(aca_negLogLik1,lower = c(0,0,0,0), upper = c(1,1,1,1), Hinit = matrix(0,2,6), half_index=half_index, allpaths = generated_data, model = 6, sim = sim, DEoptim.control(NP=40, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_SARSA = SARSA$optim$bestmem[1]
  gamma_SARSA = SARSA$optim$bestmem[2]
  lambda_SARSA = SARSA$optim$bestmem[3]
  reward_SARSA = SARSA$optim$bestmem[4]
  reward_SARSA = 1 + reward_SARSA*9
  # SARSA = ga("real-valued", fitness =  aca_negLogLik1,lower = c(0,0,0,0), upper = c(1,1,1,1),Hinit = matrix(0,2,6), allpaths = generated_data, half_index=half_index, model = 6, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE, optim = TRUE)
  # alpha_SARSA = SARSA@solution[1]
  # gamma_SARSA = SARSA@solution[2]
  # lambda_SARSA = SARSA@solution[3]
  # reward_SARSA = SARSA@solution[4]
  # reward_SARSA = 1 + reward_SARSA*9
  
  SARSA_probMatrix = Sarsa::getProbMatrix(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,reward_SARSA, Q=Qinit, sim, policyMethod = 1)
  params_lik = list("alpha"=alpha_SARSA, "gamma"=gamma_SARSA, "lambda"=lambda_SARSA, "reward"=reward_SARSA)
  
  #paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,SARSA_probMatrix)
  computationalActivity = vector()
  lik = Sarsa::getPathLikelihood(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,reward_SARSA,  Qinit, sim, policyMethod=1)
  if(end_index==0)
  {
    end_index = length(lik)
  }
  if(any(is.infinite(lik)))
  {
    lik=100000
  }
  else
  {
    lik = -1*sum(lik[(half_index+1):end_index])
  }
  SARSA <- new("Model", Name = "SARSA", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = SARSA_probMatrix)
  return(SARSA)
}

aca_negLogLik1=function(par,Hinit, allpaths,model,half_index, sim) {
  
  alpha = par[1]
  
  if(model == 1 || model == 2 || model == 3){
    
    probMatrix = baseModels::getProbMatrix(allpaths, alpha, Hinit, sim, model, policyMethod=1)
    path4Probs = probMatrix[which(probMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = probMatrix[which(probMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      lik = baseModels::getPathLikelihood(allpaths[1:half_index,], alpha, Hinit, sim, model, policyMethod=1, 0, 0)
    }
    else
    {
      lik = -1000000
    }
    
    
  }
 else if(model == 4){
    Hinit = matrix(0,2,6)
    gamma1 = par[2]
    
    probMatrix = Aca2::getProbMatrix(allpaths, alpha, gamma1, Hinit, sim, model, policyMethod=1)
    path4Probs = probMatrix[which(probMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = probMatrix[which(probMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      lik = Aca2::getPathLikelihood(allpaths[1:half_index,], alpha,gamma1,Hinit, sim, model, policyMethod=1)
    }
    else
    {
      lik = -1000000
    }
    
    
  }
  else if(model == 5){
    Hinit = matrix(0,2,6)
    gamma1 = par[2]
    gamma2 = par[3]
    
    probMatrix = Aca3::getProbMatrix(allpaths, alpha, gamma1, gamma2, Hinit, sim, model, policyMethod=1)
    path4Probs = probMatrix[which(probMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = probMatrix[which(probMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      lik = Aca3::getPathLikelihood(allpaths[1:half_index,], alpha,gamma1,gamma2, Hinit, sim, model, policyMethod=1)
    }
    else
    {
      lik = -1000000
    }
    
    
    
  }
  else if(model == 6){
    gamma = par[2]
    lambda = par[3]
    reward = par[4]
    reward = 1+reward*9
    
    
    probMatrix = Sarsa::getProbMatrix(allpaths, alpha, gamma, lambda,reward, Hinit, sim, model, policyMethod=1)
    path4Probs = probMatrix[which(probMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = probMatrix[which(probMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      lik = Sarsa::getPathLikelihood(allpaths[1:half_index,], alpha, gamma, lambda, reward, Hinit, sim, policyMethod=1)
    }
    else
    {
      lik = -1000000
    }
    
    
  }
  
  negLogLik = (1) *sum(lik)
 # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else if(is.nan(negLogLik)){
    print(sprintf("Alpha = %f, Gamma = %f, Lambda = %f",alpha, gamma, lambda ))
    return(1000000)
  }
  else{
    return(negLogLik)
  }
  
}


  
  
  



