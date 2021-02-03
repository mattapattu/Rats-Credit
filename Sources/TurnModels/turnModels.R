library(Rmpfr)
library(DEoptim)
library(TTR)
#library(GA)



acaTurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  ACA = DEoptim(negLogLikFunc, lower = c(0,0), upper = c(1,0), allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model=1, sim=sim, DEoptim.control(NP=20, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1, reltol = 0.0005, steptol = 10))
  alpha_ACA = ACA$optim$bestmem[1]
  reward_ACA = ACA$optim$bestmem[2]
  reward_ACA = 1+reward_ACA*9
  
  # ACA = ga("real-valued", fitness =  negLogLikFunc,lower = c(0), upper = c(1),allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model = 1, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE) 
  # alpha_ACA = ACA@solution[1]
  # reward_ACA = 1
  
  params_lik = list("alpha"=alpha_ACA, "reward"=reward_ACA)
  

  ACA_probMatrix = TurnsModels::getProbMatrix(generated_data,turnTimes,turnMethod = turnMethod,alpha=alpha_ACA,reward_ACA, sim,model=1)
  computationalActivity = vector()
  lik = TurnsModels::getTurnsLikelihood(generated_data, turnTimes, turnMethod, alpha_ACA, reward_ACA, sim, model=1)
  
  if(sim==1)
  {
    half_index = last(which(turnTimes[,6]<=half_index))
  }
  else
  {
    half_index = last(which(turnTimes[,1]<=half_index))
  }
  if(end_index==0)
  {
    end_index=length(lik)
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

gbTurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  GB = DEoptim(negLogLikFunc, lower = c(0,0), upper = c(1,0), allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model=2, sim=sim, DEoptim.control(NP=20, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1, reltol = 0.0005, steptol = 10))
  alpha_GB = GB$optim$bestmem[1]
  reward_GB = GB$optim$bestmem[2]
  reward_GB = 1+reward_GB*9
  # GB = ga("real-valued", fitness =  negLogLikFunc,lower = c(0), upper = c(1),allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model = 2, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE)
  # alpha_GB = GB@solution[1]
  # reward_GB = 1
  params_lik = list("alpha"= alpha_GB, "reward"= reward_GB)
  
  
  GB_probMatrix = TurnsModels::getProbMatrix(generated_data,turnTimes,turnMethod,alpha=alpha_GB,reward_GB,sim,model=2)
  computationalActivity = vector()
  lik = TurnsModels::getTurnsLikelihood(generated_data, turnTimes = turnTimes, turnMethod = turnMethod, alpha_GB, reward_GB, sim, model=2)
  
  if(sim==1)
  {
    half_index = last(which(turnTimes[,6]<=half_index))
  }
  else
  {
    half_index = last(which(turnTimes[,1]<=half_index))
  }
  
  if(end_index==0)
  {
    end_index=length(lik)
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

aca2TurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  ACA2 <- DEoptim(negLogLikFunc,lower = c(0,0), upper = c(1,1), allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model = 4, sim = sim, DEoptim.control(NP=20, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1, reltol = 0.0005, steptol = 10))
  alpha_ACA2 = ACA2$optim$bestmem[1]
  gamma1_ACA2 = ACA2$optim$bestmem[2]
  # ACA2 = ga("real-valued", fitness =  negLogLikFunc,lower = c(0,0), upper = c(1,1),allpaths = generated_data, half_index=half_index,  turnTimes = turnTimes, turnMethod = turnMethod, model = 4, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE)
  # alpha_ACA2 = ACA2@solution[1]
  # gamma1_ACA2 = ACA2@solution[2]

  ACA2_probMatrix = Aca2Turns::getProbMatrix(generated_data, turnTimes, turnMethod, alpha_ACA2,gamma1_ACA2,1,sim)
  params_lik = list("alpha"=alpha_ACA2, "gamma1"=gamma1_ACA2)
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
  lik = Aca2Turns::getTurnsLikelihood(generated_data, turnTimes, turnMethod, alpha_ACA2, gamma1_ACA2, 1, sim)
  
  if(sim==1)
  {
    half_index = last(which(turnTimes[,6]<=half_index))
  }
  else
  {
    half_index = last(which(turnTimes[,1]<=half_index))
  }
  if(end_index==0)
  {
    end_index=length(lik)
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

aca3TurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  ACA3 <- DEoptim(negLogLikFunc,lower = c(0,0,0,0), upper = c(1,1,1,0), allpaths = generated_data, half_index=half_index, turnTimes = turnTimes, turnMethod = turnMethod, model = 5, sim = sim, DEoptim.control(NP=40, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1, reltol = 0.0005, steptol = 10))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma1_ACA3 = ACA3$optim$bestmem[2]
  gamma2_ACA3 = ACA3$optim$bestmem[3]
  reward_ACA3 = ACA3$optim$bestmem[4]
  reward_ACA3 = 1 + reward_ACA3*9
  #suggestedSol = matrix(c(0.8,0.9,0.9, 0.9, 0.9, 0.7 ), nrow = 2, ncol = 3, byrow = TRUE)
  # ACA3 = ga("real-valued", fitness =  negLogLikFunc,lower = c(0,0,0), upper = c(1,1,1),allpaths = generated_data, half_index=half_index,    turnTimes = turnTimes, turnMethod = turnMethod, model = 5, sim = sim, popSize=50, maxiter = 100, run = 10,monitor=FALSE)
  # alpha_ACA3 = ACA3@solution[1]
  # gamma1_ACA3 = ACA3@solution[2]
  # gamma2_ACA3 = ACA3@solution[3]
  # reward_ACA3 = 1
  ACA3_probMatrix = Aca3Turns::getProbMatrix(generated_data, turnTimes, turnMethod, alpha_ACA3,gamma1_ACA3,gamma2_ACA3,reward_ACA3,sim)
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
  
  if(sim==1)
  {
    half_index = last(which(turnTimes[,6]<=half_index))
  }
  else
  {
    half_index = last(which(turnTimes[,1]<=half_index))
  }
  lik = Aca3Turns::getTurnsLikelihood(generated_data, turnTimes, turnMethod, alpha_ACA3, gamma1_ACA3, gamma2_ACA3, reward_ACA3, sim)
  if(end_index==0)
  {
    end_index=length(lik)
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

sarsaTurnData=function(generated_data,turnTimes, sim, half_index, end_index, window){
  SARSA <- DEoptim(negLogLikFunc,lower = c(0,0,0,0), upper = c(1,1,1,1), allpaths = generated_data,half_index=half_index, turnTimes = 0, turnMethod = 0, model = 6, sim = sim, DEoptim.control(NP=40, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1, reltol = 0.0005, steptol = 10))
  alpha_SARSA = SARSA$optim$bestmem[1]
  gamma_SARSA = SARSA$optim$bestmem[2]
  lambda_SARSA = SARSA$optim$bestmem[3]
  reward_SARSA = SARSA$optim$bestmem[4]
  reward_SARSA = 1 + reward_SARSA*9
  # SARSA = ga("real-valued", fitness =  negLogLikFunc,lower = c(0,0,0,0), upper = c(1,1,1,1),allpaths = generated_data, half_index=half_index, turnTimes = 0, turnMethod = 0, model = 6, sim = sim,  popSize=50, maxiter = 100, run = 10,monitor=FALSE, optim = TRUE)
  # alpha_SARSA = SARSA@solution[1]
  # gamma_SARSA = SARSA@solution[2]
  # lambda_SARSA = SARSA@solution[3]
  # reward_SARSA = SARSA@solution[4]
  # reward_SARSA = 1 + reward_SARSA*9
  SARSA_probMatrix = SarsaTurns::getProbMatrix(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,reward_SARSA, sim)
  params_lik = list("alpha"=alpha_SARSA, "gamma"=gamma_SARSA, "lambda" =lambda_SARSA, "reward"= reward_SARSA)
  
  #paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,SARSA_probMatrix)
  computationalActivity = vector()
  
  if(sim==1)
  {
    half_index = last(which(turnTimes[,6]<=half_index))
  }
  else
  {
    half_index = last(which(turnTimes[,1]<=half_index))
  }
  lik = SarsaTurns::getTurnsLikelihood(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,reward_SARSA,sim)
  if(end_index==0)
  {
    end_index=length(lik)
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

negLogLikFunc=function(par,allpaths,turnTimes,turnMethod,half_index,model,sim) {
  
  alpha = par[1]

  if(model == 1 || model == 2 || model == 3){
    reward = 1
    probMatrix = TurnsModels::getProbMatrix(allpaths,turnTimes,turnMethod,alpha,reward,sim,model)
    pathProbMatrix = TurnsModels::getPathProbMatrix(probMatrix,allpaths,sim)
    path4Probs = pathProbMatrix[which(pathProbMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = pathProbMatrix[which(pathProbMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      turnlik=TurnsModels::getTurnsLikelihood(allpaths[1:half_index,],turnTimes,turnMethod,alpha,reward,sim,model)
    }
    else
    {
      turnlik = -1000000
    }
    
  }
  else if(model == 4){
    gamma1 = par[2]
    
    probMatrix = Aca2Turns::getProbMatrix(allpaths,turnTimes,turnMethod,alpha,gamma1,1,sim)
    pathProbMatrix = TurnsModels::getPathProbMatrix(probMatrix,allpaths,sim)
    path4Probs = pathProbMatrix[which(pathProbMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = pathProbMatrix[which(pathProbMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      turnlik=Aca2Turns::getTurnsLikelihood(allpaths[1:half_index,],turnTimes,turnMethod,alpha,gamma1,1,sim)
    }
    else
    {
      turnlik = -1000000
    }
    
    
  }
  else if(model == 5){
    gamma1 = par[2]
    gamma2 = par[3]
    # reward = par[4]
    # reward = 1+reward*9
    reward = 1
    # 
    probMatrix = Aca3Turns::getProbMatrix(allpaths,turnTimes,turnMethod,alpha,gamma1,gamma2,1,sim)
    pathProbMatrix = TurnsModels::getPathProbMatrix(probMatrix,allpaths,sim)
    path4Probs = pathProbMatrix[which(pathProbMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.95)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = pathProbMatrix[which(pathProbMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.95)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      turnlik=Aca3Turns::getTurnsLikelihood(allpaths[1:half_index,],turnTimes,turnMethod,alpha,gamma1,gamma2,1,sim)
    }
    else
    {
      turnlik = -1000000
    }
    
    
  }
  else if(model == 6){
    gamma = par[2]
    lambda = par[3]
    reward = par[4]
    reward = 1+reward*9
    
    probMatrix = SarsaTurns::getProbMatrix(allpaths,alpha,gamma,lambda,reward,sim)
    pathProbMatrix = TurnsModels::getPathProbMatrix(probMatrix,allpaths,sim)
    path4Probs = pathProbMatrix[which(pathProbMatrix[,4]>0),4]
    path4AboveLim = which(path4Probs >= 0.80)
    result <- rle(diff(path4AboveLim))
    path4Converged = any(result$lengths>=30 & result$values==1)
    
    path10Probs = pathProbMatrix[which(pathProbMatrix[,10]>0),10]
    path10AboveLim = which(path10Probs >= 0.80)
    result <- rle(diff(path10AboveLim))
    path10Converged = any(result$lengths>=30 & result$values==1)
    
    if(path4Converged &&  path10Converged)
    {
      turnlik=SarsaTurns::getTurnsLikelihood(allpaths[1:half_index,],alpha,gamma,lambda,reward,sim)
    }
    else
    {
      turnlik = -1000000
    }
    
  }
  
  negLogLik = (-1) *sum(turnlik)
 # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else if(is.nan(negLogLik)){
    print(sprintf("Alpha = %f",alpha ))
    return(1000000)
  }
  else{
    return(negLogLik)
  }
  
}


aca_negLogLik2=function(par,Hinit, allpaths,model,sim, epsilon) {
  
  alpha = par[1]
  epsilon = (round(par[2]*1000)+0.001)
  endTrial = (round(par[3]*1000)+1)
  if(model == 1 || model == 2 || model == 3){
    lik = baseModels::getPathLikelihood(allpaths, alpha, Hinit, sim, model, policyMethod=2, epsilon,endTrial)
  }else if(model == 4){
    lik = Aca2::getPathLikelihood(allpaths, alpha, Hinit, sim, model, policyMethod=2, epsilon)
  }else if(model == 5){
    gamma = par[2]
    lambda = par[3]
    lik = Aca3::getPathLikelihood(allpaths, alpha, gamma, lambda, Hinit, sim, model, policyMethod=2, epsilon)
  }
  
  negLogLik = (-1) *sum(lik)
 # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}
  
  
  



