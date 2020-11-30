library(Rmpfr)
library(DEoptim)
library(TTR)



acaTurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  ACA = DEoptim(negLogLikFunc, lower = 0, upper = 1, allpaths = generated_data[1:half_index,], turnTimes = turnTimes, turnMethod = turnMethod, model=1, sim=sim, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  alpha_ACA = ACA$optim$bestmem[1]
  params_lik = list("alpha"=alpha_ACA)
  

  ACA_probMatrix = TurnsModels::getProbMatrix(generated_data,turnTimes,turnMethod = turnMethod,alpha=alpha_ACA,sim,model=1)
  computationalActivity = vector()
  lik = TurnsModels::getTurnsLikelihood(generated_data, turnTimes, turnMethod, alpha_ACA, sim, model=1)
  lik = -1*sum(lik[(half_index+1):end_index])
  ACA <- new("Model", Name = "ACA", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = ACA_probMatrix)
  
  return(ACA)
}

gbTurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  GB = DEoptim(negLogLikFunc, lower = 0, upper = 1, allpaths = generated_data[1:half_index,], turnTimes = turnTimes, turnMethod = turnMethod, model=2, sim=sim, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
  alpha_GB = GB$optim$bestmem[1]
  params_lik = list("alpha"= alpha_GB)
  
  
  GB_probMatrix = TurnsModels::getProbMatrix(generated_data,turnTimes,turnMethod,alpha=alpha_GB,sim,model=2)
  computationalActivity = vector()
  lik = TurnsModels::getTurnsLikelihood(generated_data, turnTimes = turnTimes, turnMethod = turnMethod, alpha_GB, sim, model=2)
  lik = -1*sum(lik[(half_index+1):end_index])
  GB <- new("Model", Name = "GB", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = GB_probMatrix)
  
  return(GB)
}


aca3TurnData = function(generated_data, turnTimes, turnMethod, sim, half_index, end_index, window){
  ACA3 <- DEoptim(negLogLikFunc,lower = c(0,0,0), upper = c(1,1,1), allpaths = generated_data[1:half_index,],  turnTimes = turnTimes, turnMethod = turnMethod, model = 5, sim = sim, DEoptim.control(NP=30, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma1_ACA3 = ACA3$optim$bestmem[2]
  gamma2_ACA3 = ACA3$optim$bestmem[3]
  ACA3_probMatrix = Aca3Turns::getProbMatrix(generated_data, turnTimes, turnMethod, alpha_ACA3,gamma1_ACA3,gamma2_ACA3,sim, model=5)
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
  lik = Aca3Turns::getTurnsLikelihood(generated_data, turnTimes, turnMethod, alpha_ACA3, gamma1_ACA3, gamma2_ACA3, sim, model=5)
  lik = -1*sum(lik[(half_index+1):end_index])
  ACA3 <- new("Model", Name = "ACA3", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = ACA3_probMatrix)
  
  return(ACA3)
}

sarsaTurnData=function(generated_data, sim, half_index, end_index, window){
  SARSA <- DEoptim(aca_negLogLik1,lower = c(0,0,0), upper = c(1,1,1), Hinit = matrix(0,2,6), allpaths = generated_data[1:half_index,], turnTimes = 0, turnMethod = 0, model = 6, sim = sim, DEoptim.control(NP=30, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  alpha_SARSA = SARSA$optim$bestmem[1]
  gamma_SARSA = SARSA$optim$bestmem[2]
  lambda_SARSA = SARSA$optim$bestmem[3]
  SARSA_probMatrix = SarsaTurns::getProbMatrix(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,sim)
  params_lik = list("alpha"=alpha_SARSA, "gamma"=gamma_SARSA)
  
  #paths = baseModels::getEpisodes(generated_data)
  #computationalActivity = baseModels::getComputationalActivity(paths,SARSA_probMatrix)
  computationalActivity = vector()
  lik = SarsaTurns::getTurnsLikelihood(generated_data, alpha_SARSA, gamma_SARSA, lambda_SARSA,sim)
  lik = -1*sum(lik[(half_index+1):end_index])
  SARSA <- new("Model", Name = "SARSA", Params_lik = params_lik, Metrics = list("computationalActivity" = computationalActivity,"likelihood" = lik), ProbMatrix = SARSA_probMatrix)
  return(SARSA)
}

negLogLikFunc=function(par,allpaths,turnTimes,turnMethod,model,sim) {
  
  alpha = par[1]

  if(model == 1 || model == 2 || model == 3){
    turnlik=TurnsModels::getTurnsLikelihood(allpaths,turnTimes,turnMethod,alpha,sim,model)
  }
  else if(model == 5){
    gamma1 = par[2]
    gamma2 = par[3]
    turnlik=Aca3Turns::getTurnsLikelihood(allpaths,turnTimes,turnMethod,alpha,gamma1,gamma2,sim,model)
  }
  else if(model == 6){
    gamma = par[2]
    lambda = par[3]
    turnlik=Aca3Turns::getTurnsLikelihood(allpaths,alpha,gamma,lambda,sim,model)
  }
  
  negLogLik = (-1) *sum(turnlik)
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
  
  
  



