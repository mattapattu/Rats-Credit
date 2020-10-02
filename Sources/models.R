library(Rmpfr)
library(DEoptim)
library(TTR)




acaData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA = DEoptim(aca_negLogLik1, lower = 0, upper = 1, H=Hinit2, allpaths = generated_data[1:half_index,], model=1, sim=sim, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
  alpha_ACA = ACA$optim$bestmem[1]
  
  params_lik = list("alpha"=alpha_ACA)
  
  empProbMat = baseModels::empiricalProbMat(generated_data[1:half_index,], window = window)
  ACA <- DEoptim(aca_negLogLik2,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data, start_index = half_index, empProbMatrix = empProbMat, model = 1, window = window, sim = sim, DEoptim.control(NP=10, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA = ACA$optim$bestmem[1]
  params_activity = list("alpha"=alpha_ACA)
  ACA_probMatrix = baseModels::getProbMatrix(generated_data, alpha_ACA, H=Hinit2, sim, model=1)
  
  acaActions = getActionData(generated_data, ACA_probMatrix, half_index, end_index, window, sim)
  actErr = getActivityErr(acaActions,half_index, end_index)
  lik = -1 * sum(baseModels::getPathLikelihood(generated_data[(half_index+1):end_index,], alpha_ACA, Hinit2, sim, model=1))
  ACA <- new("Model", Name = "ACA", Params_lik = params_lik, Params_activity = params_activity, Actions = acaActions, Metrics = list("activityErr" = actErr,"likelihood" = lik), ProbMatrix = ACA_probMatrix)
  
  return(ACA)
}

gbData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  
  GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 2, sim=sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_GB = GB$optim$bestmem[1]
  
  params_lik = list("alpha"=alpha_GB)
  
  empProbMat = baseModels::empiricalProbMat(generated_data[1:half_index,], window = window)
  GB <- DEoptim(aca_negLogLik2,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data, start_index = half_index, empProbMatrix = empProbMat, model = 2, window = window, sim = sim, DEoptim.control(NP=10, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_GB = GB$optim$bestmem[1]
  params_activity = list("alpha"=alpha_GB)
  GB_probMatrix = baseModels::getProbMatrix(generated_data, alpha_GB, H=Hinit2, sim, model=2)
  
  gbActions = getActionData(generated_data, GB_probMatrix, half_index, end_index, window, sim)
  actErr = getActivityErr(gbActions,half_index, end_index)
  lik = -1 * sum(baseModels::getPathLikelihood(generated_data[(half_index+1):end_index,], alpha_GB, Hinit2, sim, model=2))
  GB <- new("Model", Name = "GB", Params_lik = params_lik, Params_activity = params_activity, Actions = gbActions, Metrics = list("activityErr" = actErr,"likelihood" = lik), ProbMatrix = GB_probMatrix)
  
  return(GB)
}

gbAcaData = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA_GB <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 3, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA_GB = ACA_GB$optim$bestmem[1]
  ## Compute MSE for Model2 using simulated data
  GB_ACA_probMatrix <- baseModels::getProbMatrix(generated_data, alpha_ACA_GB,H=Hinit2, sim, model=3)
  gbacaActions = getActionData(generated_data, GB_ACA_probMatrix, half_index, end_index, window, sim)
  mse1 = getMse1(gbacaActions,half_index, end_index)
  mse2 = getMse2(gbacaActions,half_index, end_index)
  lik = -1 * sum(baseModels::getPathLikelihood(generated_data[(half_index+1):end_index,], alpha_ACA_GB, Hinit2, sim, model=3))
  GBACA <- new("Model", Name = "GB-ACA", Actions = gbacaActions, Metrics = list("mse1" = mse1,"mse2" = mse2,"likelihood" = lik), ProbMatrix = GB_ACA_probMatrix)
  
  return(GBACA)
}


aca2Data = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA2 <- DEoptim(aca_negLogLik1,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data[1:half_index,], model = 4, sim = sim, DEoptim.control(NP=10,F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA2 = ACA2$optim$bestmem[1]

  params_lik = list("alpha"=alpha_ACA2)
  
  empProbMat = baseModels::empiricalProbMat(generated_data[1:half_index,], window = window)
  ACA2 <- DEoptim(aca_negLogLik2,lower = 0, upper = 1, H = Hinit2, allpaths = generated_data, start_index = half_index, empProbMatrix = empProbMat, model = 4, window = window, sim = sim, DEoptim.control(NP=10, F=0.8, CR = 0.9,trace = FALSE, itermax = 20))
  alpha_ACA2 = ACA2$optim$bestmem[1]
  params_activity = list("alpha"=alpha_ACA2)
  ACA2_probMatrix = Aca2::getProbMatrix(generated_data, alpha_ACA2, H=Hinit2, sim, model=4)
  
  aca2Actions = getActionData(generated_data, ACA2_probMatrix, half_index, end_index, window, sim)
  actErr = getActivityErr(aca2Actions,half_index, end_index)
  lik = -1 * sum(Aca2::getPathLikelihood(generated_data[(half_index+1):end_index,], alpha_ACA2, Hinit2, sim, model=4))
  ACA2 <- new("Model", Name = "ACA2", Params_lik = params_lik, Params_activity = params_activity, Actions = aca2Actions, Metrics = list("activityErr" = actErr,"likelihood" = lik), ProbMatrix = ACA2_probMatrix)
  
  return(ACA2)
}

aca3Data = function(Hinit2, generated_data, sim, half_index, end_index, window){
  ACA3 <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data[1:half_index,], model = 5, sim = sim, DEoptim.control(NP=20, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma_ACA3 = ACA3$optim$bestmem[2]
  #ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma_ACA3, H=Hinit2, sim, model=5)
  params_lik = list("alpha"=alpha_ACA3, "gamma"=gamma_ACA3)
  
  empProbMat = baseModels::empiricalProbMat(generated_data[1:half_index,], window = window)
  ACA3 <- DEoptim(aca_negLogLik2,lower = c(0,0), upper = c(1,1), H = Hinit2, allpaths = generated_data, start_index = half_index, empProbMatrix = empProbMat, model = 5, window = window, sim = sim, DEoptim.control(NP=20, F=0.8, CR = 0.9,trace = FALSE, itermax = 50))
  alpha_ACA3 = ACA3$optim$bestmem[1]
  gamma_ACA3 = ACA3$optim$bestmem[2]
  params_activity = list("alpha"=alpha_ACA3, "gamma"=gamma_ACA3)
  ACA3_probMatrix = Aca3::getProbMatrix(generated_data, alpha_ACA3,gamma_ACA3, H=Hinit2, sim, model=5)

  aca3Actions = getActionData(generated_data, ACA3_probMatrix, half_index, end_index, window, sim)
  actErr = getActivityErr(aca3Actions,half_index, end_index)
  lik = -1 * sum(Aca3::getPathLikelihood(generated_data[(half_index+1):end_index,], alpha_ACA3, gamma_ACA3, Hinit2, sim, model=5))
  ACA3 <- new("Model", Name = "ACA3", Params_lik = params_lik, Params_activity = params_activity, Actions = aca3Actions, Metrics = list("activityErr" = actErr,"likelihood" = lik), ProbMatrix = ACA3_probMatrix)
  
  return(ACA3)
}

aca_negLogLik1=function(par,Hinit, allpaths,model,sim) {
  
  alpha = par[1]

  if(model == 1 || model == 2 || model == 3){
    lik = baseModels::getPathLikelihood(allpaths, alpha, Hinit, sim, model)
  }else if(model == 4){
    lik = Aca2::getPathLikelihood(allpaths, alpha, Hinit, sim, model)
  }else if(model == 5){
    gamma = par[2]
    lik = Aca3::getPathLikelihood(allpaths, alpha,gamma, Hinit, sim, model)
  }
  
  negLogLik = (-1) *sum(lik)
 # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(negLogLik)){
    return(1000000)
  }else{
    return(negLogLik)
  }
  
}


aca_negLogLik2=function(par,Hinit, allpaths, start_index, empProbMatrix, model, sim, window) {
  
  alpha = par[1]
  
  if(model == 1 || model == 2 || model == 3){
    
    model_probMatrix = baseModels::getProbMatrix(allpaths, alpha, H = Hinit, sim, model = model)
    
  }else if(model == 4){
    
    model_probMatrix = Aca2::getProbMatrix(allpaths, alpha, H = Hinit, sim, model = model)
    
  }else if(model == 5){
    
    gamma = par[2]
    model_probMatrix = Aca3::getProbMatrix(allpaths, alpha, gamma, H = Hinit, sim, model = model)
    
  }
  
  
  activityErr = 0
  if(sum(as.numeric(model_probMatrix[,4] > 0.9))>50 && sum(as.numeric(model_probMatrix[,10] > 0.9))){
    
    for(state in c(1,2)){
      if(sim == 1){
        #state_idx = which(generated_data[(start_index+1):end_index,2] == (state-1))
        state_idx = which(allpaths[1:start_index,2] == (state-1))
      }else{
        #state_idx = which(generated_data[(start_index+1):end_index,2] == state)
        state_idx = which(allpaths[1:start_index,2] == state)
      }
      
      for(act in c(4)){
        probVector =  model_probMatrix[state_idx,(6*(state-1)+act)]
        empProbVector = empProbMatrix[state_idx,(6*(state-1)+act)]
        
        modelActivity = abs(diff(probVector))
        # if(sum(modelActivity) !=0 ){
        #   modelActivity = modelActivity/mean(modelActivity)
        # }
        
        empActivity = abs(diff(empProbVector))
        # if(sum(empActivity) !=0 ){
        #   empActivity = empActivity/mean(empActivity)
        # }
        
        activityErr = activityErr + sum((modelActivity - empActivity ))
        
      }
    }
  }else{
    activityErr = 1000000
  }
  
  activityErr = abs(activityErr)
  
  
  # print(sprintf("negLogLik = %f",negLogLik))
  if(is.infinite(activityErr)){
    return(1000000)
  }else{
    return(activityErr)
  }
  
}



