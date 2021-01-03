setClass("Action", 
         slots = list(
           action="numeric",
           empActivity = "vector",
           modelActivity = "vector")
          )


setClass("Model", 
         slots = list(
           Name = "character", 
           Params_lik = "list",
           Metrics ="list",
           ProbMatrix = "matrix")
           
)




getModelData = function(generated_data, models, window, sim){
  
  #start_index = getStartIndex(generated_data)
  end_index = getEndIndex(generated_data, sim)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  #end_index = length(generated_data[,1])
  #Hinit1 <-genInitValues(generated_data,sim=sim)
  #Qinit <-genInitValues(generated_data,sim=sim)
  Hinit1 = matrix(0,2,6)
  Qinit = matrix(0,2,6)
  #empProbMat <- getEmpProbMat(generated_data,window=window,sim=sim)
  
  
  
  
  acamse = list()
  gbmse = list()
  gbacamse = list()
  aca2mse = list()
  aca3mse = list()
  sarsamse = list()
  
  if("aca" %in% models){
    acamse = acaData(Hinit1, generated_data, sim=sim, start_index, end_index, window)
  }
  if("gb" %in% models){
    gbmse = gbData(Hinit1, generated_data, sim=sim, start_index, end_index, window)
  }
  if("gbaca" %in% models){
    gbacamse = gbAcaData(Hinit1, generated_data, sim=1, start_index, end_index, window)
  }
  if("aca2" %in% models){
    aca2mse = aca2Data(Hinit1, generated_data, sim=sim, start_index, end_index, window)
  }
  if("aca3" %in% models){
    aca3mse = aca3Data(Hinit1, generated_data, sim=sim, start_index, end_index, window)
  }
  if("sarsa" %in% models){
    sarsamse = sarsaData(Qinit, generated_data, sim=sim, start_index, end_index, window)
  }
  
  
  return(list("acamse"=acamse,"gbmse"=gbmse,"gbacamse"=gbacamse, "aca2mse"=aca2mse, "aca3mse"=aca3mse, "sarsamse"=sarsamse))
  
}


validateHoldout=function(models,Hinit,endLearningStage,allpaths_num, turnTimes, window, rat){
  validated = TRUE
  alpha=0
  gamma=0
  turnMethod = 0
  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  endLearningStage = endLearningStage/2
  
  for(model in models){
    
    if(model == "aca"){
      out = DEoptim(aca_negLogLik1, lower = 0, upper = 1, Hinit=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = 1, sim=2, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
      alpha = out$optim$bestmem[1]
    }
    else if(model == "gb"){
      out = DEoptim(aca_negLogLik1, lower = 0, upper = 1, Hinit=Hinit, allpaths = allpaths_num[1:endLearningStage,],  model = 2, sim=2, DEoptim.control(NP = 10,F = 0.8, CR = 0.9, trace = FALSE, itermax = 20))
      alpha = out$optim$bestmem[1]
    }
    else if(model == "aca2"){
      out <- DEoptim(aca_negLogLik1,lower = c(0,0), upper = c(1,1),Hinit=Hinit, allpaths = allpaths_num[1:endLearningStage,], model = 4, sim = 2, DEoptim.control(NP=20, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = out$optim$bestmem[1]
      gamma1 = out$optim$bestmem[2]
    }
    else if(model == "aca3"){
      out <- DEoptim(aca_negLogLik1,lower = c(0,0,0), upper = c(1,1,1),Hinit=Hinit, allpaths = allpaths_num[1:endLearningStage,], model = 5, sim = 2, DEoptim.control(NP=30, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = out$optim$bestmem[1]
      gamma1 = out$optim$bestmem[2]
      gamma2 = out$optim$bestmem[3]
    }
    else if(model == "sarsa"){
      out <- DEoptim(aca_negLogLik1,lower = c(0,0,0,0), upper = c(1,1,1,1),Hinit=Hinit, allpaths = allpaths_num[1:endLearningStage,], model = 6, sim = 2, DEoptim.control(NP=40, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = out$optim$bestmem[1]
      gamma = out$optim$bestmem[2]
      lambda = out$optim$bestmem[3]
      reward = out$optim$bestmem[4]
      reward = 1 + reward*9
    }
    else if(model == "acaTurns"){
      ACA = DEoptim(negLogLikFunc, lower = c(0,0), upper = c(1,0), allpaths = allpaths_num[1:endLearningStage,], turnTimes = turnTimes, turnMethod = turnMethod, model=1, sim=2, DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
      alpha_ACA = ACA$optim$bestmem[1]
    }
    else if(model == "gbTurns"){
      GB = DEoptim(negLogLikFunc, lower = c(0,0), upper = c(1,0), allpaths = allpaths_num[1:endLearningStage,], turnTimes = turnTimes, turnMethod = turnMethod, model=2, sim=2, DEoptim.control(NP = 20,F = 0.8, CR = 0.9, trace = FALSE, itermax = 200))
      alpha_GB = GB$optim$bestmem[1]
    }
    else if(model == "aca2Turns"){
      ACA3 <- DEoptim(negLogLikFunc,lower = c(0,0), upper = c(1,1), allpaths = allpaths_num[1:endLearningStage,],  turnTimes = turnTimes, turnMethod = turnMethod, model = 4, sim = 2, DEoptim.control(NP=20, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = ACA3$optim$bestmem[1]
      gamma1 = ACA3$optim$bestmem[2]
    }
    else if(model == "aca3Turns"){
      ACA3 <- DEoptim(negLogLikFunc,lower = c(0,0,0,0), upper = c(1,1,1,0), allpaths = allpaths_num[1:endLearningStage,],  turnTimes = turnTimes, turnMethod = turnMethod, model = 5, sim = 2, DEoptim.control(NP=40, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = ACA3$optim$bestmem[1]
      gamma1 = ACA3$optim$bestmem[2]
      gamma2 = ACA3$optim$bestmem[3]
    }
    else if(model == "sarsaTurns"){
      SARSA <- DEoptim(negLogLikFunc,lower = c(0,0,0,0), upper = c(1,1,1,1), allpaths = allpaths_num[1:endLearningStage,], turnTimes = 0, turnMethod = 0, model = 6, sim = 2, DEoptim.control(NP=40, F=0.8, CR = 0.9,trace = FALSE, itermax = 200))
      alpha = SARSA$optim$bestmem[1]
      gamma = SARSA$optim$bestmem[2]
      lambda = SARSA$optim$bestmem[3]
      reward = SARSA$optim$bestmem[4]
      reward = 1 + reward*9
    }
    
    #mat_res = matrix(0,nrow=100,ncol=(2*length(models)+1))
    #colnames(mat_res) = c("ACA loglikhood","ACA bestval","GB loglikhood","GB bestval","ACA2 loglikhood","ACA2 bestval","ACA3 loglikhood","ACA3 bestval", "Selected Model")
    
    
    iter = 1
    start_index = 0
    end_index = 0
    missedOptimalIter = 0
    while(iter <= 100){
      total_trials = length(allpaths_num[,1])
      init_state = as.numeric(allpaths_num[1,2])-1
      
      if(model == "aca"){
        generated_data = baseModels::simulateTrials(allpaths_num, turnTimes, alpha, model=1, turnMethod=0)
      }
      else if(model == "gb"){
        generated_data = Aca2::simulateTrials(allpaths_num, turnTimes, alpha, model=2, turnMethod=0)
      }
      else if(model == "aca2"){
        generated_data = Aca2::simulateTrials(allpaths_num, turnTimes, alpha, gamma1,turnMethod=0)
      }
      else if(model == "aca3"){
        generated_data = Aca3::simulateTrials(allpaths_num, turnTimes, alpha, gamma1,gamma2, turnMethod=0)
      }
      else if(model == "sarsa"){
        generated_data = Sarsa::simulateSarsa(allpaths_num, turnTimes, alpha, gamma, lambda, reward, turnMethod=0)
      }
      else if(model == "acaTurns"){
        generated_data = TurnsModels::simulateTurnsModels(allpaths_num, turnTimes, alpha, model=1, turnMethod=0)
      }
      else if(model == "gbTurns"){
        generated_data = TurnsModels::simulateTurnsModels(allpaths_num, turnTimes, alpha, model=2, turnMethod=0)
      }
      else if(model == "aca2Turns"){
        generated_data = Aca2Turns::simulateTurnsModels(allpaths_num, turnTimes, alpha, gamma1,turnMethod=0)
      }
      else if(model == "aca3Turns"){
        generated_data = Aca3Turns::simulateTurnsModels(allpaths_num, turnTimes, alpha, gamma1,gamma2, turnMethod=0)
      }
      else if(model == "sarsaTurns"){
        generated_data = SarsaTurns::simulateSarsa(allpaths_num, turnTimes, alpha, gamma, lambda, reward, turnMethod=0)
      }
      
      
      # if(length(which(SMA(generated_data[,3],10) >= 0.9)) < 500){
      #   missedOptimalIter=missedOptimalIter+1
      #   next
      # }
      #generated_data[,1:2]=generated_data[,1:2]+1
      end_index = getEndIndex(generated_data$PathData, sim=1)
      if(end_index == -1){
        missedOptimalIter=missedOptimalIter+1
        next
      }
      
      #debug(getTurnModelData)
      res1 = getModelData(generated_data$PathData, models, window = window, sim=1)
      res2 = getTurnModelData(generated_data$PathData, generated_data$TurnData, models, window = window, sim=1)
      

      min_index = 0
      min = 100000
      min_method = "null"
      
      
      for(m in models)
      {
        if(m == "aca")
        {
          if(res1$acamse@Metrics$likelihood < min)
          {
            min = res1$acamse@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "gb")
        {
          if(res1$gbmse@Metrics$likelihood < min)
          {
            min = res1$gbmse@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "aca3")
        {
          if(res1$aca3mse@Metrics$likelihood < min)
          {
            min = res1$aca3mse@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "sarsa")
        {
          if(res1$sarsamse@Metrics$likelihood < min)
          {
            min = res1$sarsamse@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "acaTurns")
        {
          if(res2$acaTurnData@Metrics$likelihood < min)
          {
            min = res2$acaTurnData@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "gbTurns")
        {
          if(res2$gbTurnData@Metrics$likelihood < min)
          {
            min = res2$gbTurnData@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "aca3Turns")
        {
          if(res2$aca3TurnData@Metrics$likelihood < min)
          {
            min = res2$aca3TurnData@Metrics$likelihood
            min_method = m
          }
          
        }
        else if(m == "sarsaTurns")
        {
          if(res2$sarsaTurnData@Metrics$likelihood < min)
          {
            min = res2$sarsaTurnData@Metrics$likelihood
            min_method = m
          }
          
        }
      }
      
      mat_res[toString(model),toString(min_method)] = mat_res[toString(model),toString(min_method)] + 1
      
      print(sprintf("iter=%i", iter))
      iter=iter+1
    }
    
    #save(mat_res, file = paste0(rat,"_mat_res.Rdata"))
    print(sprintf("Nb of iterations where optimal behaviour was not learned=%i", missedOptimalIter))
    print(mat_res)
    
    #boxplotMse(mat_res,model,rat)
    
    # if(!checkValidation(mat_res,model,rat)){
    #   validated = FALSE
    #   break
    # }
    
  }
  
  # if(validated){
  #   print(sprintf("All 3  models validated for %s.",rat)) 
  # }else{
  #   print(sprintf("Model %d  failed validation for %s.",model, rat))
  # }
  return(mat_res)
}


