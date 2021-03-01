


HoldoutTest=function(ratdata, testingdata)
{
  validated = TRUE
  
  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  
  pathmodels = testingdata@pathModels
  turnmodels = testingdata@turnModels

  print(sprintf("models: %s",toString(models)))
  for(model in models){
    print(sprintf("model= %s",model))
    
    trueModelData = new("ModelData", Name = model)
    trueModelData = updateTrueModelData(ratdta,currmodel)
    
    
    iter = 1
    start_index = 0
    end_index = 0
    missedOptimalIter = 0
    while(iter <= 100){
      total_trials = length(allpaths[,1])
      init_state = as.numeric(allpaths[1,2])-1
      
      generated_data = simulateModelData(ratdata, trueModelData)
      
      
      end_index = getEndIndex(generated_data@allpaths, sim=1, limit=0.95)
      
      if(end_index == -1){
        missedOptimalIter=missedOptimalIter+1
        if(missedOptimalIter>1000)
        {
          break
        }
        else
        {
          next
        }
        
      }
      
      allmodelRes = getModelResults(generated_data,models)
      # #debug(getTurnModelData)
      # res1 = getModelData(generated_data$PathData, models, window = window, sim=1)
      # res2 = getTurnModelData(generated_data$PathData, generated_data$TurnData, models, window = window, sim=1)
      
      min_method = getMinimumLikelihood(res1,res2)
      
      
      mat_res[toString(model),toString(min_method)] = mat_res[toString(model),toString(min_method)] + 1
      
      #print(sprintf("iter=%i", iter))
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
  
  print(sprintf("Returning mat_res")) 
  return(mat_res)
}


updateTrueModelData=function(ratdata,modelData)
{
  modelName = modelData@Name
  endLearningStage = getEndIndex(allpaths_num,sim=2, limit=0.95)
  endLearningStage = endLearningStage/2
  
  
  if(modelName == "aca3Paths"){
    Aca3Paths = setArgList(Aca3Paths,ratdata,endLearningStage)
    
    res = optimize(aca_negLogLik1,Aca3Paths@argList)
    modelData@alpha = res[1]
    modelData@gamma1 = res[2]
    modelData@gamma2 = res[3]
    
  }
  else if(modelName == "aca3Turns"){
    Aca3Turns = setArgList(Aca3Turns,ratdata,endLearningStage)
    
    res = optimize(negLogLikFunc,argList)
    modelData@alpha = res[1]
    modelData@gamma1 = res[2]
    modelData@gamma2 = res[3]
  }
  return(modelData)
}

simulateModelData=function(ratdata, modelData)
{
  modelName = modelData@Name
  allpaths = ratdata@allpaths
  turnTimes = ratdata@turnTimes
  endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
  turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
  endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
  turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
  endStage3 = length(allpaths[,1])
  turnIdxStage3 = length(turnTimes[,1])
  
  pathstages=c(1,endStage1,endStage2,endStage3)
  turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
  
  if(modelName == "aca3Paths"){
    alpha = modelData@alpha
    gamma1 = modelData@gamma1
    gamma2 = modelData@gamma2
    arglist = list(allpaths, turnTimes, alpha, gamma1,gamma2, pathstages)
    generated_data = do.call(Aca3Paths@simulateFunc,arglist)
  }
  else if(modelName == "aca3Turns"){
    alpha = modelData@alpha
    gamma1 = modelData@gamma1
    gamma2 = modelData@gamma2
    arglist = list(allpaths, turnTimes, alpha, gamma1,gamma2, turnstages)
    generated_data = do.call(Aca3Turns@simulateFunc,arglist)
  }
  
  generated_data = new("RatData", rat = "simulation",allpaths = generated_data$PathData, turnTimes = generated_data$TurnData)
  return(generated_data)
  
}


getMinimumLikelihood=function(res1, res2)
{
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
  
  return(min_method)
}

