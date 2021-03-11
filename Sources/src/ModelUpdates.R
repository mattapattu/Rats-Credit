library(DEoptim)
library(rlist)

getModelResults=function(ratdata, models,sim)
{
  end_index = getEndIndex(ratdata@allpaths, sim, limit=0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  allmodels = new("AllModels", models=models)
  for(model in models)
  {
    modelData = updateModelData(ratdata, new("ModelData", Name=model, sim=sim))
    allmodels = addModelDataToResult(allmodels,model,modelData)
  }
  return(allmodels)
}

updateModelData=function(ratdata,modelData)
{
  modelName = modelData@Name
  # endLearningStage = getEndIndex(allpaths_num,sim=2, limit=0.95)
  # endLearningStage = endLearningStage/2
  
  
  res = callOptimize(modelData,ratdata)
  
  
  modelData = setModelParams(modelData,res)
  modelData = setResults(modelData,ratdata)
  
  
  return(modelData)
}



optimize=function(fn,argList)
{
  np.val = length(argList$lower) * 10
  myList <- DEoptim.control(NP=np.val, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1,reltol = 0.0005, steptol = 10)
  out<-do.call("DEoptim",list.append(argList, fn=fn, myList))
  return(out$optim$bestmem)
  
}

aca_negLogLik1=function(par,allpaths,model,half_index, sim) {
  
  alpha = par[1]
  
  
  if(model == 5){
    Hinit = matrix(0,2,6)
    gamma1 = par[2]
    gamma2 = par[3]
    
    probMatrix = Aca3::getProbMatrix(allpaths, alpha, gamma1, gamma2, sim)
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
      lik = Aca3::getPathLikelihood(allpaths[1:half_index,], alpha,gamma1,gamma2, sim)
    }
    else
    {
      lik = -1000000
    }
  
  }
  
  negLogLik = (-1) *sum(lik)
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


negLogLikFunc=function(par,allpaths,turnTimes,half_index,model,sim) {
  
  alpha = par[1]
  
 if(model == 5){
    gamma1 = par[2]
    gamma2 = par[3]
    # reward = par[4]
    # reward = 1+reward*9
    reward = 1
    # 
    probMatrix = TurnsNew::getProbMatrix(allpaths,turnTimes,alpha,gamma1,gamma2,1,sim)
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
      turnlik=TurnsNew::getTurnsLikelihood(allpaths[1:half_index,],turnTimes,alpha,gamma1,gamma2,1,sim)
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