library(DEoptim)
library(rlist)
library(parallel)

getModelResults=function(ratdata, testingdata,sim, cl)
{
  end_index = getEndIndex(ratdata@allpaths, sim, limit=0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  allmodelRes = new("AllModelRes")
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment
  for(model in models)
  {
    for(method in creditAssignment)
    {
      modelData = updateModelData(ratdata, new("ModelData", Model=model, creditAssignment = method, sim=sim),cl)
      allmodelRes = addModelData(allmodelRes,modelData) 
    }
    
    
  }
  return(allmodelRes)
}

updateModelData=function(ratdata,modelData,cl)
{
  res = callOptimize(modelData,ratdata,allModels,cl)
  modelData = setModelParams(modelData,res)
  modelData = setModelResults(modelData,ratdata,allModels)
  
  
  return(modelData)
}



optimize=function(fn,argList,cl)
{
 
  np.val = length(argList$lower) * 10
  myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9, trace = FALSE, itermax = 200,cluster = cl)
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


negLogLikFunc=function(par,ratdata,half_index,modelData,testModel,sim) {
  
  alpha = par[1]
  creditAssignment = modelData@creditAssignment
  
 if(creditAssignment == "aca3"){
    gamma1 = par[2]
    gamma2 = par[3]
    # reward = par[4]
    # reward = 1+reward*9
    reward = 1
    # 
    modelData@alpha = alpha
    modelData@gamma1 = gamma1
    modelData@gamma2 = gamma2
    probMatrix = TurnsNew::getProbMatrix(ratdata,modelData,testModel,sim)
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
      #ratdata@allpaths = ratdata@allpaths[1:half_index,]
      turnlik=TurnsNew::getTurnsLikelihood(ratdata,modelData,testModel,sim)
      turnlik = turnlik[1:half_index]
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