#library(GA)
library(DEoptim)
library(rlist)
library(parallel)

getModelResults=function(ratdata, testingdata, sim, cl)
{
  end_index = getEndIndex(ratdata@allpaths, sim, limit=0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment
 
  worker.nodes = mpi.universe.size()-1
  print(sprintf("worker.nodes=%i",worker.nodes))
  cl <- makeCluster(mpi.universe.size()-1, type='PSOCK')
  registerDoParallel(cl)
  
  capture.output(clusterEvalQ(cl, .libPaths("/home/amoongat/R/x86_64-redhat-linux-gnu-library/3.6")),file='NUL')
  capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/ModelClasses.R")),file='NUL')  
    
    resMatrix <-
      foreach(model=models, .combine='rbind') %:%
        foreach(method=creditAssignment, .combine='comb') %dopar% {
          modelData =  new("ModelData", Model=model, creditAssignment = method, sim=sim)
          callOptimize(modelData,ratdata,cl)
      }
    
    
    #modelData = updateModelData(ratdata,resMatrix, models)
    allmodelRes = getAllModelResults(ratdata, resMatrix,testingdata, sim) 
    stopCluster(cl)
  
  return(allmodelRes)
}

getAllModelResults=function(ratdata, resMatrix, testingdata, sim)
{
  #res = callOptimize(modelData,ratdata,allModels)
  models = testingdata@models
  methods = testingdata@creditAssignment
  for(i in 1:length(models))
  {
    for(j in 1:length(methods))
    {
      modelData = new("ModelData", Model=models[i], creditAssignment = methods[j], sim=sim)
      modelData = setModelParams(modelData, resMatrix[i*(j-1),])
      modelData = setModelResults(modelData,ratdata,allModels)
      allmodelRes = addModelData(allmodelRes,modelData) 
    }
    
    
  }

  return(allmodelRes)
}


optimize=function(fn,argList,cl)
{ 
  
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/TurnModel.R")),file='NUL')
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel1.R")),file='NUL')
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel2.R")),file='NUL')
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel3.R")),file='NUL')
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/HybridModel4.R")),file='NUL')
  #capture.output(clusterEvalQ(cl, source("/home/amoongat/Projects/Rats-Credit/Sources/src/BaseClasses.R")),file='NUL') 
  capture.output(clusterExport(cl, varlist = c("argList","fn"),envir=environment()),file='NUL')
 
  print(sprintf("Inside optimize")) 
  np.val = length(argList$lower) * 10
  myList <- DEoptim.control(NP=np.val, F=0.8, CR = 0.9, trace = FALSE, itermax = 200,parallelType = 2)
  time <- system.time(out<-do.call("DEoptim",list.append(argList, fn=fn, myList)))
  #args <- list.append(argList,type = "real-valued", fitness = fn, monitor=FALSE, popSize = 50, maxiter = 100, run = 100, optim = TRUE, parallel = cl)
  #args <- list.append(argList,type = "real-valued", fitness = fn, monitor=FALSE, popSize = 100, parallel = cl, maxiter = 1000, run = 100,numIslands = 4,migrationRate = 0.2,			  migrationInterval = 50)

  #time<-system.time(out <- do.call("ga",args))
  #print(sprintf("optimize summary:"))
  #print(summary(out))
  #print(out@solution)
  print(out$optim$bestmem)
  print(time)
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
