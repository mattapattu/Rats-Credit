#library(GA)
#library(DEoptim)
library(rgenoud)
library(rlist)
library(foreach)
library(Rmpi)
library(snow);


getModelResults=function(ratdata, testingdata, sim, src.dir, setup.hpc)
{
  end_index = getEndIndex(ratdata@allpaths, sim, limit=0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment
 
  if(setup.hpc)
  {
    worker.nodes = mpi.universe.size()-1
    print(sprintf("worker.nodes=%i",worker.nodes))
    #cl <- makeCluster(mpi.universe.size()-1, type='MPI')
    cl <- makeMPIcluster(worker.nodes)
    
  }
  else
  {
    cl <- makeCluster(detectCores()-1, outfile = "")
  }
  registerDoParallel(cl)
  
  capture.output(clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","negLogLikFunc","src.dir")),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"ModelClasses.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"TurnModel.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel1.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel2.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel3.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel4.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"BaseClasses.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, library("TTR")))
  capture.output(clusterEvalQ(cl, library("rlist")))
  capture.output(clusterEvalQ(cl, library("rgenoud")))
  capture.output(clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","negLogLikFunc")),file='NUL')

  time <- system.time(
 
   resMatrix <-
      foreach(model=models, .combine='rbind') %:%
        foreach(method=creditAssignment, .combine='rbind') %dopar% {
          modelData =  new("ModelData", Model=model, creditAssignment = method, sim=sim)
          argList<-getArgList(modelData,ratdata)
          nvar = length(argList$lower)
          out<-do.call("genoud",list.append(argList[3:7],
                                        fn=negLogLikFunc,
                                        nvars = nvars,
                                        pop.size=3000,
                                        max=FALSE,
                                        boundary.enforcement =2,
                                        wait.generations=5,
                                        solution.tolerance = 0.5,
					cluster = cl,
                                        print.level=0,
                                        gradient.check= FALSE,
                                        Domains = cbind(c(argList[[1]]),c(argList[[2]]))
                                        )
                   )
       	res <-out$par
      }
   )   
   print(time) 
    #modelData = updateModelData(ratdata,resMatrix, models)
    allmodelRes = getAllModelResults(ratdata, resMatrix,testingdata, sim) 
    stopCluster(cl)
  
  return(allmodelRes)
}

getAllModelResults=function(ratdata, resMatrix, testingdata, sim)
{
  #res = callOptimize(modelData,ratdata,allModels)
  models = testingdata@Models
  methods = testingdata@creditAssignment
  allmodelRes = new("AllModelRes")
  for(i in 1:length(models))
  {
    for(j in 1:length(methods))
    {
      modelData = new("ModelData", Model=models[i], creditAssignment = methods[j], sim=sim)
      index = length(methods)*(i-1) + j
      modelData = setModelParams(modelData, resMatrix[index,])
      modelData = setModelResults(modelData,ratdata,allModels)
      allmodelRes = addModelData(allmodelRes,modelData) 
    }
    
    
  }

  return(allmodelRes)
}



negLogLikFunc=function(par,ratdata,half_index,modelData,testModel,sim) {
  
  alpha = par[1]
  Model = modelData@Model
  creditAssignment = modelData@creditAssignment

  if(Model == "Paths")
 {
    if(creditAssignment == "aca3"){
      Hinit = matrix(0,2,6)
      gamma1 = par[2]
      gamma2 = par[3]
      
      probMatrix = Aca3::getProbMatrix(ratdata@allpaths, alpha, gamma1, gamma2, sim)
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
        lik = Aca3::getPathLikelihood(ratdata@allpaths[1:half_index,], alpha,gamma1,gamma2, sim)
      }
      else
      {
        lik = -1000000
      }
      
    }
 }
  else
  {
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
        lik=TurnsNew::getTurnsLikelihood(ratdata,modelData,testModel,sim)
        lik = lik[1:half_index]
      }
      else
      {
        lik = -1000000
      }
      
      
    }   
  }  
 
  negLogLik = (-1) *sum(lik)
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
