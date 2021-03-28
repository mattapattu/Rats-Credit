
HoldoutTest=function(ratdata,allModelRes,testData,src.dir,setup.hpc)
{
  models = testData@Models
  creditAssignment = testData@creditAssignment

  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))

  mat_res = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res) <- modelNames
  rownames(mat_res) <- modelNames
  

  print(sprintf("models: %s",toString(modelNames)))
  
  
  #iter = 1
  
  if(setup.hpc)
  {
    #worker.nodes = mpi.universe.size()-1
    #print(sprintf("worker.nodes=%i",worker.nodes))
    cl <- makeCluster(100, type='PSOCK')
    
    #cl <- startMPIcluster(worker.nodes)
    #registerDoMPI(cl)
  }
  else
  {
    cl <- makeCluster(3, outfile = "")
    #registerDoParallel(cl)
  }
  
  capture.output(clusterExport(cl, varlist = c("getEndIndex", "convertTurnTimes","simulateData","src.dir","populateSimRatModel","getMinimumLikelihood")),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"ModelClasses.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"TurnModel.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel1.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel2.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel3.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"HybridModel4.R", sep="/"))),file='NUL')
  capture.output(clusterEvalQ(cl, source(paste(src.dir,"BaseClasses.R", sep="/"))),file='NUL')
  capture.output(clusterExport(cl, varlist = c("allModelRes","creditAssignment"),envir=environment()),file='NUL')
  capture.output(clusterEvalQ(cl, library("TTR")),file='NUL')
  capture.output(clusterEvalQ(cl, library("dplyr")),file='NUL')

  clusterCall(cl, function() {
    library(doParallel)
    NULL
  })
  
  registerDoParallel(cl)
  
  resVec <- 
    foreach(model=modelNames, .combine='rbind') %:%
    #print(sprintf("model= %s",model))
    #capture.output(clusterExport(cl, varlist = c("trueModelData"),envir=environment()),file='NUL')
      foreach(i=c(1:100), .combine='rbind', .inorder=FALSE) %dopar%{
        
        modelName = strsplit(model,"\\.")[[1]][1]
        creditAssignment = strsplit(model,"\\.")[[1]][2]
        trueModelData = slot(slot(allModelRes,modelName),creditAssignment)
        
        end_index = -1
        missedOptimalIter = 0

        while(end_index == -1){
          generated_data = simulateData(trueModelData,ratdata,allModels)
          end_index = getEndIndex(generated_data@allpaths, sim=1, limit=0.95)
          
          missedOptimalIter=missedOptimalIter+1
          
          if(missedOptimalIter>200)
          {
            break
          }
          set.seed(NULL)
        }
        
        if(end_index > -1)
        {
          generated_data = populateSimRatModel(generated_data,modelName)
          allmodelRes = getModelResults(generated_data,testData,sim=1, src.dir, setup.hpc)
          min_method = getMinimumLikelihood(allmodelRes,testData)
          
        }
        else
        {
          min_method = "Not converging"
        }
  
        res_vec = c(toString(model),toString(min_method))

    }
    #save(mat_res, file = paste0(rat,"_mat_res.Rdata"))
    #print(sprintf("Nb of iterations where optimal behaviour was not learned=%i", missedOptimalIter))
    #print(mat_res)
  if(setup.hpc)
  {
    stopCluster(cl)
    stopImplicitCluster()
    #closeCluster(cl)
  }
  else
  {
    stopCluster(cl)
    stopImplicitCluster()
  }

 rat = ratdata@rat
 save(resVec, file = paste0(rat,"_resVec.Rdata"))
    
    #boxplotMse(mat_res,model,rat)
    
    # if(!checkValidation(mat_res,model,rat)){
    #   validated = FALSE
    #   break
    # }
    
}
  

getMinimumLikelihood=function(allmodelRes,testingdata)
{
  min_index = 0
  min = 100000
  min_method = "null"
  
  for(m in testingdata@Models)
  {
    for(crAssgn in testingdata@creditAssignment)
    {
      modelData = getModelData(allmodelRes,m,crAssgn)
      lik = modelData@likelihood
      modelName = paste(modelData@Model,modelData@creditAssignment,sep=".")
      
      print(sprintf("model=%s,likelihood=%f",modelName,lik))
      
      if(lik < min)
      {
        min = lik
        min_method = modelName
      }
    }
  }
  return(min_method)
}

