


HoldoutTest=function(ratdata, testingdata)
{
  models = testingdata@Models
  creditAssignment = testingdata@creditAssignment

  modelNames = as.vector(sapply(creditAssignment, function(x) paste(models, x, sep=".")))

  mat_res = matrix(0, length(modelNames), length(modelNames))
  colnames(mat_res) <- modelNames
  rownames(mat_res) <- modelNames
  

  print(sprintf("models: %s",toString(modelNames)))
  
  for(model in modelNames){
    print(sprintf("model= %s",model))
    
    modelName = strsplit(model,"\\.")[[1]][1]
    creditAssignment = strsplit(model,"\\.")[[1]][2]
    trueModelData = new("ModelData", Model = modelName, creditAssignment = creditAssignment, sim=2)
    trueModelData = updateModelData(ratdata,trueModelData)
    
    
    iter = 1
    start_index = 0
    end_index = 0
    missedOptimalIter = 0
    while(iter <= 1){
      total_trials = length(allpaths[,1])
      init_state = as.numeric(allpaths[1,2])-1
      
      generated_data = simulateData(trueModelData,ratdata,allModels)
      
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
      
      allmodelRes = getModelResults(generated_data,testingdata,sim=1)
      min_method = getMinimumLikelihood(allmodelRes,testingdata)
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

