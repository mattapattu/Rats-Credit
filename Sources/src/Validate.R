


HoldoutTest=function(ratdata, testingdata)
{
  validated = TRUE
  
  pathmodels = testingdata@pathModels
  turnmodels = testingdata@turnModels
  
  models = c(pathmodels,turnmodels)
  
  mat_res = matrix(0, length(models), length(models))
  colnames(mat_res) <- models
  rownames(mat_res) <- models
  

  print(sprintf("models: %s",toString(models)))
  for(model in models){
    print(sprintf("model= %s",model))
    
    trueModelData = new("ModelData", Name = model)
    trueModelData = updateModelData(ratdta,currmodel)
    
    
    iter = 1
    start_index = 0
    end_index = 0
    missedOptimalIter = 0
    while(iter <= 100){
      total_trials = length(allpaths[,1])
      init_state = as.numeric(allpaths[1,2])-1
      
      generated_data = simulateData(trueModelData,ratdata)
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
      min_method = getMinimumLikelihood(allmodelRes)
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






getMinimumLikelihood=function(allmodelRes)
{
  min_index = 0
  min = 100000
  min_method = "null"
  
  
  for(m in allmodelRes@models)
  {
    modelData = getModelData(allmodelRes,m)
    lik = modelData@likelihood
    if(lik < min)
    {
      min = lik
      min_method = modelData@Name
    }
    
  }
  
  return(min_method)
}

