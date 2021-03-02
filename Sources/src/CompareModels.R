getModelResults=function(ratdata, models)
{
  end_index = getEndIndex(generated_data, sim, limit=0.95)
  start_index = round(end_index/2)
  if(start_index >= end_index){
    print(sprintf("start_index >= end_index. Check if rat learns optimal behavior"))
    return()
  }
  
  end_index = 0
  
  allmodels = new("AllModels", models=models)
  for(model in models)
  {
    modelData = updateModelData(ratdata, new("ModelData", Name=model))
    allmodels = addModelDataToResult(allmodels,model,modelData)
  }
  return(allmodels)
}

updateModelData=function(ratdata,modelData)
{
  modelName = modelData@Name
  # endLearningStage = getEndIndex(allpaths_num,sim=2, limit=0.95)
  # endLearningStage = endLearningStage/2
  
  
  if(modelName == "aca3Paths"){

    res = callOptimize(Aca3Paths,ratdata)

  }
  else if(modelName == "aca3Turns"){

    res = callOptimize(Aca3Turns, ratdata)
  }
  
  modelData = setModelParams(modelData,res)
  modelData = setResults(modelData)
  
  
  return(modelData)
}



optimize=function(fn,argList)
{
  np.val = length(argList$lower) * 10
  crtlList <- DEoptim.control(NP=np.val, F=2, CR = 0.9,trace = FALSE, itermax = 100,parallelType = 1,reltol = 0.0005, steptol = 10)
  out <- DEoptim(fn,argList,crtlList)
  return(out$optim$bestmem)
  
}