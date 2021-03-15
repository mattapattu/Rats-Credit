allModels = new("AllModels",Turns = TurnModel,Hybrid1 = Hybrid1)

setClass("RatData", 
         slots = list(
           rat = "character",
           allpaths="matrix",
           turnTimes = "matrix",
           hybridModel1 = "matrix",
           hybridModel2 = "matrix",
           hybridModel3 = "matrix",
           hybridModel4 = "matrix")
)


setClass("TestModels", 
         slots = list(
           Models="character",
           creditAssignment = "character"
           )
)

setClass("BaseModel", 
         slots = list(
           Name = "character", 
           simulateFunc = "function",
           likelihoodFunc ="function",
           probMatFunc = "function",
           rule="character",
           type="character")
         )

setClass("ModelData", 
         slots = list(
           Model = "character", 
           creditAssignment = "character",
           alpha = "numeric",
           gamma1 = "numeric",
           gamma2 = "numeric",
           lambda = "numeric",
           likelihood = "numeric",
           probMatrix = "matrix",
           sim = "numeric"
           )
        )

setClass("ModelDataList",
         slots = list(
           Model = "character", 
           aca = "ModelData",
           aca2 = "ModelData",
           aca3 = "ModelData",
           gb = "ModelData",
           sarsa = "ModelData"
         )
)


setClass("AllModelRes", 
         representation(
           models = "vector",
           Paths = "ModelDataList",
           Turns = "ModelDataList",
           Hybrid1 = "ModelDataList",
           Hybrid2 = "ModelDataList",
           Hybrid3 = "ModelDataList",
           Hybrid4 = "ModelDataList",
           type = "character"),
         contains = "ModelData"
)


#### func setModelParams ###
setGeneric("setModelParams", function(x,modelParams) standardGeneric("setModelParams"))
setGeneric("callOptimize", function(x,ratdata,allModels)  standardGeneric("callOptimize"))
setGeneric("setModelResults", function(x,ratdata, allModels)  standardGeneric("setModelResults"))
setGeneric("simulateData", function(x,ratdata,allModels) standardGeneric("simulateData"))
setGeneric("addModelData", function(x,modelData) standardGeneric("addModelData"))
setGeneric("getModelData", function(x,modelName,creditAssignment) standardGeneric("getModelData"))


setMethod("setModelParams",  signature=c("ModelData","numeric"),
          definition=function(x,modelParams)
          {
            if(x@creditAssignment == "aca3")
            {
              x@alpha = modelParams[1]
              x@gamma1 = modelParams[2]
              x@gamma2 = modelParams[3]
            }
            
            return(x)
          }
)


setMethod("callOptimize",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=x@sim, limit=0.95)
            endLearningStage = endLearningStage/2
            if(x@Model == "Paths")
            {
              argList = list(lower = c(0,0,0), 
                             upper = c(1,1,1),
                             allpaths = ratdata@allpaths, 
                             half_index = endLearningStage, 
                             model = 5, 
                             sim = x@sim)
              res = optimize(aca_negLogLik1,argList)
            }
            else
            {
              model = x@Model
              argList = list(lower = c(0,0,0), 
                             upper = c(1,1,1),
                             allpaths = ratdata@allpaths, 
                             turnTimes = ratdata@turnTimes,
                             half_index = endLearningStage, 
                             creditAssignment = "aca3",
                             turnModel = slot(allModels,model),
                             sim = x@sim)
              
              res = optimize(negLogLikFunc,argList)
            }
            
            
            
            return(res)
          }
)


setMethod("setModelResults",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=x@sim, limit=0.95)
            endLearningStage = endLearningStage/2
            baseModel = getBaseModel(x@Model)
            

            
            if(x@Model == "Paths")
            {
              
              x@probMatrix = baseModel@probMatFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
              likelihood = baseModel@likelihoodFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
              x@likelihood = sum(likelihood[-(1:endLearningStage)])
            }
            else
            {
              modelName = x@Model
              model = slot(allModels,modelName)
              x@probMatrix = baseModel@probMatFunc(ratdata@allpaths,ratdata@turnTimes, x@alpha,x@gamma1,x@gamma2,1,model,x@sim)
              likelihood = baseModel@likelihoodFunc(ratdata@allpaths,ratdata@turnTimes,x@alpha,x@gamma1,x@gamma2,1, model,x@sim)
              if(x@sim==1)
              {
                endLearningStage = last(which(ratdata@turnTimes[,6]<=endLearningStage))
              }
              else
              {
                endLearningStage = last(which(ratdata@turnTimes[,1]<=endLearningStage))
              }
              x@likelihood = sum(likelihood[-(1:endLearningStage)])
              
            }
            return(x)
          }
        )

setMethod("simulateData",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            endStage1 = getEndIndex(ratdata@allpaths,sim=2,limit=0.5)
            turnIdxStage1 = last(which(ratdata@turnTimes[,1]<=endStage1))
            endStage2 = getEndIndex(ratdata@allpaths,sim=2,limit=0.95)
            turnIdxStage2 = last(which(ratdata@turnTimes[,1]<=endStage2))
            endStage3 = length(ratdata@allpaths[,1])
            turnIdxStage3 = length(ratdata@turnTimes[,1])
            
            pathstages=c(1,endStage1,endStage2,endStage3)
            turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
            
            
            
            if(x@Model == "Paths")
            {
              argList = list(allpaths = ratdata@allpaths, 
                             turnTimes = ratdata@turnTimes,
                             alpha = x@alpha,
                             gamma1 = x@gamma1,
                             gamma2 = x@gamma2,
                             pathStages = pathstages)
              generated_data = do.call(pathModelFuncs@simulateFunc,argList)
            }
            else
            {
              model = x@Model
              argList = list(allpaths = ratdata@allpaths, 
                             turnTimes = ratdata@turnTimes,
                             alpha = x@alpha,
                             gamma1 = x@gamma1,
                             gamma2 = x@gamma2,
                             turnModel = allModels@model,
                             turnStages = turnstages)
              
              generated_data = do.call(turnModelFuncs@simulateFunc,argList)
            }
            
            
            simData = new("RatData", rat = "simulation",allpaths = generated_data$PathData, turnTimes = generated_data$TurnData)
            return(simData)
          }
)


setMethod("addModelData",  signature=c("AllModelRes","ModelData"),
          function(x,modelData)
          {
            model = modelData@Model
            creditAssignment = modelData@creditAssignment
            

            slot(slot(x,model),creditAssignment) = modelData
            
            return(x)
          }
)


setMethod("getModelData",  signature=c("AllModelRes","character","character"),
          function(x,modelName,creditAssignment)
          {
            
            return(slot(slot(x,modelName),creditAssignment))
          }
)





### Models 

pathModelFuncs = new("BaseModel", 
                Name = "PathModel", 
                simulateFunc = Aca3::simulateTrials, 
                likelihoodFunc = Aca3::getPathLikelihood,
                probMatFunc = Aca3::getProbMatrix,
                type = "paths")

turnModelFuncs = new("BaseModel", 
                Name = "TurnModel", 
                simulateFunc = TurnsNew::simulateTurnsModels, 
                likelihoodFunc = TurnsNew::getTurnsLikelihood,
                probMatFunc = TurnsNew::getProbMatrix,
                type = "turns")


getBaseModel=function(modelName)
{
  if(modelName == "Paths")
  {
    baseModel = pathModelFuncs
  }
  else
  {
    baseModel = turnModelFuncs
  } 
  return(baseModel)
}



