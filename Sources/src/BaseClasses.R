allModels = new("AllModels",Paths = "Dummy", Turns = TurnModel,Hybrid1 = Hybrid1,Hybrid2 = Hybrid2, Hybrid3 = Hybrid3,Hybrid4 = Hybrid4)

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
setGeneric("callOptimize", function(x,ratdata,cluster)  standardGeneric("callOptimize"))
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


setMethod("callOptimize",  signature=c("ModelData","RatData","ANY"),
          definition=function(x,ratdata,cluster)
          {
            
            endLearningStage = getEndIndex(ratdata@allpaths,sim=x@sim, limit=0.95)
            model = x@Model
            testModel = slot(allModels,model)
            endLearningStage = endLearningStage/2
            
            argList = list(lower = c(0,0,0), 
                           upper = c(1,1,1),
                           ratdata = ratdata,
                           half_index = endLearningStage, 
                           modelData = x,
                           testModel = testModel,
                           sim = x@sim)
            #debug(optimize)
            # if(model == "Paths")
            # {
            #   endLearningStage = endLearningStage/2
            #   argList = list(lower = c(0,0,0), 
            #                  upper = c(1,1,1),
            #                  ratdata = ratdata, 
            #                  half_index = endLearningStage, 
            #                  modelData = x,
            #                  testModel = "Paths", 
            #                  sim = x@sim)
            #   
            #   #res = optimize(aca_negLogLik1,argList,cluster)
            # }
            # else if(model == "Turns")
            # {
            #   endLearningStage = endLearningStage/2
            #   argList = list(lower = c(0,0,0), 
            #                  upper = c(1,1,1),
            #                  ratdata = ratdata,
            #                  half_index = endLearningStage, 
            #                  modelData = x,
            #                  testModel = TurnModel,
            #                  sim = x@sim)
            #   #res = optimize(negLogLikFunc,argList,cluster)
            # }
            # else
            # {
            #   testModel = slot(allModels,model)
            #   testTurnTimes = convertTurnTimes(ratdata,TurnModel,testModel,sim=x@sim)
            #   endLearningStage = endLearningStage/2
            # 
            #   argList = list(lower = c(0,0,0), 
            #                  upper = c(1,1,1),
            #                  ratdata = ratdata,
            #                  half_index = endLearningStage, 
            #                  modelData = x,
            #                  testModel = testModel,
            #                  sim = x@sim)
            #   
            #   #res = optimize(negLogLikFunc,argList,cluster)
            # }

            return(argList)
          }
)


setMethod("setModelResults",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=x@sim, limit=0.95)
            baseModel = getBaseModel(x@Model)
            
            model = x@Model
            
            if(model == "Paths")
            {
              endLearningStage = endLearningStage/2
              x@probMatrix = baseModel@probMatFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
              likelihood = baseModel@likelihoodFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
              x@likelihood = (-1) * sum(likelihood[-(1:endLearningStage)])
            }
            else if(model == "Turns")
            {
              testTurnTimes = ratdata@turnTimes
              endLearningStage = endLearningStage/2
              x@probMatrix = baseModel@probMatFunc(ratdata, x,TurnModel,x@sim)
              likelihood = baseModel@likelihoodFunc(ratdata, x,TurnModel,x@sim)
              x@likelihood = (-1) * sum(likelihood[-(1:endLearningStage)])
              
            }
            else
            {
              testModel = slot(allModels,model)
              testTurnTimes = convertTurnTimes(ratdata,TurnModel,testModel,sim=x@sim)
              hybridRatdata = new("RatData",allpaths=ratdata@allpaths,turnTimes =testTurnTimes )
              endLearningStage = endLearningStage/2
              x@probMatrix = baseModel@probMatFunc(hybridRatdata,x,testModel,x@sim)
              likelihood = baseModel@likelihoodFunc(hybridRatdata,x,testModel,x@sim)
              x@likelihood = (-1) *sum(likelihood[-(1:endLearningStage)])
            }
            return(x)
          }
        )

setMethod("simulateData",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            endStage1 = getEndIndex(ratdata@allpaths,sim=2,limit=0.5)
            endStage2 = getEndIndex(ratdata@allpaths,sim=2,limit=0.95)
            endStage3 = length(ratdata@allpaths[,1])
            pathstages=c(1,endStage1,endStage2,endStage3)
            
            
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
            else if(x@Model == "Turns")
            {
              
              turnIdxStage1 = last(which(ratdata@turnTimes[,1]<=endStage1))
              turnIdxStage2 = last(which(ratdata@turnTimes[,1]<=endStage2))
              turnIdxStage3 = length(ratdata@turnTimes[,1])
              turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
              model = x@Model
              testModel = TurnModel
              testTurnTimes = ratdata@turnTimes
              argList = list(ratdata = ratdata, 
                             testTurnTimes = testTurnTimes,
                             modelData = x,
                             testModel = testModel,
                             turnstages = turnstages)
              
              generated_data = TurnsNew::simulateTurnsModels(ratdata,testTurnTimes,x,testModel,turnstages)
            }
            else
            {
              model = x@Model
              testModel = slot(allModels,model)
              testTurnTimes = convertTurnTimes(ratdata,TurnModel,testModel,sim=x@sim)
              
              turnIdxStage1 = last(which(testTurnTimes[,1]<=endStage1))
              turnIdxStage2 = last(which(testTurnTimes[,1]<=endStage2))
              turnIdxStage3 = length(testTurnTimes[,1])
              turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
              
              argList = list(ratdata = ratdata, 
                             testTurnTimes = testTurnTimes,
                             modelData = x,
                             testModel = testModel,
                             turnstages = turnstages)
              
              generated_data = TurnsNew::simulateTurnsModels(ratdata,testTurnTimes,x,testModel,turnstages)
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



