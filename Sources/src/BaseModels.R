setClass("RatData", 
         slots = list(
           rat = "character",
           allpaths="matrix",
           turnTimes = "matrix",
           hybridModel1 = "matrix",
           hybridModel2 = "matrix")
)


setClass("TestData", 
         slots = list(
           pathModels="vector",
           turnModels = "vector",
           hybridModels1 = "vector",
           hybridModels2 = "vector")
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

#### func callOptimize ###
setGeneric("callOptimize", function(x,ratdata) 
  standardGeneric("callOptimize") )

setMethod("callOptimize",  signature=c("BaseModel","RatData"),
          definition=function(x,ratdata)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=2, limit=0.95)
            endLearningStage = endLearningStage/2
            if(x@Name == "aca3Paths")
            {
              argList = list(lower = c(0,0,0), 
                               upper = c(1,1,1),
                               allpaths = ratdata@allpaths, 
                               half_index = endLearningStage, 
                               model = 5, 
                               sim = x@sim)
              res = optimize(aca_negLogLik1,argList)
            }
            else if(x@Name == "aca3Turns")
            {
              argList = list(lower = c(0,0,0), 
                               upper = c(1,1,1),
                               allpaths = ratdata@allpaths, 
                               turnTimes = ratdata@turnTimes,
                               half_index = endLearningStage, 
                               model = 5, 
                               sim = x@sim)
              
              res = optimize(negLogLikFunc,argList)
            }
            
            
            
            return(res)
          }
)






setClass("ModelData", 
         slots = list(
           Name = "character", 
           alpha = "numeric",
           gamma1 = "numeric",
           gamma2 = "numeric",
           lambda = "numeric",
           likelihood = "list",
           probMatrix = "matrix",
           sim = "numeric"
           )
        )

#### func setModelParams ###
setGeneric("setModelParams", function(x,modelParams) 
  standardGeneric("setModelParams") )

setMethod("setModelParams",  signature=c("ModelData","numeric"),
          definition=function(x,modelParams)
          {
            if(x@Name == "aca3Paths"||x@Name == "aca3Turns" )
            {
              x@alpha = modelParams[1]
              x@gamma1 = modelParams[2]
              x@gamma2 = modelParams[3]
            }
            
            return(x)
          }
)

#### func setResults ###
setGeneric("setResults", function(x,baseModel,ratdata) 
  standardGeneric("setResults") )

setMethod("setResults",  signature=c("ModelData","BaseModel","RatData"),
          definition=function(x,baseModel,ratdata)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=2, limit=0.95)
            endLearningStage = endLearningStage/2
            x@probMatrix = baseModel@probMatFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
            likelihood = baseModel@likelihoodFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
            
            
            if(type == "paths")
            {
              
              x@likelihood = sum(likelihood[-(1:endLearningStage)])
            }
            else if(type == "turns")
            {
              if(x@sim==1)
              {
                endLearningStage = last(which(turnTimes[,6]<=endLearningStage))
              }
              else
              {
                endLearningStage = last(which(turnTimes[,1]<=endLearningStage))
              }
              x@likelihood = sum(likelihood[-(1:endLearningStage)])
              
            }
            return(x)
          }
        )

#### func simulateData ###
setGeneric("simulateData", function(x,ratdata) 
  standardGeneric("simulateData") )

setMethod("simulateData",  signature=c("ModelData","RatData"),
          definition=function(x,ratdata)
          {
            endStage1 = getEndIndex(allpaths,sim=2,limit=0.5)
            turnIdxStage1 = last(which(turnTimes[,1]<=endStage1))
            endStage2 = getEndIndex(allpaths,sim=2,limit=0.95)
            turnIdxStage2 = last(which(turnTimes[,1]<=endStage2))
            endStage3 = length(allpaths[,1])
            turnIdxStage3 = length(turnTimes[,1])
            
            pathstages=c(1,endStage1,endStage2,endStage3)
            turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
            
            
            
            if(x@Name == "aca3Paths")
            {
              argList = list(allpaths = ratdata@allpaths, 
                             turnTimes = ratdata@turnTimes,
                             alpha = x@alpha,
                             gamma1 = x@gamma1,
                             gamma2 = x@gamma2,
                             pathstages)
              generated_data = do.call(Aca3Paths@simulateFunc,arglist)
            }
            else if(x@Name == "aca3Turns")
            {
              argList = list(allpaths = ratdata@allpaths, 
                             turnTimes = ratdata@turnTimes,
                             alpha = x@alpha,
                             gamma1 = x@gamma1,
                             gamma2 = x@gamma2,
                             turnstages)
              
              generated_data = do.call(Aca3Turns@simulateFunc,arglist)
            }
            
            
            simData = new("RatData", rat = "simulation",allpaths = generated_data$PathData, turnTimes = generated_data$TurnData)
            return(simData)
          }
)


setClass("AllModels", 
         representation(
           models = "vector",
           aca3Paths = "ModelData",
           aca3Turns = "ModelData",
           type = "character"),
           contains = "ModelData"
         )

setGeneric("addModelDataToResult", function(x,modelName, modelData) 
  standardGeneric("addModelDataToResult") )
setMethod("addModelDataToResult",  signature=c("AllModelResults","character","ModelData"),
          function(x,modelName, modelData)
          {
            if(modelName == "aca3Paths")
            {
              x@aca3Paths = modelData
            }
            else if(modelName == "aca3Turns")
            {
              x@aca3Turns = modelData
            }
            
            return(x)
          }
)

setGeneric("getModelData", function(x,modelName) 
  standardGeneric("getModelData") )
setMethod("getModelData",  signature=c("AllModelResults","character"),
          function(x,modelName)
          {
            if(modelName == "aca3Paths")
            {
              modelData = x@aca3Paths 
            }
            else if(modelName == "aca3Turns")
            {
              modelData = x@aca3Turns
            }
            
            return(modelData)
          }
)





### Models 

Aca3Paths = new("BaseModel", 
                Name = "aca3Paths", 
                simulateFunc = Aca3::simulateTrials, 
                likelihoodFunc = Aca3::getPathLikelihood,
                probMatFunc = Aca3::getProbMatrix,
                rule = "aca3", 
                type = "paths")

Aca3Turns = new("BaseModel", 
                Name = "aca3Turns", 
                simulateFunc = TurnsNew::simulateTurnsModels, 
                likelihoodFunc = TurnsNew::getTurnsLikelihood,
                probMatFunc = TurnsNew::getProbMatrix,
                rule = "aca3", 
                type = "turns")






