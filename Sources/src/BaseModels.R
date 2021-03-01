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
           type="character" ,
           arglist = "list")
         )

#### func setArgList ###
setGeneric("setArgList", function(x,ratdata) 
  standardGeneric("setArgList") )

setMethod("setArgList",  signature=c("BaseModel","RatData"),
          definition=function(x,ratdata)
          {
            endLearningStage = getEndIndex(ratdata@allpaths,sim=2, limit=0.95)
            endLearningStage = endLearningStage/2
            if(modelName == "aca3Paths")
            {
              x@argList = list(lower = c(0,0,0), 
                               upper = c(1,1,1),
                               allpaths = ratdata@allpaths, 
                               half_index = endLearningStage, 
                               model = 5, 
                               sim = x@sim)
            }
            else if(modelName == "aca3Turns")
            {
              x@argList = list(lower = c(0,0,0), 
                               upper = c(1,1,1),
                               allpaths = ratdata@allpaths, 
                               turnTimes = ratdata@turnTimes,
                               half_index = endLearningStage, 
                               model = 5, 
                               sim = x@sim)
            }
            
            return(x)
          }
)



setClass("ModelData", 
         slots = list(
           Name = "character", 
           alpha = "numeric",
           gamma1 = "numeric",
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
            if(modelName == "aca3Paths"||modelName == "aca3Turns" )
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
            if(modelName == "aca")
            {
              x@aca = modelData
            }
            else if(modelName == "gb")
            {
              x@gb = modelData
            }
            else if(modelName == "aca2")
            {
              x@aca2 = modelData
            }
            else if(modelName == "aca3")
            {
              x@aca3 = modelData
            }
            else(modelName == "sarsa")
            {
              x@sarsa = modelData
            }
            return(x)
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






