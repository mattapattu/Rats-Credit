library(Rmpfr)
library(DEoptim)
library(TTR)
library(baseModels)
library(Aca2)
library(Aca3)
library(TurnsModels)
library(Aca3Turns)
library(SarsaTurns)

### Use optimal paramters on actual data and compute Mean Squared Error.
compareTurnModels=function(enreg,rat, window){
  
  enregres = enregCombine(enreg, rat)
  allpaths = enregres$allpaths
  boxTimes = enregres$boxTimes
  allpaths = updateACAPathNbmse(allpaths)
  print(sprintf("rat:%s",rat))
  
  enreg_comb<-matrix(, nrow = 0, ncol = 7)
  for(ses in 1:length(enreg)){
    
    if(is.null(enreg[[ses]])){
      #print(sprintf("skipping %s ses %i as enreg is empty",rat,ses))
      next
    }else if(isempty(enreg[[ses]]$EVENTS)){
      #print(sprintf("skipping %s ses %i as reward data is empty",rat,ses))
      next
    }else if(rat=="rat_106" && ses==3){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_112" && ses==1){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
      
    }else if(rat=="rat_113" && ses==13){
      #print(sprintf("skipping %s ses %i as enreg is not good",rat,ses))
      next
    }
    
    enreg_comb<-rbind(enreg_comb,enreg[[ses]]$POS)
  }
  
  l = cbind(as.numeric(enreg_comb[, 1]),as.numeric(enreg_comb[, 6]),as.numeric(enreg_comb[, 7]) )
  y = baseModels::getPathTimes(as.numeric(allpaths[,2]),l)
  allpaths = cbind(allpaths,y)
  allpaths = cbind(allpaths,c(1:length(allpaths[,1])))
  allpaths_num = matrix(as.numeric(unlist(allpaths[,c(3,5,4,6,2)])),nrow=nrow(allpaths[,c(3,5,4,6,2)]))
  allpaths_num = cbind(allpaths_num,c(1:length(allpaths_num[,1])))
  
  # empprob2 = baseModels::empiricalProbMat2(allpaths_num,window)
  # endLearningStage = getEndIndex(allpaths_num,sim=2)
  # #debug(generateEmpiricalPlots)
  # generateEmpiricalPlots(rat, empprob2,endLearningStage)
  
  
  
  # #### Holdout Validation ########################################
  # endLearningStage = getEndIndex(allpaths_num,sim=2)
  # Hinit = genInitValues(allpaths_num,sim=2)
  # #Hinit <- matrix(0,2,6)
  # 
  # Models = list("ACA" = 1, "GB" = 2, "GB-ACA" = 3, "ACA2" = 4, "ACA3" = 5, "SARSA"=6)
  # models = c(6,1,2,5)
  # #debug(validateHoldout)
  # #mat_res = validateHoldout(models,Hinit=matrix(0,2,6),endLearningStage,allpaths_num, window = window, rat)
  # 
  # # ##### Model Selection On Acutal Data #########################3
  # 
  generated_data <- allpaths_num
  #mat_res = windowCompare(generated_data,models, sim=2)
  #debug(getModelData)
  
  models = c(1,2,5,6)
  turnTimes = TurnsModels::getTurnTimes(allpaths,boxTimes,sim=2)
  colnames(turnTimes) = c("Idx", "Path","State","TurnIdx","SessionNb","TurnTime1","TurnTime2","TurnTime3")
  
  debug(simTurns)
  simTurns(allpaths_num,turnTimes,0.1,model=1,turnMethod = 1)
  
  #TurnsModels::getProbMatrix(allpaths_num,turnTimes,turnMethod = 0,alpha=0.1,sim=2,model=1)
  #debug(getTurnModelData)
  res = getTurnModelData(generated_data, turnTimes, models, window = window, sim=2)

  min_index = 0
  min = 100000
  for(m in models){
    print(sprintf("%s: %s likelihood = %f", rat, res[[m]]@Name, res[[m]]@Metrics$likelihood))
    #print(res[[m]]@Params_lik)


    if(res[[m]]@Metrics$likelihood < min){
      min = res[[m]]@Metrics$likelihood
      min_index = m
    }
  }
  print(sprintf("%s is best fit for %s", res[[min_index]]@Name, rat))


  save(res, file = paste0(rat,"_res.Rdata"))

  #ranges = getTestRange(generated_data)
  #debug(plotData)
  #plotData2(res,rat,ranges)
  #empProbMat = baseModels::empiricalProbMat(generated_data, window = window)
  #debug(generateTurnPlots)
  generateTurnPlots(rat,window, res$acaTurnData@ProbMatrix, res$gbTurnData@ProbMatrix, res$aca3TurnData@ProbMatrix, res$sarsaTurnData@ProbMatrix, turnTimes)
}

simTurns=function(allpaths_num,turnTimes,alpha,model,turnMethod)
{
  test = TurnsModels::simulateTurnsModels(allpaths_num,turnTimes,alpha,model,turnMethod)
}